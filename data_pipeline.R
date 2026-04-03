# NFL Draft Quasi-Sharpe Ratio — Data Pipeline
# Pulls nflreadr data, joins draft picks with snap counts and contracts,
# classifies hits, computes Sharpe ratio components, exports parquet.

library(nflreadr)
library(tidyverse)
library(arrow)

cat("Draft Sharpe data pipeline\n")
cat("=========================\n\n")

# --- Helpers ----------------------------------------------------------------

# Normalize names for matching: lowercase, strip suffixes (Jr., III, etc.),
# collapse whitespace, trim. Handles PFR vs OTC naming differences.
normalize_name <- function(x) {
  x |>
    str_to_lower() |>
    str_trim() |>
    str_remove_all("\\.") |>
    str_remove_all(",") |>
    str_remove_all("\\b(jr|sr|ii|iii|iv|v)$") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

# Nickname aliases: PFR name → OTC name (normalized)
# Used to match players whose PFR name differs from their OTC legal name
NICKNAME_ALIASES <- tibble::tribble(
  ~pfr_norm,                ~otc_norm,
  "sauce gardner",          "ahmad gardner",
  "jackrabbit jenkins",     "janoris jenkins",
  "ha ha clinton-dix",      "ha-ha clinton-dix",
  "shaq mason",             "shaquille mason",
  "joe barksdale",          "joseph barksdale"
)

# Apply nickname aliases: replace PFR normalized names with OTC equivalents
apply_aliases <- function(name_norm) {
  idx <- match(name_norm, NICKNAME_ALIASES$pfr_norm)
  if_else(!is.na(idx), NICKNAME_ALIASES$otc_norm[idx], name_norm)
}

# --- Constants ---------------------------------------------------------------

POSITION_MAP <- c(
  "QB" = "QB",
  "RB" = "RB", "FB" = "RB",
  "WR" = "WR",
  "TE" = "TE",
  "T" = "OT", "OT" = "OT", "LT" = "OT", "RT" = "OT",
  "G" = "IOL", "C" = "IOL", "OL" = "IOL", "OG" = "IOL", "LG" = "IOL", "RG" = "IOL",
  "DE" = "EDGE", "OLB" = "EDGE", "EDGE" = "EDGE", "ED" = "EDGE",
  "DT" = "DL", "NT" = "DL", "DL" = "DL", "IDL" = "DL",
  "LB" = "LB", "ILB" = "LB", "MLB" = "LB",
  "CB" = "CB", "DB" = "CB",
  "S" = "S", "SS" = "S", "FS" = "S"
)

TIER_BREAKS <- c(0, 10, 32, 64, 100, Inf)
TIER_LABELS <- c("Top 10", "Late 1st", "Day 2", "Day 3 Early", "Day 3 Late")

# Hit thresholds: 75% of positional baseline snap % (tightened from Riske's
# 2/3 to better align with elite-probability Sharpe formulation)
HIT_THRESHOLDS <- tibble::tibble(
  pos_group = c("IOL", "OT", "S", "LB", "CB", "WR", "QB", "EDGE", "DL", "TE", "RB"),
  hit_threshold = c(0.747, 0.730, 0.714, 0.692, 0.692, 0.640, 0.622, 0.609, 0.538, 0.534, 0.423)
)

ROOKIE_DEAL_YEARS <- 4  # First 4 seasons for snap evaluation
MAX_DRAFT_SEASON <- 2022  # Only include players with at least 4 years in league


# --- 1. Draft Picks ----------------------------------------------------------

cat("Loading draft picks...\n")
draft_raw <- load_draft_picks(seasons = TRUE)

draft <- draft_raw |>
  filter(!is.na(position)) |>
  mutate(
    pos_group = POSITION_MAP[position],
    tier = cut(pick, breaks = TIER_BREAKS, labels = TIER_LABELS, right = TRUE),
    team = case_match(team,
      "STL" ~ "LA", "SL" ~ "LA", "LAR" ~ "LA",
      "OAK" ~ "LV", "LVR" ~ "LV",
      "SDG" ~ "LAC", "SD" ~ "LAC",
      .default = team
    )
  ) |>
  filter(!is.na(pos_group)) |>
  select(
    season, round, pick, team, pfr_player_id, gsis_id,
    pfr_player_name, position, pos_group, tier,
    games, seasons_started, allpro, probowls, w_av, car_av
  )

cat(sprintf("  %d draft picks loaded (%d-%d)\n", nrow(draft), min(draft$season), max(draft$season)))
cat(sprintf("  Position groups: %s\n", paste(sort(unique(draft$pos_group)), collapse = ", ")))

# --- 2. Snap Counts (first 4 seasons) ----------------------------------------

cat("Loading snap counts (2012+)...\n")
snaps_raw <- load_snap_counts(seasons = TRUE)

cat(sprintf("  Snap count seasons available: %d-%d\n",
            min(snaps_raw$season), max(snaps_raw$season)))

snap_min_season <- min(snaps_raw$season)

draft_with_snap_window <- draft |>
  filter(season >= snap_min_season - ROOKIE_DEAL_YEARS + 1) |>
  mutate(
    snap_start = season,
    snap_end = season + ROOKIE_DEAL_YEARS - 1
  )

offensive_groups <- c("QB", "RB", "WR", "TE", "OT", "IOL")
defensive_groups <- c("EDGE", "DL", "LB", "CB", "S")

# Count total regular-season games per season for denominator
games_per_season <- snaps_raw |>
  filter(game_type == "REG" | is.na(game_type)) |>
  group_by(season) |>
  summarise(max_games = max(week, na.rm = TRUE), .groups = "drop")

snaps_agg <- snaps_raw |>
  select(season, week, pfr_player_id, offense_pct, defense_pct) |>
  inner_join(
    draft_with_snap_window |> select(pfr_player_id, snap_start, snap_end, pos_group),
    by = "pfr_player_id"
  ) |>
  filter(season >= snap_start, season <= snap_end) |>
  mutate(
    snap_pct = if_else(pos_group %in% offensive_groups, offense_pct, defense_pct)
  ) |>
  group_by(pfr_player_id) |>
  summarise(
    games_played = n(),
    seasons_with_snaps = n_distinct(season),
    sum_snap_pct = sum(snap_pct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Join back to get the 4-season window and compute expected games
  inner_join(
    draft_with_snap_window |> select(pfr_player_id, snap_start, snap_end),
    by = "pfr_player_id"
  ) |>
  # Total expected games across the 4-season window
  mutate(
    expected_games = map2_dbl(snap_start, snap_end, function(s, e) {
      gs <- games_per_season |> filter(season >= s, season <= e)
      sum(gs$max_games)
    }),
    # Average snap % denominated by ALL expected games, not just games played
    avg_snap_pct = sum_snap_pct / expected_games
  ) |>
  select(pfr_player_id, avg_snap_pct, games_played, seasons_with_snaps)

cat(sprintf("  Aggregated snap data for %d players\n", nrow(snaps_agg)))

# --- 3. Hit Classification ----------------------------------------------------

cat("Classifying hits...\n")

draft_snaps <- draft_with_snap_window |>
  left_join(snaps_agg, by = "pfr_player_id") |>
  left_join(HIT_THRESHOLDS, by = "pos_group") |>
  mutate(
    avg_snap_pct = replace_na(avg_snap_pct, 0),
    is_hit = avg_snap_pct >= hit_threshold
  )

hit_summary <- draft_snaps |>
  group_by(pos_group) |>
  summarise(
    n = n(),
    hits = sum(is_hit),
    hit_rate = mean(is_hit),
    .groups = "drop"
  ) |>
  arrange(desc(hit_rate))

cat("\n  Overall hit rates by position:\n")
print(hit_summary, n = Inf)

# --- 4. Contracts -------------------------------------------------------------

cat("Loading contracts...\n")
contracts_raw <- load_contracts()

contracts <- contracts_raw |>
  mutate(pos_group = POSITION_MAP[position]) |>
  filter(!is.na(pos_group), !is.na(apy_cap_pct))

cat(sprintf("  %d contracts loaded\n", nrow(contracts)))

# --- 4b. Position Coercion (CB→S, IOL↔OT only) -------------------------------
# PFR maps all DBs to CB (many are safeties) and IOL/OT can swap between
# systems. When OTC has a meaningful contract at the corrected position,
# coerce the draft player. EDGE/DL/LB are left as-is — draft position stands.

cat("Coercing draft positions (CB→S, IOL↔OT) where OTC disagrees...\n")

COERCION_ALLOWED <- list(
  c("CB", "S"),
  c("IOL", "OT"), c("OT", "IOL"),
  c("LB", "EDGE")  # OLB pass rushers drafted as LB → EDGE
)

pos_overrides <- draft_snaps |>
  select(pfr_player_id, pfr_player_name, season, draft_pos = pos_group) |>
  mutate(name_norm = apply_aliases(normalize_name(pfr_player_name))) |>
  inner_join(
    contracts |>
      filter(apy_cap_pct > 0.02) |>
      select(player, year_signed, apy_cap_pct, contract_pos = pos_group) |>
      mutate(name_norm = normalize_name(player)),
    by = "name_norm", relationship = "many-to-many"
  ) |>
  filter(
    draft_pos != contract_pos,
    year_signed >= season, year_signed <= season + ROOKIE_DEAL_YEARS + 2,
    pmap_lgl(list(draft_pos, contract_pos), function(d, c) {
      any(map_lgl(COERCION_ALLOWED, ~ all(c(d, c) %in% .x)))
    })
  ) |>
  group_by(pfr_player_id) |>
  slice_max(apy_cap_pct, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(pfr_player_id, coerced_pos = contract_pos)

cat(sprintf("  %d players coerced to OTC position\n", nrow(pos_overrides)))

# Apply overrides and re-classify hits
draft_snaps <- draft_snaps |>
  left_join(pos_overrides, by = "pfr_player_id") |>
  mutate(pos_group = coalesce(coerced_pos, pos_group)) |>
  select(-coerced_pos, -hit_threshold, -is_hit) |>
  left_join(HIT_THRESHOLDS, by = "pos_group") |>
  mutate(is_hit = avg_snap_pct >= hit_threshold)

hit_summary_coerced <- draft_snaps |>
  group_by(pos_group) |>
  summarise(n = n(), hits = sum(is_hit), hit_rate = mean(is_hit), .groups = "drop") |>
  arrange(desc(hit_rate))

cat("  Hit rates after position coercion:\n")
print(hit_summary_coerced, n = Inf)

# --- 5. Second Contracts (post-rookie deal) -----------------------------------

cat("Matching rookie contracts to drafted players...\n")

rookie_contracts <- draft_snaps |>
  select(pfr_player_id, pfr_player_name, season, pos_group) |>
  mutate(name_norm = apply_aliases(normalize_name(pfr_player_name))) |>
  inner_join(
    contracts |> select(player, year_signed, apy_cap_pct, pos_group) |>
      mutate(name_norm = normalize_name(player)),
    by = c("name_norm", "pos_group"),
    relationship = "many-to-many"
  ) |>
  filter(
    year_signed >= season, year_signed <= season + 1
  ) |>
  group_by(pfr_player_id) |>
  slice_max(apy_cap_pct, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(pfr_player_id, rookie_apy_cap_pct = apy_cap_pct)

cat(sprintf("  Matched %d rookie contracts\n", nrow(rookie_contracts)))

cat("Matching second contracts to drafted players...\n")

second_contracts <- draft_snaps |>
  select(pfr_player_id, pfr_player_name, season, pos_group) |>
  mutate(name_norm = apply_aliases(normalize_name(pfr_player_name))) |>
  inner_join(
    contracts |> select(player, otc_id, year_signed, apy_cap_pct, apy, value, years, pos_group) |>
      mutate(name_norm = normalize_name(player)),
    by = c("name_norm", "pos_group"),
    relationship = "many-to-many"
  ) |>
  filter(
    year_signed >= season + 2,
    year_signed <= season + ROOKIE_DEAL_YEARS + 2
  ) |>
  group_by(pfr_player_id) |>
  slice_max(apy_cap_pct, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(pfr_player_id, second_apy_cap_pct = apy_cap_pct,
         second_contract_year = year_signed, second_contract_apy = apy,
         second_contract_value = value, second_contract_years = years)

cat(sprintf("  Matched %d second contracts\n", nrow(second_contracts)))

# --- 6. Free Agency Replacement Cost ------------------------------------------

# FA replacement cost based on actual market supply of starter-caliber FAs.
# "Starter-caliber" = contract at or above the 25th percentile of what drafted
# hits earn on their second deal (consistent with our hit classification).
# Top-N per position = median number of such contracts signed per year.
# FA replacement = median of the top-N contracts per position per year.

cat("Computing FA replacement cost (hit-based supply)...\n")

hit_contract_thresholds <- draft_snaps |>
  filter(is_hit == TRUE) |>
  mutate(name_norm = apply_aliases(normalize_name(pfr_player_name))) |>
  inner_join(
    contracts |> select(player, year_signed, apy_cap_pct, pos_group) |>
      mutate(name_norm = normalize_name(player)),
    by = c("name_norm", "pos_group"), relationship = "many-to-many"
  ) |>
  filter(
    year_signed >= season + 2, year_signed <= season + ROOKIE_DEAL_YEARS + 2
  ) |>
  group_by(pfr_player_id) |>
  slice_max(apy_cap_pct, n = 1, with_ties = FALSE) |>
  ungroup() |>
  group_by(pos_group) |>
  summarise(starter_threshold = quantile(apy_cap_pct, 0.25), .groups = "drop")

cat("  Starter contract thresholds (25th pctl of hit second contracts):\n")
print(hit_contract_thresholds, n = Inf)

fa_supply <- hit_contract_thresholds |>
  pmap_dfr(function(pos_group, starter_threshold) {
    contracts |>
      filter(pos_group == !!pos_group, apy_cap_pct >= starter_threshold) |>
      group_by(year_signed) |>
      summarise(n = n(), .groups = "drop") |>
      summarise(
        pos_group = !!pos_group,
        fa_top_n = as.integer(median(n))
      )
  })

cat("  FA supply (starter-caliber contracts per year):\n")
print(fa_supply, n = Inf)

fa_replacement <- fa_supply |>
  pmap_dfr(function(pos_group, fa_top_n) {
    contracts |>
      filter(pos_group == !!pos_group, apy_cap_pct > 0.001) |>
      group_by(year_signed) |>
      slice_max(apy_cap_pct, n = fa_top_n, with_ties = FALSE) |>
      ungroup() |>
      summarise(
        pos_group = !!pos_group,
        fa_median_apy_cap_pct = median(apy_cap_pct, na.rm = TRUE),
        fa_mean_apy_cap_pct = mean(apy_cap_pct, na.rm = TRUE),
        fa_n = n(),
        fa_top_n = fa_top_n
      )
  })

cat("\n  FA replacement cost (hit-based supply) by position:\n")
print(fa_replacement, n = Inf)

# --- 7. Final Join ------------------------------------------------------------

cat("Building final analysis dataset...\n")

analysis_df <- draft_snaps |>
  left_join(rookie_contracts, by = "pfr_player_id") |>
  left_join(second_contracts, by = "pfr_player_id") |>
  left_join(fa_replacement, by = "pos_group") |>
  mutate(
    rookie_apy_cap_pct = replace_na(rookie_apy_cap_pct, 0),
    second_apy_cap_pct = replace_na(second_apy_cap_pct, 0)
  )

cat(sprintf("  Final dataset: %d players\n", nrow(analysis_df)))

# --- 8. Sharpe Ratio Computation ----------------------------------------------

cat("Computing Sharpe ratios...\n")

# Compute snap-weighted return for each player (used for elite threshold)
# Return = snap_ratio × second_contract + rookie_surplus
# Rookie surplus = snap_ratio × (FA_replacement - rookie_contract) × rookie_deal_years
# i.e. actual cap savings from getting starter production at rookie salary

analysis_df <- analysis_df |>
  mutate(
    snap_ratio = avg_snap_pct / hit_threshold,
    rookie_surplus = snap_ratio * pmax(fa_median_apy_cap_pct - rookie_apy_cap_pct, 0) * ROOKIE_DEAL_YEARS,
    player_return = second_apy_cap_pct + rookie_surplus
  )

# Filter to players with at least 4 years in league for fair comparison
sharpe_df <- analysis_df |> filter(season <= MAX_DRAFT_SEASON)
cat(sprintf("  Sharpe eligible: %d players (drafted <= %d)\n", nrow(sharpe_df), MAX_DRAFT_SEASON))

elite_threshold <- quantile(
  sharpe_df$player_return[sharpe_df$player_return > 0],
  probs = 0.90, na.rm = TRUE
)
cat(sprintf("  Elite threshold (90th pctl of player return): %.4f\n", elite_threshold))

sharpe_ratios <- sharpe_df |>
  group_by(pos_group, tier) |>
  summarise(
    n = n(),
    hit_rate = mean(is_hit),
    mean_player_return = mean(player_return, na.rm = TRUE),
    mean_second_contract = mean(second_apy_cap_pct, na.rm = TRUE),
    sd_player_return = sd(player_return, na.rm = TRUE),
    sd_second_contract = sd(second_apy_cap_pct, na.rm = TRUE),
    elite_prob = mean(player_return >= elite_threshold, na.rm = TRUE),
    fa_replacement = first(fa_median_apy_cap_pct),
    .groups = "drop"
  ) |>
  mutate(
    sharpe_linear = (mean_second_contract - fa_replacement) / sd_second_contract,
    sharpe_elite = (elite_prob * elite_threshold - fa_replacement) / sd_player_return
  ) |>
  mutate(
    across(starts_with("sharpe_"), ~ if_else(is.finite(.x), .x, NA_real_))
  )

cat("\n  Sharpe ratios (linear):\n")
sharpe_ratios |>
  select(pos_group, tier, n, hit_rate, sharpe_linear, sharpe_elite) |>
  arrange(pos_group, tier) |>
  print(n = Inf)

# --- 9. Export ----------------------------------------------------------------

cat("\nExporting parquet...\n")
write_parquet(analysis_df, "data/draft_sharpe_analysis.parquet")

dir.create("output", showWarnings = FALSE)
write_csv(sharpe_ratios, "output/sharpe_ratios.csv")

hit_rates_export <- analysis_df |>
  group_by(pos_group, tier) |>
  summarise(n = n(), hits = sum(is_hit), hit_rate = mean(is_hit), .groups = "drop")
write_csv(hit_rates_export, "output/hit_rates.csv")

write_csv(fa_replacement, "output/fa_replacement.csv")

# --- 10. Player-Level Sharpe --------------------------------------------------

cat("Computing player-level Sharpe ratios...\n")

# Player Sharpe = (player_return - FA replacement) / bucket volatility
# player_return = (snap_pct / positional_baseline) * second_contract_cap_pct
# This rewards both playing time (Riske) and contract quality (Brill)

player_lookup <- sharpe_df |>
  left_join(
    sharpe_ratios |> select(pos_group, tier, sd_player_return, sd_second_contract, sharpe_linear, sharpe_elite),
    by = c("pos_group", "tier")
  ) |>
  mutate(
    # snap_ratio and player_return already computed in section 8
    player_sharpe = (player_return - fa_median_apy_cap_pct) / sd_player_return,
    player_sharpe = if_else(is.finite(player_sharpe), player_sharpe, NA_real_)
  ) |>
  select(pfr_player_name, pos_group, team, gsis_id, season, pick, tier,
         is_hit, avg_snap_pct, snap_ratio, rookie_apy_cap_pct,
         fa_median_apy_cap_pct, rookie_surplus, second_apy_cap_pct,
         player_return, player_sharpe, sharpe_linear, sharpe_elite) |>
  arrange(desc(player_sharpe))

cat(sprintf("  Top 5 player Sharpe ratios:\n"))
player_lookup |> head(5) |> select(pfr_player_name, pos_group, pick, player_sharpe) |> print()

write_csv(player_lookup, "output/player_lookup.csv")

cat("Done! Outputs:\n")
cat("  data/draft_sharpe_analysis.parquet\n")
cat("  output/sharpe_ratios.csv\n")
cat("  output/hit_rates.csv\n")
cat("  output/fa_replacement.csv\n")
cat("  output/player_lookup.csv\n")
