# NFL Draft Sharpe — Counter-Article Data Pipeline (V2)
# Extends Article 1 with: roster needs, class depth, blue-chip, market efficiency

library(nflreadr)
library(tidyverse)
library(arrow)

cat("Counter-Article Data Pipeline (V2)\n")
cat("===================================\n\n")

# --- Shared Constants (from Article 1) ----------------------------------------

POSITION_MAP <- c(
  "QB" = "QB",
  "RB" = "RB", "FB" = "RB", "HB" = "RB",
  "WR" = "WR",
  "TE" = "TE",
  "T" = "OT", "OT" = "OT", "LT" = "OT", "RT" = "OT",
  "G" = "IOL", "C" = "IOL", "OL" = "IOL", "OG" = "IOL", "LG" = "IOL", "RG" = "IOL",
  "DE" = "EDGE", "OLB" = "EDGE", "EDGE" = "EDGE", "ED" = "EDGE",
  "DT" = "DL", "NT" = "DL", "DL" = "DL", "IDL" = "DL",
  "LB" = "LB", "ILB" = "LB", "MLB" = "LB",
  "CB" = "CB", "DB" = "CB",
  "S" = "S", "SS" = "S", "FS" = "S", "SAF" = "S"
)

HIT_THRESHOLDS <- tibble::tibble(
  pos_group = c("IOL", "OT", "S", "LB", "CB", "WR", "QB", "EDGE", "DL", "TE", "RB"),
  hit_threshold = c(0.747, 0.730, 0.714, 0.692, 0.692, 0.640, 0.622, 0.609, 0.538, 0.534, 0.423)
)

TIER_BREAKS <- c(0, 10, 32, 64, 100, Inf)
TIER_LABELS <- c("Top 10", "Late 1st", "Round 2", "Round 3", "Rounds 4-7")

MAX_DRAFT_SEASON <- 2022

offensive_groups <- c("QB", "RB", "WR", "TE", "OT", "IOL")
defensive_groups <- c("EDGE", "DL", "LB", "CB", "S")

# --- Helpers (from Article 1) -------------------------------------------------

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

# --- Load Article 1 Outputs --------------------------------------------------

cat("Loading Article 1 outputs...\n")
df <- read_parquet("data/draft_sharpe_analysis.parquet")
sharpe <- read_csv("output/sharpe_ratios.csv", show_col_types = FALSE)
player_lookup <- read_csv("output/player_lookup.csv", show_col_types = FALSE)
fa_replacement <- read_csv("output/fa_replacement.csv", show_col_types = FALSE)

df_eligible <- df |> filter(season <= MAX_DRAFT_SEASON)

cat(sprintf("  Loaded %d players (%d Sharpe-eligible)\n", nrow(df), nrow(df_eligible)))

dir.create("output/v2", showWarnings = FALSE, recursive = TRUE)

# --- Load Raw Draft Data (needed for car_av) ----------------------------------

cat("Loading raw draft data...\n")
draft_raw <- load_draft_picks(seasons = TRUE)

draft_all <- draft_raw |>
  filter(!is.na(position)) |>
  mutate(
    pos_group = POSITION_MAP[position],
    tier = cut(pick, breaks = TIER_BREAKS, labels = TIER_LABELS, right = TRUE)
  ) |>
  filter(!is.na(pos_group))

cat(sprintf("  %d total draft picks loaded\n", nrow(draft_all)))

# --- Load Contracts -----------------------------------------------------------

cat("Loading contracts...\n")
contracts_raw <- load_contracts()
contracts <- contracts_raw |>
  mutate(pos_group = POSITION_MAP[position]) |>
  filter(!is.na(pos_group), !is.na(apy_cap_pct))

cat(sprintf("  %d contracts loaded\n", nrow(contracts)))

# --- Load Snap Counts ---------------------------------------------------------

cat("Loading snap counts...\n")
snaps_raw <- load_snap_counts(seasons = TRUE)
cat(sprintf("  Snap counts: %d-%d\n", min(snaps_raw$season), max(snaps_raw$season)))

# --- Derive car_av from w_av (nflreadr's car_av column is empty) --------------
draft_all <- draft_all |> mutate(car_av = w_av)

# --- 1. Draft Class Depth ----------------------------------------------------

cat("Computing draft class depth...\n")

class_depth <- df_eligible |>
  group_by(season, pos_group) |>
  summarise(
    n_drafted = n(),
    n_hits = sum(is_hit),
    hit_rate = mean(is_hit),
    mean_player_return = mean(player_return, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    draft_all |>
      filter(season <= MAX_DRAFT_SEASON) |>
      group_by(season, pos_group) |>
      summarise(
        mean_car_av = mean(car_av, na.rm = TRUE),
        max_car_av = max(car_av, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("season", "pos_group")
  ) |>
  group_by(pos_group) |>
  mutate(
    median_hit_rate = median(hit_rate, na.rm = TRUE),
    is_deep = hit_rate > median_hit_rate,
    class_quality = if_else(is_deep, "Deep", "Thin")
  ) |>
  ungroup()

cat(sprintf("  Class depth computed: %d season-position combos\n", nrow(class_depth)))

write_csv(class_depth, "output/v2/class_depth.csv")
cat("  Saved output/v2/class_depth.csv\n")

# --- 2. Blue-Chip Classification ----------------------------------------------

cat("Classifying blue-chip prospects...\n")

blue_chip <- draft_all |>
  filter(season <= MAX_DRAFT_SEASON, !is.na(car_av)) |>
  group_by(pos_group) |>
  mutate(
    car_av_pctl = percent_rank(car_av),
    is_blue_chip = car_av_pctl >= 0.90
  ) |>
  ungroup() |>
  select(season, round, pick, pfr_player_name, pfr_player_id, pos_group, tier,
         car_av, car_av_pctl, is_blue_chip) |>
  left_join(
    player_lookup |> select(pfr_player_name, season, pos_group, player_return,
                            player_sharpe, is_hit, avg_snap_pct),
    by = c("pfr_player_name", "season", "pos_group")
  ) |>
  left_join(
    class_depth |> select(season, pos_group, class_quality, hit_rate),
    by = c("season", "pos_group")
  ) |>
  rename(class_hit_rate = hit_rate)

cat(sprintf("  %d players classified, %d blue chips\n",
            nrow(blue_chip), sum(blue_chip$is_blue_chip)))

bc_summary <- blue_chip |>
  filter(is_blue_chip) |>
  group_by(pos_group) |>
  summarise(
    n = n(),
    mean_pick = mean(pick),
    mean_car_av = mean(car_av),
    hit_rate = mean(is_hit, na.rm = TRUE),
    mean_return = mean(player_return, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(mean_return))

cat("  Blue chip summary by position:\n")
print(bc_summary, n = Inf)

write_csv(blue_chip, "output/v2/blue_chip.csv")
cat("  Saved output/v2/blue_chip.csv\n")

# --- 3. Roster Needs -----------------------------------------------------------

cat("\nComputing roster needs...\n")

# Team abbreviation mapping: PFR/draft style -> nflreadr snap style
# We'll normalize snap team abbreviations to match draft conventions
snap_to_draft_team <- c(

  "GB"  = "GNB",
  "KC"  = "KAN",
  "LA"  = "LAR",
  "LV"  = "LVR",
  "NE"  = "NWE",
  "NO"  = "NOR",
  "SF"  = "SFO",
  "TB"  = "TAM",
  "OAK" = "OAK"
)

# Compute per-player season avg snap % from snap counts, for drafted players only
# Join to draft_all to get pos_group and team
player_season_snaps <- snaps_raw |>
  filter(game_type == "REG") |>
  mutate(
    snap_pct = case_when(
      !is.na(offense_pct) & offense_pct > 0 ~ offense_pct,
      !is.na(defense_pct) & defense_pct > 0 ~ defense_pct,
      TRUE ~ 0
    ),
    # Normalize team abbrev to draft convention
    team_draft = if_else(
      team %in% names(snap_to_draft_team),
      snap_to_draft_team[team],
      team
    )
  ) |>
  group_by(season, pfr_player_id, team_draft) |>
  summarise(
    avg_snap_pct = mean(snap_pct, na.rm = TRUE),
    games = n(),
    .groups = "drop"
  ) |>
  filter(!is.na(pfr_player_id), pfr_player_id != "") |>
  # Join to draft_all for pos_group

  inner_join(
    draft_all |> select(pfr_player_id, pos_group) |> distinct(),
    by = "pfr_player_id"
  )

cat(sprintf("  Player-season snap records: %d\n", nrow(player_season_snaps)))

# For each team-season-pos_group, find the best starter (highest avg snap %)
best_starters <- player_season_snaps |>
  group_by(team_draft, season, pos_group) |>
  slice_max(avg_snap_pct, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(team = team_draft)

# Check if the best starter meets the hit threshold
best_starters <- best_starters |>
  left_join(HIT_THRESHOLDS, by = "pos_group") |>
  mutate(starter_is_hit = avg_snap_pct >= hit_threshold)

# Quality gate: starter must have earned a second contract above FA replacement
# This uses the market's verdict — if a player got paid like a starter, they were one.
# A bridge deal or minimum contract doesn't count. This fixes cases like Tyrod Taylor
# (played a lot in Buffalo but was never the long-term answer — no big extension).
player_second_contracts <- df_eligible |>
  filter(second_apy_cap_pct > 0) |>
  select(pfr_player_id, second_apy_cap_pct) |>
  distinct()

best_starters <- best_starters |>
  left_join(player_second_contracts, by = "pfr_player_id") |>
  left_join(fa_replacement |> select(pos_group, fa_median_apy_cap_pct), by = "pos_group") |>
  mutate(
    # Quality = earned a second contract above FA replacement for their position
    starter_is_quality = !is.na(second_apy_cap_pct) &
                         second_apy_cap_pct >= fa_median_apy_cap_pct,
    starter_is_good = starter_is_hit & starter_is_quality
  )

# Normalize contract names for matching
contracts_norm <- contracts |>
  mutate(player_norm = normalize_name(player)) |>
  select(player_norm, pos_group, year_signed, years, apy_cap_pct)

# Get player names from draft_all for matching to contracts
draft_names <- draft_all |>
  select(pfr_player_id, pfr_player_name) |>
  distinct() |>
  mutate(player_norm = normalize_name(pfr_player_name))

# For each best starter, check if their contract is expiring
# Contract is expiring if year_signed + years - 1 <= prior_season (i.e., the season we're checking)
best_starters_with_contracts <- best_starters |>
  left_join(draft_names |> select(pfr_player_id, player_norm), by = "pfr_player_id") |>
  left_join(
    contracts_norm,
    by = c("player_norm", "pos_group"),
    relationship = "many-to-many"
  ) |>
  # Keep only contracts that were active during this season
  filter(is.na(year_signed) | (year_signed <= season & year_signed + years - 1 >= season)) |>
  # If multiple contracts match, keep the most recent

  group_by(pfr_player_id, season, pos_group) |>
  slice_max(year_signed, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    contract_expiring = case_when(
      is.na(year_signed) ~ TRUE,  # No contract found → assume expiring
      year_signed + years - 1 <= season ~ TRUE,  # Contract ends this season or earlier
      TRUE ~ FALSE
    )
  )

# Build roster needs: for each draft year, check the PRIOR season's starter
# We need needs for draft seasons (the pick addresses a need for the upcoming season)
# So for a pick in draft year Y, check season Y-1
draft_seasons <- tibble(draft_season = sort(unique(
  draft_all$season[draft_all$season >= 2014 & draft_all$season <= MAX_DRAFT_SEASON]
)))

all_pos_groups <- unique(draft_all$pos_group)
all_teams <- unique(draft_all$team[draft_all$season >= 2014 & draft_all$season <= MAX_DRAFT_SEASON])

# Create all team × draft_season × pos_group combinations
roster_grid <- expand_grid(
  team = all_teams,
  draft_season = draft_seasons$draft_season,
  pos_group = all_pos_groups
)

# For each combo, look up the prior season's best starter
roster_needs <- roster_grid |>
  mutate(prior_season = draft_season - 1) |>
  left_join(
    best_starters_with_contracts |>
      select(team, season, pos_group, pfr_player_id, avg_snap_pct,
             starter_is_hit, starter_is_good, contract_expiring),
    by = c("team", "prior_season" = "season", "pos_group")
  ) |>
  mutate(
    is_filled = !is.na(starter_is_good) & starter_is_good & !contract_expiring,
    is_need = !is_filled
  ) |>
  select(team, draft_season, pos_group, pfr_player_id, avg_snap_pct,
         starter_is_hit, starter_is_good, contract_expiring, is_filled, is_need)

cat(sprintf("  Roster needs computed: %d team-season-position combos\n", nrow(roster_needs)))
cat(sprintf("  Filled: %d (%.1f%%)  Needs: %d (%.1f%%)\n",
            sum(roster_needs$is_filled), mean(roster_needs$is_filled) * 100,
            sum(roster_needs$is_need), mean(roster_needs$is_need) * 100))

write_csv(roster_needs, "output/v2/roster_needs.csv")
cat("  Saved output/v2/roster_needs.csv\n")

# Spot-check: Bills QB needs by year
cat("\n  Spot-check: Bills QB needs by year:\n")
bills_qb <- roster_needs |>
  filter(team == "BUF", pos_group == "QB") |>
  arrange(draft_season)
print(bills_qb, n = Inf)

# --- 4. Need-Adjusted Team Efficiency ------------------------------------------

cat("\nComputing need-adjusted team efficiency...\n")

# Get R1 picks (Top 10 + Late 1st) with their sharpe_elite values
r1_picks <- df_eligible |>
  filter(tier %in% c("Top 10", "Late 1st"),
         season >= 2014, season <= MAX_DRAFT_SEASON) |>
  select(pfr_player_id, pfr_player_name, team, season, pos_group, tier,
         is_hit, player_return, avg_snap_pct) |>
  left_join(
    sharpe |> select(pos_group, tier, sharpe_elite),
    by = c("pos_group", "tier")
  )

cat(sprintf("  R1 picks with sharpe: %d\n", nrow(r1_picks)))

# Join roster needs
r1_with_needs <- r1_picks |>
  left_join(
    roster_needs |> select(team, draft_season, pos_group, is_filled, is_need),
    by = c("team", "season" = "draft_season", "pos_group")
  ) |>
  mutate(
    # Default to need if no roster data available
    is_need = if_else(is.na(is_need), TRUE, is_need),
    is_filled = if_else(is.na(is_filled), FALSE, is_filled),
    # Adjusted scoring
    need_bonus = case_when(
      is_need & sharpe_elite < 0 ~ abs(sharpe_elite) * 0.5,
      TRUE ~ 0
    ),
    filled_penalty = case_when(
      is_filled & sharpe_elite > 0 ~ -sharpe_elite * 0.5,
      TRUE ~ 0
    ),
    sharpe_adjusted = sharpe_elite + need_bonus + filled_penalty
  )

# Team efficiency: average sharpe per team (2018-2022, min 2 picks)
team_eff_original <- r1_with_needs |>
  filter(season >= 2018, season <= MAX_DRAFT_SEASON) |>
  group_by(team) |>
  summarise(
    n_picks = n(),
    avg_sharpe_original = mean(sharpe_elite, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n_picks >= 2)

team_eff_adjusted <- r1_with_needs |>
  filter(season >= 2018, season <= MAX_DRAFT_SEASON) |>
  group_by(team) |>
  summarise(
    n_picks = n(),
    avg_sharpe_original = mean(sharpe_elite, na.rm = TRUE),
    avg_sharpe_adjusted = mean(sharpe_adjusted, na.rm = TRUE),
    n_needs = sum(is_need),
    n_filled = sum(is_filled),
    avg_need_bonus = mean(need_bonus, na.rm = TRUE),
    avg_filled_penalty = mean(filled_penalty, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n_picks >= 2) |>
  mutate(
    rank_original = rank(-avg_sharpe_original),
    rank_adjusted = rank(-avg_sharpe_adjusted),
    rank_change = rank_original - rank_adjusted
  ) |>
  arrange(rank_adjusted)

cat(sprintf("  %d teams with 2+ R1 picks (2018-2022)\n", nrow(team_eff_adjusted)))

write_csv(team_eff_adjusted, "output/v2/team_efficiency_adjusted.csv")
cat("  Saved output/v2/team_efficiency_adjusted.csv\n")

# Also save the pick-level detail
write_csv(r1_with_needs, "output/v2/r1_picks_need_adjusted.csv")
cat("  Saved output/v2/r1_picks_need_adjusted.csv\n")

# Spot-check: top 5 teams whose rankings improved most
cat("\n  Top 5 teams most improved by need adjustment:\n")
top_improved <- team_eff_adjusted |>
  arrange(desc(rank_change)) |>
  head(5) |>
  select(team, n_picks, avg_sharpe_original, avg_sharpe_adjusted,
         rank_original, rank_adjusted, rank_change)
print(top_improved, n = Inf)

cat("\n  Top 5 teams most hurt by need adjustment:\n")
top_hurt <- team_eff_adjusted |>
  arrange(rank_change) |>
  head(5) |>
  select(team, n_picks, avg_sharpe_original, avg_sharpe_adjusted,
         rank_original, rank_adjusted, rank_change)
print(top_hurt, n = Inf)

# --- 5. Market Efficiency -----------------------------------------------------

cat("Computing market efficiency (post-contract performance)...\n")

# Find second contract signing year for each player
player_second_years <- df_eligible |>
  filter(second_apy_cap_pct > 0) |>
  mutate(name_norm = normalize_name(pfr_player_name)) |>
  inner_join(
    contracts |>
      mutate(name_norm = normalize_name(player)) |>
      select(name_norm, pos_group, year_signed, apy_cap_pct, years),
    by = c("name_norm", "pos_group"),
    relationship = "many-to-many"
  ) |>
  filter(
    year_signed >= season + 2,
    year_signed <= season + 6,
    abs(apy_cap_pct - second_apy_cap_pct) < 0.005
  ) |>
  group_by(pfr_player_id) |>
  slice_max(apy_cap_pct, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(pfr_player_id, pfr_player_name, pos_group, draft_season = season,
         second_apy_cap_pct, second_contract_year = year_signed,
         contract_years = years)

cat(sprintf("  Matched %d second contract signing years\n", nrow(player_second_years)))

# Post-contract snap performance (2 seasons after signing)
post_contract_snaps <- snaps_raw |>
  select(season, week, pfr_player_id, offense_pct, defense_pct) |>
  inner_join(
    player_second_years |>
      select(pfr_player_id, pos_group, second_contract_year),
    by = "pfr_player_id"
  ) |>
  filter(
    season >= second_contract_year,
    season <= second_contract_year + 1
  ) |>
  mutate(
    snap_pct = if_else(pos_group %in% offensive_groups, offense_pct, defense_pct)
  ) |>
  group_by(pfr_player_id) |>
  summarise(
    post_contract_games = n(),
    post_contract_avg_snap = mean(snap_pct, na.rm = TRUE),
    .groups = "drop"
  )

market_efficiency <- player_second_years |>
  left_join(post_contract_snaps, by = "pfr_player_id") |>
  filter(!is.na(post_contract_avg_snap), post_contract_games >= 10) |>
  left_join(HIT_THRESHOLDS, by = "pos_group") |>
  mutate(
    post_contract_snap_ratio = post_contract_avg_snap / hit_threshold
  )

# Position-level summary
market_by_pos <- market_efficiency |>
  group_by(pos_group) |>
  summarise(
    n = n(),
    mean_second_contract = mean(second_apy_cap_pct),
    mean_post_snap_ratio = mean(post_contract_snap_ratio),
    cor_contract_snaps = cor(second_apy_cap_pct, post_contract_avg_snap, use = "complete.obs"),
    .groups = "drop"
  ) |>
  arrange(cor_contract_snaps)

cat("  Market efficiency by position:\n")
print(market_by_pos, n = Inf)

write_csv(market_efficiency, "output/v2/market_efficiency.csv")
cat("  Saved output/v2/market_efficiency.csv\n")

# --- 6. NGS Prospect Scores (2025) -------------------------------------------

cat("Loading NGS prospect scores...\n")
ngs_prospects <- read_csv("data/ngs_prospect_scores_2025.csv", show_col_types = FALSE) |>
  filter(CmbnScr != "-") |>
  mutate(
    CmbnScr = as.numeric(CmbnScr),
    AthleteScr = as.numeric(AthleteScr),
    ProdScr = as.numeric(ProdScr),
    SizeScr = as.numeric(SizeScr)
  ) |>
  select(player = playerFullName, position, college = College,
         cmbn_score = CmbnScr, athlete_score = AthleteScr, prod_score = ProdScr,
         size_score = SizeScr) |>
  mutate(
    # Normalize names using the same strategy as Article 1 pipeline
    name_norm = normalize_name(player),
    pos_group = case_when(
      position == "DB" ~ "CB",
      position == "OL" ~ "IOL",
      position %in% names(POSITION_MAP) ~ POSITION_MAP[position],
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(pos_group))

cat(sprintf("  %d prospects with NGS scores\n", nrow(ngs_prospects)))

# Spot-check: verify Love is present and normalized
love_check <- ngs_prospects |> filter(grepl("love", name_norm))
cat(sprintf("  Love check: %s (cmbn_score: %s)\n",
            love_check$player[1], love_check$cmbn_score[1]))

write_csv(ngs_prospects, "output/v2/ngs_prospects_2025.csv")

# --- 6b. Historical NGS Scores (joined to draft data) --------------------------

cat("Loading historical NGS scores...\n")
ngs_historical <- read_csv("data/ngs_historical_scores.csv", show_col_types = FALSE) |>
  filter(CmbnScr != "-") |>
  mutate(
    CmbnScr = as.numeric(CmbnScr),
    AthleteScr = as.numeric(AthleteScr),
    ProdScr = suppressWarnings(as.numeric(ProdScr)),
    SizeScr = suppressWarnings(as.numeric(SizeScr))
  ) |>
  select(player = playerFullName, position, college = College,
         cmbn_score = CmbnScr, athlete_score = AthleteScr, prod_score = ProdScr,
         size_score = SizeScr) |>
  mutate(
    name_norm = normalize_name(player),
    pos_group = case_when(
      position %in% names(POSITION_MAP) ~ POSITION_MAP[position],
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(pos_group))

cat(sprintf("  %d historical players with NGS scores\n", nrow(ngs_historical)))

# Join to draft data via normalized name + position group
# Handle duplicates (e.g., Adrian Peterson x2) by keeping the match
# closest in draft year to the player's NFL career
draft_ngs_join <- draft_all |>
  mutate(name_norm = normalize_name(pfr_player_name)) |>
  select(pfr_player_id, pfr_player_name, name_norm, pos_group, season, pick, round, team, car_av, w_av)

ngs_matched <- ngs_historical |>
  inner_join(draft_ngs_join, by = c("name_norm", "pos_group"), relationship = "many-to-many") |>
  # Deduplicate: for each NGS record × draft record combo,

  # prefer the more recent draft pick (NGS data skews modern)
  group_by(name_norm, pos_group) |>
  slice_max(season, n = 1, with_ties = FALSE) |>
  ungroup()

cat(sprintf("  Matched %d historical NGS players to draft records\n", nrow(ngs_matched)))
cat(sprintf("  Draft seasons covered: %d-%d\n", min(ngs_matched$season), max(ngs_matched$season)))

# Spot-check: top RBs
cat("\n  Top 10 matched RBs by NGS Combined Score:\n")
ngs_matched |>
  filter(pos_group == "RB") |>
  arrange(desc(cmbn_score)) |>
  head(10) |>
  select(player, cmbn_score, prod_score, season, pick) |>
  print(n = Inf)

# --- NGS-based blue-chip classification (prospective) --------------------------
# Blue chip = top decile of CmbnScr within position group (pre-draft signal)

ngs_blue_chip <- ngs_matched |>
  group_by(pos_group) |>
  mutate(
    ngs_pctl = percent_rank(cmbn_score),
    is_ngs_blue_chip = ngs_pctl >= 0.90
  ) |>
  ungroup()

cat(sprintf("  NGS blue chips: %d of %d matched players\n",
            sum(ngs_blue_chip$is_ngs_blue_chip), nrow(ngs_blue_chip)))

# Add NGS scores to existing blue_chip data
blue_chip <- blue_chip |>
  left_join(
    ngs_blue_chip |>
      select(pfr_player_id, cmbn_score, athlete_score, prod_score, size_score,
             ngs_pctl, is_ngs_blue_chip),
    by = "pfr_player_id"
  )

write_csv(blue_chip, "output/v2/blue_chip.csv")
cat("  Updated blue_chip.csv with NGS scores\n")

# Save full NGS-matched dataset
write_csv(ngs_blue_chip, "output/v2/ngs_historical_matched.csv")
cat("  Saved output/v2/ngs_historical_matched.csv\n")

# --- NGS-based class depth (prospective class quality) -------------------------
# Use official NGS tiers: Below Avg (50-60), Average (60-70), Good (70-90), Elite (90+)
# Class depth = count of Good+ (>=70) prospects per class

# First, tag each player with their NGS tier
ngs_matched <- ngs_matched |>
  mutate(
    ngs_tier = case_when(
      cmbn_score >= 90 ~ "Elite",
      cmbn_score >= 70 ~ "Good",
      cmbn_score >= 60 ~ "Average",
      TRUE ~ "Below Avg"
    ),
    ngs_tier = factor(ngs_tier, levels = c("Elite", "Good", "Average", "Below Avg"))
  )

ngs_class_quality <- ngs_matched |>
  filter(season <= MAX_DRAFT_SEASON) |>
  group_by(season, pos_group) |>
  summarise(
    ngs_n = n(),
    ngs_mean_cmbn = mean(cmbn_score, na.rm = TRUE),
    ngs_max_cmbn = max(cmbn_score, na.rm = TRUE),
    ngs_mean_prod = mean(prod_score, na.rm = TRUE),
    ngs_top_gap = max(cmbn_score, na.rm = TRUE) - nth(sort(cmbn_score, decreasing = TRUE), 2, default = NA),
    ngs_n_elite = sum(cmbn_score >= 90, na.rm = TRUE),
    ngs_n_good_plus = sum(cmbn_score >= 70, na.rm = TRUE),
    ngs_n_avg_plus = sum(cmbn_score >= 60, na.rm = TRUE),
    ngs_pct_good_plus = mean(cmbn_score >= 70, na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("  NGS class quality: %d season-position combos\n", nrow(ngs_class_quality)))

# Merge into class_depth
class_depth <- class_depth |>
  left_join(ngs_class_quality, by = c("season", "pos_group"))

# Deep/Thin based on count of Good+ (>=70) prospects
# above position median = Deep, below = Thin
class_depth <- class_depth |>
  group_by(pos_group) |>
  mutate(
    ngs_median_good_plus = median(ngs_n_good_plus, na.rm = TRUE),
    ngs_class_quality = case_when(
      is.na(ngs_n_good_plus) ~ NA_character_,
      ngs_n_good_plus > ngs_median_good_plus ~ "Deep",
      ngs_n_good_plus < ngs_median_good_plus ~ "Thin",
      TRUE ~ "Average"
    )
  ) |>
  ungroup()

write_csv(class_depth, "output/v2/class_depth.csv")
cat("  Updated class_depth.csv with NGS class quality metrics\n")

# Save tier-level class breakdown for charting (ALL years, not just Sharpe-eligible)
ngs_class_tiers <- ngs_matched |>
  group_by(season, pos_group, ngs_tier) |>
  summarise(n = n(), .groups = "drop")

# Also add 2025 prospect class tiers from the prospect file
ngs_2025_tiers <- ngs_prospects |>
  mutate(
    ngs_tier = case_when(
      cmbn_score >= 90 ~ "Elite",
      cmbn_score >= 70 ~ "Good",
      cmbn_score >= 60 ~ "Average",
      TRUE ~ "Below Avg"
    ),
    ngs_tier = factor(ngs_tier, levels = c("Elite", "Good", "Average", "Below Avg"))
  ) |>
  group_by(pos_group, ngs_tier) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(season = 2025L)

# If 2025 already exists from historical match, replace with prospect file (more complete)
ngs_class_tiers <- ngs_class_tiers |>
  filter(season != 2025) |>
  bind_rows(ngs_2025_tiers)

write_csv(ngs_class_tiers, "output/v2/ngs_class_tiers.csv")
cat("  Saved output/v2/ngs_class_tiers.csv\n")

# Also re-save matched with tier column
write_csv(ngs_matched, "output/v2/ngs_historical_matched.csv")

# Spot-check: RB class quality by year
cat("\n  RB class quality (NGS) by year:\n")
class_depth |>
  filter(pos_group == "RB", !is.na(ngs_mean_cmbn)) |>
  select(season, hit_rate, class_quality, ngs_class_quality, ngs_n_good_plus, ngs_n_elite, ngs_max_cmbn, ngs_top_gap) |>
  arrange(season) |>
  print(n = Inf)

# --- 7. Prospective Class Depth (Draft Capital) --------------------------------

cat("Computing prospective class depth signals...\n")

draft_capital_density <- draft_all |>
  filter(season <= MAX_DRAFT_SEASON, round <= 3) |>
  group_by(season, pos_group) |>
  summarise(early_picks = n(), .groups = "drop")

class_depth <- class_depth |>
  left_join(draft_capital_density, by = c("season", "pos_group")) |>
  mutate(early_picks = replace_na(early_picks, 0))

write_csv(class_depth, "output/v2/class_depth.csv")
cat("  Updated class_depth with draft capital density\n")

cat("\n===================================\n")
cat("V2 pipeline complete!\n")
