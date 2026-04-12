# Explore: Decaying hit threshold and elite threshold by tier
# Compares current (75% / 90th pctl) vs decayed thresholds
#
# Idea: later-round picks shouldn't be held to the same bar as top-10 picks.
# Decay the snap threshold from 75% (current) toward 67% (original Riske 2/3)
# and the elite threshold from 90th percentile toward a lower bar.

library(tidyverse)
library(arrow)
library(scales)

# --- Load base data (same as pipeline) ----------------------------------------

analysis_df <- read_parquet("data/draft_sharpe_analysis.parquet")
fa_replacement <- read_csv("output/fa_replacement.csv", show_col_types = FALSE)

tier_order <- c("Top 10", "Late 1st", "Round 2", "Round 3", "Rounds 4-7")
analysis_df <- analysis_df |> mutate(tier = factor(tier, levels = tier_order))

sharpe_df <- analysis_df |> filter(season <= 2022)

# --- Positional baselines (reverse-engineer from current 75% thresholds) ------

# Current thresholds are 75% of baseline. Baseline = threshold / 0.75
BASELINES <- tibble(
  pos_group = c("IOL", "OT", "S", "LB", "CB", "WR", "QB", "EDGE", "DL", "TE", "RB"),
  baseline  = c(0.747, 0.730, 0.714, 0.692, 0.692, 0.640, 0.622, 0.609, 0.538, 0.534, 0.423) / 0.75
)

# --- Define decay schedules ---------------------------------------------------

# Hit threshold multiplier: 75% for top 10, decaying linearly to 67% for Rounds 4-7
hit_multipliers <- tibble(
  tier = factor(tier_order, levels = tier_order),
  hit_mult = seq(0.75, 2/3, length.out = 5)  # 0.750, 0.729, 0.708, 0.688, 0.667
)

# Elite threshold percentile: 90th for top 10, decaying to 75th for Rounds 4-7
elite_pctls <- tibble(
  tier = factor(tier_order, levels = tier_order),
  elite_pctl = seq(0.90, 0.75, length.out = 5)  # 0.90, 0.8625, 0.825, 0.7875, 0.75
)

cat("=== Decay Schedules ===\n\n")
cat("Hit threshold multiplier by tier:\n")
print(hit_multipliers)
cat("\nElite percentile by tier:\n")
print(elite_pctls)

# --- Scenario A: Current (75% / global 90th) ----------------------------------

current_elite <- quantile(sharpe_df$player_return[sharpe_df$player_return > 0], 0.90, na.rm = TRUE)

compute_sharpe <- function(df, elite_thresh) {
  df |>
    group_by(pos_group, tier) |>
    summarise(
      n = n(),
      hit_rate = mean(is_hit),
      mean_player_return = mean(player_return, na.rm = TRUE),
      sd_player_return = sd(player_return, na.rm = TRUE),
      elite_prob = mean(player_return >= elite_thresh, na.rm = TRUE),
      fa_replacement = first(fa_median_apy_cap_pct),
      .groups = "drop"
    ) |>
    mutate(
      sharpe_elite = (elite_prob * elite_thresh - fa_replacement) / sd_player_return,
      sharpe_elite = if_else(is.finite(sharpe_elite), sharpe_elite, NA_real_)
    )
}

scenario_a <- compute_sharpe(sharpe_df, current_elite) |>
  mutate(scenario = "A: Current (75% / P90)")

# --- Scenario B: Decayed hit threshold, global elite --------------------------

# Recompute is_hit, snap_ratio, rookie_surplus, player_return with decayed thresholds
build_decayed_df <- function(df, hit_mults, baselines) {
  df |>
    left_join(hit_mults, by = "tier") |>
    left_join(baselines, by = "pos_group") |>
    mutate(
      hit_threshold_decayed = baseline * hit_mult,
      is_hit_decayed = avg_snap_pct >= hit_threshold_decayed,
      snap_ratio_decayed = avg_snap_pct / hit_threshold_decayed,
      rookie_surplus_decayed = snap_ratio_decayed * pmax(fa_median_apy_cap_pct - rookie_apy_cap_pct, 0) * 4,
      player_return_decayed = second_apy_cap_pct + rookie_surplus_decayed
    )
}

decayed_df <- build_decayed_df(sharpe_df, hit_multipliers, BASELINES)

# Scenario B: decayed hits, but same global 90th elite threshold
scenario_b_sharpe <- decayed_df |>
  group_by(pos_group, tier) |>
  summarise(
    n = n(),
    hit_rate = mean(is_hit_decayed),
    mean_player_return = mean(player_return_decayed, na.rm = TRUE),
    sd_player_return = sd(player_return_decayed, na.rm = TRUE),
    elite_prob = mean(player_return_decayed >= current_elite, na.rm = TRUE),
    fa_replacement = first(fa_median_apy_cap_pct),
    .groups = "drop"
  ) |>
  mutate(
    sharpe_elite = (elite_prob * current_elite - fa_replacement) / sd_player_return,
    sharpe_elite = if_else(is.finite(sharpe_elite), sharpe_elite, NA_real_),
    scenario = "B: Decayed hit (75->67%), P90 elite"
  )

# --- Scenario C: Decayed hit threshold AND decayed elite threshold ------------

# Compute tier-specific elite thresholds from decayed player returns
tier_elite_thresholds <- decayed_df |>
  filter(player_return_decayed > 0) |>
  left_join(elite_pctls, by = "tier") |>
  group_by(tier, elite_pctl) |>
  summarise(
    elite_thresh_decayed = quantile(player_return_decayed, first(elite_pctl), na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Tier-Specific Elite Thresholds ===\n")
cat(sprintf("Current global P90: %.4f\n", current_elite))
print(tier_elite_thresholds)

scenario_c_sharpe <- decayed_df |>
  left_join(elite_pctls, by = "tier") |>
  left_join(tier_elite_thresholds, by = c("tier", "elite_pctl")) |>
  group_by(pos_group, tier, elite_thresh_decayed) |>
  summarise(
    n = n(),
    hit_rate = mean(is_hit_decayed),
    mean_player_return = mean(player_return_decayed, na.rm = TRUE),
    sd_player_return = sd(player_return_decayed, na.rm = TRUE),
    elite_prob = mean(player_return_decayed >= first(elite_thresh_decayed), na.rm = TRUE),
    fa_replacement = first(fa_median_apy_cap_pct),
    .groups = "drop"
  ) |>
  mutate(
    sharpe_elite = (elite_prob * elite_thresh_decayed - fa_replacement) / sd_player_return,
    sharpe_elite = if_else(is.finite(sharpe_elite), sharpe_elite, NA_real_),
    scenario = "C: Decayed hit + decayed elite (P90->P75)"
  )

# --- Compare ------------------------------------------------------------------

compare <- bind_rows(
  scenario_a |> select(pos_group, tier, n, hit_rate, elite_prob, sharpe_elite, scenario),
  scenario_b_sharpe |> select(pos_group, tier, n, hit_rate, elite_prob, sharpe_elite, scenario),
  scenario_c_sharpe |> select(pos_group, tier, n, hit_rate, elite_prob, sharpe_elite, scenario)
)

# Wide format: one row per pos_group × tier, columns for each scenario
compare_wide <- compare |>
  select(pos_group, tier, scenario, sharpe_elite) |>
  pivot_wider(names_from = scenario, values_from = sharpe_elite)

cat("\n\n=== SHARPE RATIO COMPARISON (all positions x tiers) ===\n\n")
compare_wide |>
  arrange(pos_group, tier) |>
  print(n = Inf, width = 200)

# Focus view: key positions, round by round
key_pos <- c("QB", "RB", "WR", "TE", "OT", "EDGE")

cat("\n\n=== KEY POSITIONS: Sharpe by Tier ===\n\n")
compare_wide |>
  filter(pos_group %in% key_pos) |>
  arrange(match(pos_group, key_pos), tier) |>
  print(n = Inf, width = 200)

# Delta view: how much did each scenario change from current?
delta <- compare |>
  select(pos_group, tier, scenario, sharpe_elite) |>
  pivot_wider(names_from = scenario, values_from = sharpe_elite) |>
  mutate(
    delta_B = `B: Decayed hit (75->67%), P90 elite` - `A: Current (75% / P90)`,
    delta_C = `C: Decayed hit + decayed elite (P90->P75)` - `A: Current (75% / P90)`
  )

cat("\n\n=== DELTAS FROM CURRENT (positive = Sharpe improved) ===\n\n")
delta |>
  filter(pos_group %in% key_pos) |>
  select(pos_group, tier, delta_B, delta_C) |>
  arrange(match(pos_group, key_pos), tier) |>
  print(n = Inf, width = 200)

# Hit rate changes
hit_compare <- compare |>
  select(pos_group, tier, scenario, hit_rate) |>
  pivot_wider(names_from = scenario, values_from = hit_rate) |>
  filter(pos_group %in% key_pos)

cat("\n\n=== HIT RATE COMPARISON (key positions) ===\n\n")
hit_compare |>
  arrange(match(pos_group, key_pos), tier) |>
  print(n = Inf, width = 200)

cat("\n\nDone. Review the tables above and decide if any scenario warrants inclusion.\n")
