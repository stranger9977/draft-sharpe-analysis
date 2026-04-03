# Counter-Article Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a second R Markdown article ("When to Ignore the Sharpe Ratio") that stress-tests Article 1's assumptions through four counter-arguments, using Jeremiah Love as a case study.

**Architecture:** Two new files — `data_pipeline_v2.R` (data prep for all four arguments) and `counter_analysis.Rmd` (the article). The v2 pipeline re-runs Article 1's pipeline to regenerate the parquet (not in repo), then adds new data layers: roster needs, class depth, blue-chip classification, and market efficiency. The Rmd reads the v2 outputs and renders to self-contained HTML.

**Tech Stack:** R 4.5, nflreadr, tidyverse, arrow, ggplot2, gt, nflplotR, scales, ggridges

---

## File Structure

| File | Purpose |
|------|---------|
| `data_pipeline_v2.R` | Sources Article 1 pipeline, adds 4 new data layers, exports to `output/v2/` |
| `counter_analysis.Rmd` | The article — loads v2 data, renders all charts and narrative |
| `output/v2/class_depth.csv` | Draft class hit rate and car_av by season × position |
| `output/v2/blue_chip.csv` | Player-level blue-chip classification with car_av percentiles |
| `output/v2/roster_needs.csv` | Team × season × position need flags |
| `output/v2/team_efficiency_adjusted.csv` | Need-adjusted team draft efficiency |
| `output/v2/market_efficiency.csv` | Post-contract snap performance vs contract value |

---

### Task 1: Re-run Article 1 Pipeline + Bootstrap V2 Script

The parquet file isn't in the repo. We need to regenerate it by running `data_pipeline.R`, then create the v2 script skeleton that sources the Article 1 outputs.

**Files:**
- Run: `data_pipeline.R`
- Create: `data_pipeline_v2.R`
- Create: `output/v2/` directory

- [ ] **Step 1: Create `data/` directory and run Article 1 pipeline**

```bash
cd /Users/nick/draft-sharpe-analysis
mkdir -p data
Rscript data_pipeline.R
```

Expected: parquet file at `data/draft_sharpe_analysis.parquet`, CSVs updated in `output/`.

- [ ] **Step 2: Create v2 pipeline skeleton**

Create `data_pipeline_v2.R` with shared setup — libraries, constants reused from Article 1, and the parquet + CSV loading:

```r
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

HIT_THRESHOLDS <- tibble::tibble(
  pos_group = c("IOL", "OT", "S", "LB", "CB", "WR", "QB", "EDGE", "DL", "TE", "RB"),
  hit_threshold = c(0.747, 0.730, 0.714, 0.692, 0.692, 0.640, 0.622, 0.609, 0.538, 0.534, 0.423)
)

TIER_BREAKS <- c(0, 10, 32, 64, 100, Inf)
TIER_LABELS <- c("Top 10", "Late 1st", "Round 2", "Round 3", "Rounds 4-7")

MAX_DRAFT_SEASON <- 2022

# --- Load Article 1 Outputs --------------------------------------------------

cat("Loading Article 1 outputs...\n")
df <- read_parquet("data/draft_sharpe_analysis.parquet")
sharpe <- read_csv("output/sharpe_ratios.csv", show_col_types = FALSE)
player_lookup <- read_csv("output/player_lookup.csv", show_col_types = FALSE)
fa_replacement <- read_csv("output/fa_replacement.csv", show_col_types = FALSE)

df_eligible <- df |> filter(season <= MAX_DRAFT_SEASON)

cat(sprintf("  Loaded %d players (%d Sharpe-eligible)\n", nrow(df), nrow(df_eligible)))

dir.create("output/v2", showWarnings = FALSE, recursive = TRUE)
```

- [ ] **Step 3: Verify pipeline loads correctly**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e 'source("data_pipeline_v2.R")'
```

Expected: prints loaded player counts, no errors.

- [ ] **Step 4: Commit**

```bash
git add data_pipeline_v2.R
git commit -m "feat: bootstrap v2 data pipeline for counter-article"
```

---

### Task 2: Draft Class Depth Analysis

Compute class-level quality metrics for each season × position group. This feeds both the class depth argument and the Love case study.

**Files:**
- Modify: `data_pipeline_v2.R`
- Output: `output/v2/class_depth.csv`

- [ ] **Step 1: Add class depth computation to v2 pipeline**

Append to `data_pipeline_v2.R`:

```r
# --- 1. Draft Class Depth ----------------------------------------------------

cat("Computing draft class depth...\n")

# Need draft data with car_av — reload from nflreadr since parquet
# doesn't include players drafted after 2022 and car_av may be missing
draft_raw <- load_draft_picks(seasons = TRUE)

draft_all <- draft_raw |>
  filter(!is.na(position)) |>
  mutate(
    pos_group = POSITION_MAP[position],
    tier = cut(pick, breaks = TIER_BREAKS, labels = TIER_LABELS, right = TRUE)
  ) |>
  filter(!is.na(pos_group))

# Class depth: for each season × pos_group, how many hits and what was average car_av?
# Use df_eligible for hit data (players with 4+ years), draft_all for car_av
class_depth <- df_eligible |>
  group_by(season, pos_group) |>
  summarise(
    n_drafted = n(),
    n_hits = sum(is_hit),
    hit_rate = mean(is_hit),
    mean_player_return = mean(player_return, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Join car_av from the raw draft data
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
  # Tag deep vs thin: above/below median hit rate for that position
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
```

- [ ] **Step 2: Run and verify**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e 'source("data_pipeline_v2.R")'
```

Expected: prints "Class depth computed: ~140 season-position combos" (11 positions × ~13 years), CSV saved.

- [ ] **Step 3: Spot-check the data**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e '
d <- read.csv("output/v2/class_depth.csv")
cat("RB class depth by year:\n")
d |> dplyr::filter(pos_group == "RB") |>
  dplyr::select(season, n_drafted, n_hits, hit_rate, mean_car_av, class_quality) |>
  print(n = 20)
'
```

Expected: shows RB class depth per year with some "Deep" and some "Thin" years.

- [ ] **Step 4: Commit**

```bash
git add data_pipeline_v2.R output/v2/class_depth.csv
git commit -m "feat: add draft class depth analysis to v2 pipeline"
```

---

### Task 3: Blue-Chip Classification

Compute `car_av` percentiles per position group and classify top-decile players as blue chips. This is the independent quality signal that's not baked into the Sharpe formula.

**Files:**
- Modify: `data_pipeline_v2.R`
- Output: `output/v2/blue_chip.csv`

- [ ] **Step 1: Add blue-chip computation to v2 pipeline**

Append to `data_pipeline_v2.R`:

```r
# --- 2. Blue-Chip Classification ----------------------------------------------

cat("Classifying blue-chip prospects...\n")

# car_av percentile within position group (using all drafted players with 4+ years)
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
  # Join Sharpe data from Article 1
  left_join(
    player_lookup |> select(pfr_player_name, season, pos_group, player_return,
                            player_sharpe, is_hit, avg_snap_pct),
    by = c("pfr_player_name", "season", "pos_group")
  ) |>
  # Join class depth
  left_join(
    class_depth |> select(season, pos_group, class_quality, hit_rate),
    by = c("season", "pos_group")
  ) |>
  rename(class_hit_rate = hit_rate)

cat(sprintf("  %d players classified, %d blue chips\n",
            nrow(blue_chip), sum(blue_chip$is_blue_chip)))

# Blue chip summary by position
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
```

- [ ] **Step 2: Run and verify**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e 'source("data_pipeline_v2.R")'
```

Expected: prints blue chip count (~10% of eligible players), summary table showing blue-chip hit rates near 90%+ for most positions.

- [ ] **Step 3: Spot-check blue-chip RBs**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e '
bc <- read.csv("output/v2/blue_chip.csv")
cat("Blue-chip RBs:\n")
bc |> dplyr::filter(pos_group == "RB", is_blue_chip == TRUE) |>
  dplyr::select(pfr_player_name, season, pick, car_av, player_return, class_quality) |>
  dplyr::arrange(dplyr::desc(car_av)) |>
  print(n = 20)
'
```

Expected: names like Ezekiel Elliott, Saquon Barkley, Dalvin Cook, etc. Should show their draft position and whether they were in deep or thin RB classes.

- [ ] **Step 4: Commit**

```bash
git add data_pipeline_v2.R output/v2/blue_chip.csv
git commit -m "feat: add blue-chip classification (top decile car_av) to v2 pipeline"
```

---

### Task 4: Roster Needs Analysis

For each team × draft year, determine which positions were "filled" (hit-level starter on non-expiring contract) vs "needs." Then recompute team draft efficiency penalizing only actual needs.

**Files:**
- Modify: `data_pipeline_v2.R`
- Output: `output/v2/roster_needs.csv`, `output/v2/team_efficiency_adjusted.csv`

- [ ] **Step 1: Add roster needs computation to v2 pipeline**

Append to `data_pipeline_v2.R`:

```r
# --- 3. Roster Needs ----------------------------------------------------------

cat("Computing roster needs by team...\n")

# Load snap counts for determining starters
snaps_raw <- load_snap_counts(seasons = TRUE)

# Load contracts for expiry check
contracts_raw <- load_contracts()
contracts <- contracts_raw |>
  mutate(pos_group = POSITION_MAP[position]) |>
  filter(!is.na(pos_group), !is.na(apy_cap_pct))

offensive_groups <- c("QB", "RB", "WR", "TE", "OT", "IOL")

# For each team-season, find the best player at each position by snap %
# in the PRIOR season (the season before the draft)
team_starters <- snaps_raw |>
  filter(game_type == "REG" | is.na(game_type)) |>
  select(season, week, team, pfr_player_id, offense_pct, defense_pct) |>
  # We need position info — join via draft data or roster
  inner_join(
    draft_all |> select(pfr_player_id, pos_group) |> distinct(),
    by = "pfr_player_id"
  ) |>
  mutate(
    snap_pct = if_else(pos_group %in% offensive_groups, offense_pct, defense_pct)
  ) |>
  group_by(season, team, pfr_player_id, pos_group) |>
  summarise(
    games = n(),
    avg_snap_pct = mean(snap_pct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Best player at each position per team per season
  group_by(season, team, pos_group) |>
  slice_max(avg_snap_pct, n = 1, with_ties = FALSE) |>
  ungroup()

# Check if starter meets hit threshold
team_starters <- team_starters |>
  left_join(HIT_THRESHOLDS, by = "pos_group") |>
  mutate(starter_is_hit = avg_snap_pct >= hit_threshold)

# Check contract expiry: is the starter's contract expiring?
# A contract is "expiring" if it ends in the current season or the next
normalize_name <- function(x) {
  x |>
    str_to_lower() |> str_trim() |>
    str_remove_all("\\.") |> str_remove_all(",") |>
    str_remove_all("\\b(jr|sr|ii|iii|iv|v)$") |>
    str_replace_all("\\s+", " ") |> str_trim()
}

# Get contract end years for players
contract_expiry <- contracts |>
  mutate(
    contract_end = year_signed + years - 1,
    name_norm = normalize_name(player)
  ) |>
  select(name_norm, pos_group, year_signed, years, contract_end, apy_cap_pct)

# Join starter names from draft data
starter_names <- draft_all |>
  select(pfr_player_id, pfr_player_name) |>
  distinct() |>
  mutate(name_norm = normalize_name(pfr_player_name))

team_starters_named <- team_starters |>
  left_join(starter_names, by = "pfr_player_id")

# For each starter, find their active contract in that season
# Contract is active if year_signed <= season AND contract_end >= season
team_starters_contracts <- team_starters_named |>
  left_join(
    contract_expiry,
    by = c("name_norm", "pos_group"),
    relationship = "many-to-many"
  ) |>
  filter(year_signed <= season, contract_end >= season) |>
  group_by(season, team, pos_group, pfr_player_id) |>
  slice_max(apy_cap_pct, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    contract_expiring = contract_end <= season  # expires this season
  )

# Build the needs table: for each team-season-position, is it filled?
# "Filled" = hit-level starter on non-expiring contract
# Need to cover all team-season-position combos

all_pos <- unique(HIT_THRESHOLDS$pos_group)
draft_seasons <- sort(unique(df_eligible$season))

# All team-season combos from actual draft picks
team_seasons <- df_eligible |>
  distinct(team, season)

roster_needs <- team_seasons |>
  crossing(pos_group = all_pos) |>
  # Look at PRIOR season starters (draft happens before the new season)
  mutate(prior_season = season - 1) |>
  left_join(
    team_starters_contracts |>
      select(prior_season = season, team, pos_group, starter_is_hit,
             contract_expiring, avg_snap_pct, pfr_player_name),
    by = c("prior_season", "team", "pos_group")
  ) |>
  mutate(
    starter_is_hit = replace_na(starter_is_hit, FALSE),
    contract_expiring = replace_na(contract_expiring, TRUE),  # no contract found = need
    is_filled = starter_is_hit & !contract_expiring,
    is_need = !is_filled
  ) |>
  select(team, season, pos_group, is_filled, is_need,
         starter_is_hit, contract_expiring, avg_snap_pct, pfr_player_name)

cat(sprintf("  Roster needs computed: %d team-season-position combos\n", nrow(roster_needs)))
cat(sprintf("  Filled: %d, Needs: %d\n",
            sum(roster_needs$is_filled), sum(roster_needs$is_need)))

write_csv(roster_needs, "output/v2/roster_needs.csv")
cat("  Saved output/v2/roster_needs.csv\n")
```

- [ ] **Step 2: Run and verify**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e 'source("data_pipeline_v2.R")'
```

Expected: prints needs count. Most positions should be "needs" (it's hard to have every position filled).

- [ ] **Step 3: Spot-check Bills QB needs**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e '
rn <- read.csv("output/v2/roster_needs.csv")
cat("Bills QB roster need by year:\n")
rn |> dplyr::filter(team == "BUF", pos_group == "QB") |>
  dplyr::select(season, is_filled, starter_is_hit, contract_expiring, pfr_player_name) |>
  print(n = 20)
'
```

Expected: QB should show as "filled" (is_filled = TRUE) starting in 2019-2020 after Allen establishes himself, and "need" before that.

- [ ] **Step 4: Commit**

```bash
git add data_pipeline_v2.R output/v2/roster_needs.csv
git commit -m "feat: add roster needs analysis to v2 pipeline"
```

---

### Task 5: Need-Adjusted Team Efficiency

Recompute the team draft efficiency scores from Article 1, but only penalize teams for positions they actually needed.

**Files:**
- Modify: `data_pipeline_v2.R`
- Output: `output/v2/team_efficiency_adjusted.csv`

- [ ] **Step 1: Add need-adjusted efficiency computation**

Append to `data_pipeline_v2.R`:

```r
# --- 4. Need-Adjusted Team Efficiency -----------------------------------------

cat("Computing need-adjusted team efficiency...\n")

# Original team efficiency (from Article 1 logic):
# For each R1 pick, get sharpe_elite of that pos_group × tier
sharpe_tiers <- sharpe |> select(pos_group, tier, sharpe_elite)

team_picks <- df_eligible |>
  filter(tier %in% c("Top 10", "Late 1st")) |>
  left_join(sharpe_tiers, by = c("pos_group", "tier")) |>
  mutate(sharpe_elite = replace_na(sharpe_elite, 0))

# Original efficiency
original_eff <- team_picks |>
  filter(season >= 2018) |>
  group_by(team) |>
  summarise(
    n_picks = n(),
    avg_sharpe_original = mean(sharpe_elite),
    .groups = "drop"
  ) |>
  filter(n_picks >= 2)

# Need-adjusted: boost sharpe for picks that addressed a need,
# zero out the penalty for picks at filled positions
# Logic: if a team drafted a "negative Sharpe" position but it was a NEED,
# the pick was justified. If they drafted a "positive Sharpe" position
# that was already FILLED, they wasted capital.

team_picks_needs <- team_picks |>
  filter(season >= 2018) |>
  left_join(
    roster_needs |> select(team, season, pos_group, is_need, is_filled),
    by = c("team", "season", "pos_group")
  ) |>
  mutate(
    is_need = replace_na(is_need, TRUE),  # if no data, assume need
    # Adjusted sharpe: if the position was a need, keep the pick's sharpe
    # If the position was filled, penalize (use negative sharpe or 0)
    # Key insight: a "bad Sharpe" pick at a real need is better than
    # Article 1 suggests, and a "good Sharpe" pick at a filled position
    # is worse.
    need_bonus = if_else(is_need & sharpe_elite < 0, abs(sharpe_elite) * 0.5, 0),
    filled_penalty = if_else(is_filled & sharpe_elite > 0, -sharpe_elite * 0.5, 0),
    sharpe_adjusted = sharpe_elite + need_bonus + filled_penalty
  )

adjusted_eff <- team_picks_needs |>
  group_by(team) |>
  summarise(
    n_picks = n(),
    avg_sharpe_adjusted = mean(sharpe_adjusted),
    n_need_picks = sum(is_need),
    n_filled_picks = sum(is_filled),
    .groups = "drop"
  ) |>
  filter(n_picks >= 2)

# Combine original and adjusted
team_efficiency <- original_eff |>
  inner_join(adjusted_eff, by = c("team", "n_picks")) |>
  mutate(
    rank_original = rank(-avg_sharpe_original),
    rank_adjusted = rank(-avg_sharpe_adjusted),
    rank_change = rank_original - rank_adjusted  # positive = improved with adjustment
  ) |>
  arrange(rank_adjusted)

cat("  Top 5 biggest rank improvements with need adjustment:\n")
team_efficiency |>
  arrange(desc(rank_change)) |>
  head(5) |>
  select(team, rank_original, rank_adjusted, rank_change) |>
  print()

write_csv(team_efficiency, "output/v2/team_efficiency_adjusted.csv")
cat("  Saved output/v2/team_efficiency_adjusted.csv\n")
```

- [ ] **Step 2: Run and verify**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e 'source("data_pipeline_v2.R")'
```

Expected: prints top 5 teams whose rankings improved. Bills should be among them.

- [ ] **Step 3: Commit**

```bash
git add data_pipeline_v2.R output/v2/team_efficiency_adjusted.csv
git commit -m "feat: add need-adjusted team efficiency to v2 pipeline"
```

---

### Task 6: Market Efficiency Analysis

Compare second-contract value to post-contract snap performance. Identify which positions the market systematically overpays.

**Files:**
- Modify: `data_pipeline_v2.R`
- Output: `output/v2/market_efficiency.csv`

- [ ] **Step 1: Add market efficiency computation**

Append to `data_pipeline_v2.R`:

```r
# --- 5. Market Efficiency -----------------------------------------------------

cat("Computing market efficiency (post-contract performance)...\n")

# For players with second contracts, measure snap % in the 2 seasons
# AFTER the second contract was signed
second_contract_players <- df_eligible |>
  filter(second_apy_cap_pct > 0, !is.na(second_apy_cap_pct)) |>
  select(pfr_player_id, pfr_player_name, pos_group, season,
         second_apy_cap_pct, second_contract_year = season) |>
  # Recompute second contract year from the contracts data
  left_join(
    player_lookup |>
      select(pfr_player_name, season, pos_group, second_apy_cap_pct) |>
      filter(second_apy_cap_pct > 0),
    by = c("pfr_player_name", "season", "pos_group", "second_apy_cap_pct")
  )

# We need the actual year the second contract was signed
# Re-derive from the original contract matching logic
# Second contract = signed between season+2 and season+6
contract_years <- contracts |>
  mutate(name_norm = normalize_name(player)) |>
  select(name_norm, pos_group, year_signed, apy_cap_pct, years)

player_second_years <- df_eligible |>
  filter(second_apy_cap_pct > 0) |>
  mutate(name_norm = normalize_name(pfr_player_name)) |>
  inner_join(
    contract_years,
    by = c("name_norm", "pos_group"),
    relationship = "many-to-many"
  ) |>
  filter(
    year_signed >= season + 2,
    year_signed <= season + 6,
    abs(apy_cap_pct - second_apy_cap_pct) < 0.001  # match the contract
  ) |>
  group_by(pfr_player_id) |>
  slice_max(apy_cap_pct, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(pfr_player_id, pfr_player_name, pos_group, draft_season = season,
         second_apy_cap_pct, second_contract_year = year_signed,
         contract_years = years)

# Now get snap % for 2 seasons after the second contract was signed
post_contract_snaps <- snaps_raw |>
  select(season, week, pfr_player_id, offense_pct, defense_pct) |>
  inner_join(
    player_second_years |>
      select(pfr_player_id, pos_group, second_contract_year),
    by = "pfr_player_id"
  ) |>
  filter(
    season >= second_contract_year,
    season <= second_contract_year + 1  # 2 seasons after signing
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
    post_contract_snap_ratio = post_contract_avg_snap / hit_threshold,
    # Overpay index: high contract, low post-contract performance
    # Normalize contract to 0-1 scale within position for comparison
    contract_vs_performance = post_contract_snap_ratio - (second_apy_cap_pct * 10)
  )

# Position-level overpay summary
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

cat("  Market efficiency by position (correlation: contract vs post-contract snaps):\n")
print(market_by_pos, n = Inf)

write_csv(market_efficiency, "output/v2/market_efficiency.csv")
cat("  Saved output/v2/market_efficiency.csv\n")

cat("\nV2 pipeline complete!\n")
```

- [ ] **Step 2: Run and verify**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e 'source("data_pipeline_v2.R")'
```

Expected: prints correlation table by position. Positions with low correlation (contract value doesn't predict future performance) support the market inefficiency argument.

- [ ] **Step 3: Commit**

```bash
git add data_pipeline_v2.R output/v2/market_efficiency.csv
git commit -m "feat: add market efficiency analysis to v2 pipeline"
```

---

### Task 7: Counter-Analysis Rmd — Setup and Intro

Create the Rmd file with YAML config, setup chunk, data loading, and the intro/hook sections.

**Files:**
- Create: `counter_analysis.Rmd`

- [ ] **Step 1: Create Rmd with setup and intro**

```r
---
title: "When to Ignore the Sharpe Ratio"
subtitle: "Stress-testing our NFL Draft model — and finding when it's wrong"
output:
  html_document:
    toc: true
    toc_float: true
    self_contained: true
knit: (function(input, ...) rmarkdown::render(input, output_dir = dirname(input), ...))
---
```

Setup chunk loading all v2 data, Article 1 data, same styling constants (pos_colors, tier_order). Then a narrative hook:

> "In Part 1, we built a quasi-Sharpe ratio for NFL draft picks and concluded that RB shows negative returns across all tiers. We said you should almost never draft a running back in the first round. But we made assumptions — and some of them were wrong. This is Part 2: the case against our own model."

Then a brief recap section (2-3 paragraphs summarizing Article 1 with a link), and an assumptions table showing which ones we're testing.

- [ ] **Step 2: Verify it knits empty**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e 'rmarkdown::render("counter_analysis.Rmd")'
```

Expected: produces `counter_analysis.html` with the intro text and empty sections.

- [ ] **Step 3: Commit**

```bash
git add counter_analysis.Rmd
git commit -m "feat: scaffold counter-analysis Rmd with setup and intro"
```

---

### Task 8: Rmd — Argument 1: Roster Needs

Add the roster needs section with the Bills case study, before/after team efficiency charts, and narrative.

**Files:**
- Modify: `counter_analysis.Rmd`

- [ ] **Step 1: Add roster needs section**

Charts to build:
1. **Bills QB timeline** — Small multiple or timeline showing Bills' QB starter, snap %, contract status, and draft picks by year. Shows QB was "filled" post-Allen.
2. **Before/after team efficiency dumbbell chart** — Each team as a point, x-axis = original rank, connected to adjusted rank. Teams that improve are highlighted.
3. **Need vs filled picks scatter** — For all R1 picks, Sharpe on y-axis, colored by need/filled. Shows that "bad Sharpe" picks often addressed real needs.

Narrative: "The Bills haven't drafted a QB since Josh Allen. Article 1's model sees this as inefficiency — they're ignoring the highest-Sharpe position. But Allen was a hit. The position was filled. Penalizing the Bills for not drafting QB is like telling someone who already owns a house to keep buying houses because real estate has good returns."

- [ ] **Step 2: Verify render**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e 'rmarkdown::render("counter_analysis.Rmd")'
```

- [ ] **Step 3: Commit**

```bash
git add counter_analysis.Rmd charts/
git commit -m "feat: add roster needs argument with Bills case study"
```

---

### Task 9: Rmd — Argument 2: Draft Class Depth

Add the class depth section with heatmap, deep-vs-thin Sharpe comparison, and RB class profile.

**Files:**
- Modify: `counter_analysis.Rmd`

- [ ] **Step 1: Add class depth section**

Charts to build:
1. **Class depth heatmap** — Position (y-axis) × draft year (x-axis), fill = hit rate. Shows the dramatic year-to-year variance. Use `geom_tile()` with a diverging color scale centered on median.
2. **Deep vs thin Sharpe comparison** — Grouped bar chart: for each position, Sharpe in deep-class years vs thin-class years. Shows the gap.
3. **RB class timeline** — RB-specific version of the heatmap annotated with notable RBs drafted each year. Setup for the Love argument.

Narrative: "Article 1's Sharpe ratios average across 13 draft classes. But look at the variance: RB hit rate ranges from X% in [year] to Y% in [year]. When you tell a GM 'don't draft an RB in round 1,' you're giving them average advice for a non-average situation."

- [ ] **Step 2: Verify render**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e 'rmarkdown::render("counter_analysis.Rmd")'
```

- [ ] **Step 3: Commit**

```bash
git add counter_analysis.Rmd charts/
git commit -m "feat: add draft class depth argument with heatmap"
```

---

### Task 10: Rmd — Argument 3: Blue-Chip Prospects + Love Case Study

Add the blue-chip analysis and the Love synthesis section.

**Files:**
- Modify: `counter_analysis.Rmd`

- [ ] **Step 1: Add blue-chip section**

Charts to build:
1. **Blue-chip vs regular hit rates by tier** — Faceted by position. Shows that blue-chip players hit at dramatically higher rates even at "bad Sharpe" positions.
2. **Pick-level return curves by position** — LOESS smoothed `player_return` by pick number for each position. Key chart: shows where the RB blue-chip curve crosses the average EDGE/CB curve.
3. **Decision matrix** — 2×2 quadrant: x-axis = class depth (thin → deep), y-axis = prospect quality (regular → blue chip). Each quadrant gets a recommendation. Historical picks plotted as points.

Love section narrative: "So when SHOULD you draft an RB at 10? When three conditions align: the prospect is a blue chip (top-decile talent), the class is thin (no comparable value on Day 2), and your team has a real need. For Jeremiah Love in 2025, the question isn't whether the Sharpe ratio says RB is bad — it's whether Love meets the blue-chip test in a thin enough class."

- [ ] **Step 2: Verify render**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e 'rmarkdown::render("counter_analysis.Rmd")'
```

- [ ] **Step 3: Commit**

```bash
git add counter_analysis.Rmd charts/
git commit -m "feat: add blue-chip analysis and Love case study"
```

---

### Task 11: Rmd — Argument 4: Market Inefficiency

Add the market inefficiency section with scatter plots and overpay analysis.

**Files:**
- Modify: `counter_analysis.Rmd`

- [ ] **Step 1: Add market inefficiency section**

Charts to build:
1. **Contract vs post-contract performance scatter** — Faceted by position group. x-axis = second contract cap %, y-axis = post-contract snap ratio. With regression line. Positions where the line is flat or negative = market overpays.
2. **Overpay index bar chart** — By position: correlation between contract and post-contract performance. Low correlation = market is a bad signal.
3. **Original vs adjusted Sharpe** — Side-by-side or dumbbell chart showing how Sharpe ratios shift when using performance-adjusted returns instead of raw contract values.

Narrative: "If the market were perfectly efficient, a player's second contract would predict their future performance. It doesn't. The correlation between contract value and post-contract snap percentage is [X] for RBs and [Y] for QBs. The Sharpe ratio is measuring what GMs *believe* players are worth, not what they actually deliver."

- [ ] **Step 2: Verify render**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e 'rmarkdown::render("counter_analysis.Rmd")'
```

- [ ] **Step 3: Commit**

```bash
git add counter_analysis.Rmd charts/
git commit -m "feat: add market inefficiency argument"
```

---

### Task 12: Rmd — Revised Framework + Conclusion

Add the decision framework and closing sections.

**Files:**
- Modify: `counter_analysis.Rmd`

- [ ] **Step 1: Add revised framework section**

Build a decision tree visualization or formatted table:

```
Should you follow the Sharpe ratio for this pick?

1. Is the position a real team need? (roster needs)
   - No → Sharpe applies. Don't draft luxury positions.
   - Yes ↓

2. Is this a blue-chip prospect? (top-decile car_av comp)
   - No → Sharpe applies. Wait for Day 2.
   - Yes ↓

3. Is the class thin at this position? (below-median depth)
   - No → Sharpe still applies. Comparable talent available later.
   - Yes → OVERRIDE the Sharpe ratio. Take the blue chip.
```

Then "What We Still Believe" section — 3-4 bullets reaffirming Article 1's core findings that survived:
- QB is still the best top-10 investment on average
- Late first round is still the sweet spot for OT/EDGE
- Day 3 RBs are still fine most of the time
- The aggregate advice is right; it's the edge cases where context overrides the model

- [ ] **Step 2: Verify final render**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript -e 'rmarkdown::render("counter_analysis.Rmd")'
```

Expected: full article renders cleanly with all sections, charts, and narrative.

- [ ] **Step 3: Commit**

```bash
git add counter_analysis.Rmd charts/
git commit -m "feat: add revised framework and conclusion to counter-article"
```

---

### Task 13: Final Polish and Deploy

Clean up, verify the full pipeline runs end-to-end, and prepare for GitHub Pages.

**Files:**
- Modify: `counter_analysis.Rmd` (minor fixes from review)
- Output: `counter_analysis.html`

- [ ] **Step 1: Run full pipeline end-to-end**

```bash
cd /Users/nick/draft-sharpe-analysis
Rscript data_pipeline.R && Rscript data_pipeline_v2.R && Rscript -e 'rmarkdown::render("counter_analysis.Rmd")'
```

Expected: all three steps complete without errors, `counter_analysis.html` generated.

- [ ] **Step 2: Verify HTML renders correctly**

```bash
open /Users/nick/draft-sharpe-analysis/counter_analysis.html
```

Visually check: all charts render, TOC works, narrative flows, no broken images.

- [ ] **Step 3: Commit everything**

```bash
git add -A
git commit -m "feat: complete counter-article — When to Ignore the Sharpe Ratio"
```

- [ ] **Step 4: Push to GitHub**

```bash
git push origin main
```

GitHub Pages should automatically serve `counter_analysis.html` at the repo's Pages URL.
