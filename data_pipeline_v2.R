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
