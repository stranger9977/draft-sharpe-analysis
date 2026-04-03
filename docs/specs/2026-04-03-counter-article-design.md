# Counter-Article: "When to Ignore the Sharpe Ratio"

## Overview

A second R Markdown article that stress-tests the Draft Sharpe analysis from Article 1. Rather than tearing it down, it identifies the conditions under which the model's advice should be overridden. Uses Jeremiah Love (2025 RB, Notre Dame) as a recurring case study — Article 1 opened with "Should You Draft Jeremiah Love at 10?" and this article answers "Actually, maybe yes."

**Format:** R Markdown → self-contained HTML → GitHub Pages (same repo, separate page)
**Data:** Extends Article 1's pipeline with additional nflreadr data (rosters, contracts, `car_av`)

---

## The Four Counter-Arguments

### 1. Roster Needs — "The Bills Don't Need Another QB"

**Claim:** Article 1's team efficiency metric penalizes teams for not drafting Sharpe-optimal positions, ignoring that teams have different needs based on existing rosters. A team that drafted a franchise QB shouldn't lose efficiency points for never drafting QB again.

**Methodology:**
- Pull team rosters via `nflreadr::load_rosters()` for each draft year
- Pull snap counts for each team's players in the prior season
- A position is **"filled"** if the team had a player who met Article 1's hit threshold (positional snap %) AND that player was NOT on an expiring contract (use OTC contract data for expiry check)
- A position is a **"need"** if no hit-level player exists OR the incumbent's contract is expiring
- Recompute team draft efficiency: only penalize teams for missing Sharpe-optimal positions they actually NEEDED
- Compare original vs need-adjusted team rankings

**Key chart ideas:**
- Before/after team efficiency rankings with need adjustment
- Bills case study: show their roster had QB filled every year post-Allen, so "not drafting QB" was correct
- Scatter: teams that improved most vs least with need adjustment

**Data sources:** Existing pipeline + `load_rosters()` + OTC contracts (already loaded)

---

### 2. Draft Class Depth — "Not All WR Classes Are Created Equal"

**Claim:** The model averages across all draft classes, but class quality varies wildly by position. Drafting WR in a loaded 2020 class is different than drafting WR in a thin year. Teams making "suboptimal" positional choices may be responding to class quality.

**Methodology:**
- For each draft year × position group, compute:
  - Class hit rate (% of drafted players at that position who became hits)
  - Class mean `car_av` (average career value of that position cohort)
  - Class depth: number of players drafted at that position who became hits
- Tag each class-position combo as "deep" (above-median hit rate) vs "thin" (below-median)
- Split Sharpe ratios by class depth: does drafting a position in a deep class yield better returns than in a thin class?
- Show that the "right" position to draft depends on the year

**Key chart ideas:**
- Heatmap: position × draft year, colored by class hit rate (shows the variance)
- Sharpe ratio comparison: same position, deep class vs thin class
- RB class depth by year — set up the Love argument by showing years where RB classes were strong vs weak

**Data sources:** Existing pipeline (`hit_rates`, `player_lookup` grouped by season)

---

### 3. Blue-Chip Prospects — "When the Talent Is Undeniable"

**Claim:** The model says "don't draft RB in round 1" but that advice averages over all RB prospects. When a generational talent is available — especially in a thin class — the calculus changes. The question isn't "should you draft an RB at 10" but "should you draft THIS RB at 10."

**Definition:** Blue chip = top decile of `car_av` at their position group. This is independent of the Sharpe formula (not snap-based, not contract-based).

**Methodology:**
- Compute `car_av` percentile within each position group
- Define blue chip as >= 90th percentile `car_av` for their position
- For blue-chip players: what was their hit rate? Their Sharpe? Their draft position?
- Key question: at what pick range do blue-chip RBs outperform average EDGE/CB/WR picks?
  - Plot position-specific player_return curves by individual pick number
  - Find the crossover points where "best RB available" > "average premium position"
- Combine with class depth: blue chip + thin class = strongest case to break the model

**Love case study:**
- 2025 RB class profile: Love as the consensus top RB with significant separation from RB2
- Historical comps: years where a single dominant RB in a thin class was drafted early — outcomes
- Build the argument: if Love's prospect profile matches historical blue-chip RBs, and the 2025 RB class is thin, the Sharpe model's "don't draft RB early" advice may not apply

**Key chart ideas:**
- Pick-level hit rate curves by position (continuous, not tiered) — where do they cross?
- Blue-chip vs non-blue-chip outcomes at each tier, by position
- "When to break the model" decision matrix: class depth × prospect quality quadrant chart
- Love overlay: where does he sit on the matrix?

**Data sources:** Existing pipeline + `car_av` from draft data (already loaded but unused)

---

### 4. Market Inefficiency — "The Market Isn't Always Right"

**Claim:** Article 1's return metric uses second-contract value as a proxy for player quality. But NFL free agency systematically overpays at certain positions. If the market is wrong, the Sharpe ratio measures market sentiment, not actual value.

**Methodology:**
- For players with second contracts: compare second-contract APY cap % to post-contract performance (snap % in the 2 years after signing)
- Compute a "contract efficiency" score: post-contract snap % / contract cap %
- Identify positions where the market systematically overpays (high contract, declining snaps) vs underpays
- Recompute Sharpe ratios using performance-adjusted returns instead of raw contract values
- Show which positions' rankings shift

**Key chart ideas:**
- Scatter by position: second contract cap % vs post-contract snap % (with regression line)
- "Overpay index" by position — which positions have the biggest gap between contract and performance?
- Original vs adjusted Sharpe ratios side by side

**Data sources:** Existing pipeline + snap data for post-contract years (extend snap window beyond rookie deal)

---

## Article Structure

1. **Hook** — "We built a model that says never draft an RB in round 1. Here's when that advice is wrong."
2. **Quick recap** — 2-paragraph summary of Article 1's methodology and key findings (with link)
3. **The Assumptions** — Brief table of what Article 1 assumed, which ones we're testing
4. **Argument 1: Roster Needs** — The Bills case study, need-adjusted team rankings
5. **Argument 2: Draft Class Depth** — Not all years are equal, heatmap of class quality
6. **Argument 3: Blue-Chip Prospects** — When the talent justifies the pick, crossover analysis
7. **The Love Question Revisited** — Synthesize arguments 2+3: Love as blue chip in thin class for a team with RB need. Does the model still say no?
8. **Argument 4: Market Inefficiency** — The return metric isn't measuring what we think
9. **Revised Framework** — A decision tree: when to follow the Sharpe ratio vs when to override it
10. **What We Still Believe** — Reaffirm what survived the stress test (the core finding probably holds in aggregate, it's the edge cases that matter)

## Data Pipeline Extension

New R script: `data_pipeline_v2.R` that:
- Sources Article 1's pipeline outputs (parquet/CSV)
- Adds roster + contract expiry data for need analysis
- Computes class-level metrics (depth, hit rate by year × position)
- Computes `car_av` percentiles for blue-chip classification
- Extends snap windows for post-contract market efficiency analysis
- Exports new CSVs to `output/v2/`

## Technical Notes

- Same R Markdown → HTML → GitHub Pages deployment
- New file: `counter_analysis.Rmd` (separate page, linked from Article 1)
- Reuse Article 1's position colors, tier definitions, chart styling
- The parquet from Article 1 needs to be regenerated (not in repo). Either re-run `data_pipeline.R` or work from the CSVs + re-pull raw data via nflreadr.
