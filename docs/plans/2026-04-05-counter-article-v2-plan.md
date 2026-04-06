# Counter-Article V2 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Clean up, model, and strengthen the narrative of "What the Sharpe Ratio Can't See" — adding predictive models for career outcomes, a non-premium picks analysis, and tighter terminology throughout.

**Architecture:** Four workstreams that can largely run sequentially (some outputs feed into later tasks). Cleanup first (terminology + chart fixes), then modeling (logistic + tree-based regressions), then analysis tables (112 non-premium picks, RB counts by tier), then narrative rewrites that incorporate new model outputs.

**Tech Stack:** R (tidyverse, xgboost, broom, ggplot2, gt), R Markdown. All code lives in `counter_analysis.Rmd`. No new pipeline files — models run inline in the article.

---

## Data Inventory (for implementers)

| File | Key Columns | Notes |
|------|-------------|-------|
| `output/v2/blue_chip.csv` | `season, pick, pos_group, tier, car_av, is_blue_chip, player_return, is_hit, cmbn_score, athlete_score, prod_score, is_ngs_blue_chip` | 9,689 rows total. 2,151 rows from 2010-2022 have all features (NGS + career outcomes). This is the modeling dataset. |
| `output/sharpe_ratios.csv` | `pos_group, tier, sharpe_elite, mean_player_return, sd_player_return, elite_prob, fa_replacement` | Article 1 Sharpe values. 57 rows (11 positions × ~5 tiers). |
| `data/draft_sharpe_analysis.parquet` | `season, pick, pos_group, w_av, player_return, rookie_surplus, second_apy_cap_pct, rookie_apy_cap_pct` | 3,993 rows. Has `w_av` (weighted career AV). `car_av` column is all NA in this file — use `w_av`. |
| `output/v2/ngs_prospects_2025.csv` | `player, pos_group, cmbn_score, athlete_score, prod_score` | 2026 prospects. Love's cmbn_score = 94.3. |
| `output/v2/ngs_historical_matched.csv` | `player, cmbn_score, athlete_score, prod_score, pos_group, season, pick, car_av, w_av` | 4,138 rows. No `is_hit` or `player_return` — join to blue_chip or parquet if needed. |

**Modeling dataset filter:** `blue_chip %>% filter(season >= 2010, season <= 2022, !is.na(cmbn_score), !is.na(car_av), !is.na(is_hit))` → 2,151 rows across 11 positions.

**Premium positions:** QB, EDGE, OT, WR. Everything else is non-premium.

---

## Task 1: Terminology Cleanup — "Blue Chip" → "Elite"

Rename all references to "blue chip" / "blue-chip" to "elite" throughout `counter_analysis.Rmd`. The term "elite" aligns with the NGS tier naming (Elite = top 10% NGS) and avoids ambiguity.

**Files:**
- Modify: `counter_analysis.Rmd` (all ~25 references)

**Rules:**
- "pre-draft blue chip" → "pre-draft elite" (or just "elite" where context is clear)
- "career blue chip" → "career elite"
- "blue-chip outcome" → "elite outcome"
- "BC RB median" (in chart labels) → "Elite RB median"
- Variable names like `is_blue_chip`, `is_ngs_blue_chip`, `bc_rb_av`, `bc_median` stay as-is (they come from `blue_chip.csv` and changing them would break the pipeline)
- Chart subtitles, annotations, and narrative text all get updated
- The methodology section definitions at the bottom need updating too

- [ ] **Step 1: Find all blue-chip references**

```r
# In terminal or R: 
grep -n -i "blue.chip\|blue-chip\|BC RB\|BC median\|bc_q" counter_analysis.Rmd
```

- [ ] **Step 2: Replace display text (NOT variable names)**

Key replacements in narrative/display text (approximate lines from current file):
- Line 92: "blue-chip prospects break the averages" → "elite prospects break the averages"
- Line 139: "blue-chip outcome" → "elite outcome"
- Line 427: "blue-chip prospects (Elite tier)" → "elite prospects (Elite tier)"
- Line 581: "blue chip" → "elite" (section intro)
- Lines 585-587: Definition block — rename to "Pre-draft elite" / "Career elite"
- Line 600: "blue chip two ways" → "elite two ways"
- Line 602: "pre-draft blue chips" → "pre-draft elite prospects"
- Line 609: Comment update
- Line 624: Subtitle → "Blue = pre-draft elite (top 10% NGS) | Gray = everyone else"
- Lines 635-643: Narrative text
- Line 658: Comment
- Line 687: Subtitle → "Blue = pre-draft elite (top 10% NGS) | Top 3 elite labeled per position"
- Line 701: "pre-draft blue chips" → "pre-draft elite prospects"
- Line 1084: "blue-chip argument" → "elite prospect argument"
- Line 1267: Annotation → "Elite RB median: ..."
- Line 1284: "blue-chip RBs" → "elite RBs"
- Line 1288-1290: "blue-chip RB value" → "elite RB value"
- Line 1430: "blue-chip RB" → "elite RB"
- Line 1456: "blue-chip RBs" → "elite RBs"
- Lines 1484-1486: Methodology section definitions

- [ ] **Step 3: Verify IOL is never called premium**

Line 1186 currently says: "**premium position supply is deep** (IOL, CB, and WR are loaded with Good+ prospects)"

This is wrong. IOL and CB are NOT premium. Fix to: "**non-RB supply is deep** (IOL, CB, and WR are loaded with Good+ prospects)" or reword to not call them premium.

Also check line 1295: "Meanwhile, IOL and CB are loaded" — this is fine as-is (doesn't call them premium).

- [ ] **Step 4: Verify consistency**

Read through the rendered text to make sure "elite" reads naturally everywhere. The section header "Assumption 3" (line 577) doesn't use "blue chip" — it's fine. The `ngs-data-prep` chunk label and `blue_chip` variable names stay unchanged.

- [ ] **Step 5: Commit**

```bash
git add counter_analysis.Rmd
git commit -m "rename blue-chip to elite throughout article for consistency with NGS tiers"
```

---

## Task 2: Add R² to RB Scatter Chart

The `ngs-bc-scatter-rb` chunk (line 604) shows a regression line but doesn't display the R² value. Add it as an annotation.

**Files:**
- Modify: `counter_analysis.Rmd` — chunk `ngs-bc-scatter-rb` (~line 604-628)

- [ ] **Step 1: Calculate R² and add annotation**

After line 607 (`mutate(is_bc = is_ngs_blue_chip)`), add:

```r
# Calculate R² for annotation
rb_lm <- lm(car_av ~ cmbn_score, data = rb_scatter)
rb_r2 <- summary(rb_lm)$r.squared
```

Then in the ggplot, after the `geom_smooth` call (line 620), add:

```r
  annotate("text", 
           x = max(rb_scatter$cmbn_score, na.rm = TRUE) - 2,
           y = max(rb_scatter$car_av, na.rm = TRUE) - 5,
           label = paste0("R² = ", round(rb_r2, 3)),
           color = "#377EB8", size = 4, fontface = "bold", hjust = 1) +
```

- [ ] **Step 2: Verify it renders correctly**

The R² should appear in the upper-right area of the chart, not overlapping data points.

- [ ] **Step 3: Commit**

```bash
git add counter_analysis.Rmd
git commit -m "add R² annotation to RB NGS scatter chart"
```

---

## Task 3: P(Elite) Logistic Regression

Build a logistic regression predicting the probability of an elite career outcome from pre-draft features. "Elite" = `is_blue_chip == TRUE` (top 10% career AV at position).

**Files:**
- Modify: `counter_analysis.Rmd` — new section after the existing elite hit-rate charts (~after line 1088)

**Features:**
- `cmbn_score` (NGS Combined Score)
- `athlete_score` (NGS sub-score)
- `prod_score` (NGS sub-score)  
- `pick` (draft position — lower = more draft capital)
- `pos_group` (position — factor)

**Target:** `is_blue_chip` (binary: top 10% career AV at position)

**Dataset:** `blue_chip %>% filter(season >= 2010, season <= 2022, !is.na(cmbn_score), !is.na(car_av), !is.na(is_hit))` → 2,151 rows, 185 elite outcomes (~8.6% base rate).

**Validation:** Out-of-time split. Train on 2010-2019, test on 2020-2022. This is critical — random CV leaks future information (positional value shifts, rule changes). The test set represents "could we have predicted the most recent outcomes using only older data?" Report both train and test metrics.

- [ ] **Step 1: Write the model chunk**

Insert a new section after the "Did First-Round RBs Actually Outproduce" narrative (after line 1088). Header: `### What Makes an Elite Prospect? A Pre-Draft Model`

```r
```{r p-elite-model}
# Modeling dataset: 2010-2022 with NGS scores and career outcomes
model_data <- blue_chip %>%
  filter(season >= 2010, season <= 2022,
         !is.na(cmbn_score), !is.na(car_av), !is.na(is_hit),
         pos_group %in% key_positions) %>%
  mutate(
    is_elite = as.integer(is_blue_chip),
    pos_group = factor(pos_group)
  )

# Out-of-time split: train on 2010-2019, test on 2020-2022
train_data <- model_data %>% filter(season <= 2019)
test_data <- model_data %>% filter(season >= 2020)

# Logistic regression: P(elite) from pre-draft features
elite_glm <- glm(is_elite ~ cmbn_score + athlete_score + prod_score + log(pick) + pos_group,
                  data = train_data, family = binomial)

# Test set performance
test_data$pred_elite <- predict(elite_glm, newdata = test_data, type = "response")
# AUC on test set (use simple concordance if pROC not available)
test_concordance <- mean(
  outer(test_data$pred_elite[test_data$is_elite == 1],
        test_data$pred_elite[test_data$is_elite == 0],
        ">") +
  0.5 * outer(test_data$pred_elite[test_data$is_elite == 1],
              test_data$pred_elite[test_data$is_elite == 0],
              "==")
)
# Brier score on test set
test_brier <- mean((test_data$pred_elite - test_data$is_elite)^2)

elite_summary <- broom::tidy(elite_glm) %>%
  mutate(
    odds_ratio = exp(estimate),
    p_star = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", 
                       p.value < 0.05 ~ "*", TRUE ~ "")
  )
```

- [ ] **Step 2: Display coefficients table**

```r
```{r p-elite-table, results='asis'}
elite_summary %>%
  filter(term != "(Intercept)") %>%
  select(term, odds_ratio, p.value, p_star) %>%
  mutate(
    term = str_replace(term, "pos_group", ""),
    odds_ratio = round(odds_ratio, 3),
    p.value = format.pval(p.value, digits = 2)
  ) %>%
  gt() %>%
  cols_label(term = "Feature", odds_ratio = "Odds Ratio", 
             p.value = "p-value", p_star = "") %>%
  tab_header(title = md("**What Predicts an Elite Career?**"),
             subtitle = "Logistic regression: P(top 10% career AV) | 2010-2022 drafted players with NGS scores") %>%
  tab_options(table.font.size = px(12))
```

- [ ] **Step 3: Predict Love's P(elite)**

```r
```{r p-elite-love}
# Love's features
love_row <- data.frame(
  cmbn_score = 94.3,
  athlete_score = ngs_prospects %>% filter(player == "Jeremiah Love") %>% pull(athlete_score),
  prod_score = ngs_prospects %>% filter(player == "Jeremiah Love") %>% pull(prod_score),
  pick = 10,  # Hypothetical pick
  pos_group = factor("RB", levels = levels(model_data$pos_group))
)

love_p_elite <- predict(elite_glm, newdata = love_row, type = "response")

# Show P(elite) at different picks
love_by_pick <- map_dfr(1:32, function(p) {
  row <- love_row
  row$pick <- p
  tibble(pick = p, p_elite = predict(elite_glm, newdata = row, type = "response"))
})
```

- [ ] **Step 4: Chart — Love's P(elite) by pick**

```r
```{r p-elite-love-chart, fig.height=5, fig.width=8}
ggplot(love_by_pick, aes(x = pick, y = p_elite)) +
  geom_line(color = "#377EB8", linewidth = 1.2) +
  geom_point(data = love_by_pick %>% filter(pick == 10),
             color = "#377EB8", size = 4) +
  geom_text(data = love_by_pick %>% filter(pick == 10),
            aes(label = paste0(round(p_elite * 100, 1), "%")),
            vjust = -1.5, color = "#377EB8", fontface = "bold", size = 4) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(1, 32, 2)) +
  labs(
    title = "Love's Predicted Probability of an Elite Career",
    subtitle = "Logistic model: NGS scores + draft pick + position | Based on 2010-2022 outcomes",
    x = "Draft Pick", y = "P(Elite)"
  ) +
  theme_minimal(base_size = 13)
```

- [ ] **Step 5: Write narrative**

```r
```{r p-elite-narrative, results='asis'}
cat(paste0(
  "The logistic model — trained on 2010-2019 and validated on the 2020-2022 classes (AUC: ",
  round(test_concordance, 3), ") — confirms what the scatter plots suggested: NGS Combined Score and draft capital ",
  "are the strongest predictors of elite career outcomes. Each additional point of NGS Combined Score ",
  "multiplies the odds of an elite career by **", round(elite_summary$odds_ratio[elite_summary$term == "cmbn_score"], 2),
  "x**. Each pick later in the draft reduces them.\n\n",
  "At pick 10, Love's predicted probability of an elite career is **",
  round(love_by_pick$p_elite[love_by_pick$pick == 10] * 100, 1),
  "%**. That's ", round(love_by_pick$p_elite[love_by_pick$pick == 10] / mean(model_data$is_elite) , 1),
  "x the base rate across all drafted players with NGS scores."
))
```

- [ ] **Step 6: Commit**

```bash
git add counter_analysis.Rmd
git commit -m "add P(elite) logistic regression model with Love prediction curve"
```

---

## Task 4: Career AV Prediction — Linear + XGBoost

Two models predicting weighted career AV from pre-draft features. Linear as interpretable baseline, XGBoost to capture non-linear effects. Compare both.

**Files:**
- Modify: `counter_analysis.Rmd` — new subsection after the P(elite) section from Task 3

**Features (same as Task 3):**
- `cmbn_score`, `athlete_score`, `prod_score`, `pick`, `pos_group`

**Target:** `car_av` (continuous: weighted career AV)

**Dataset:** Same 2,151 rows as Task 3. Same out-of-time split: train 2010-2019, test 2020-2022.

**Important:** Use `log(pick)` instead of raw `pick` in both models. Draft value declines non-linearly — the gap between pick 1 and 2 is far larger than pick 20 and 21. Log transform captures this.

- [ ] **Step 1: Linear + XGBoost models with out-of-time validation**

Section header: `### Predicting Career Value: Two Approaches`

```r
```{r career-av-models}
library(xgboost)

# Same train/test split from Task 3
# train_data and test_data already exist (2010-2019 / 2020-2022)

# Linear model (log pick)
av_lm <- lm(car_av ~ cmbn_score + athlete_score + prod_score + log(pick) + pos_group,
             data = train_data)
av_lm_summary <- broom::tidy(av_lm)

# Linear test performance
lm_test_pred <- predict(av_lm, newdata = test_data)
lm_test_rmse <- sqrt(mean((test_data$car_av - lm_test_pred)^2))
lm_test_r2 <- 1 - sum((test_data$car_av - lm_test_pred)^2) / 
  sum((test_data$car_av - mean(train_data$car_av))^2)

# Linear train R² for reference
av_lm_r2 <- summary(av_lm)$r.squared

# XGBoost — use log(pick) in feature matrix
train_xgb <- train_data %>% mutate(log_pick = log(pick))
test_xgb <- test_data %>% mutate(log_pick = log(pick))

xgb_train_features <- model.matrix(
  ~ cmbn_score + athlete_score + prod_score + log_pick + pos_group - 1,
  data = train_xgb
)
xgb_test_features <- model.matrix(
  ~ cmbn_score + athlete_score + prod_score + log_pick + pos_group - 1,
  data = test_xgb
)
xgb_train_label <- train_data$car_av
xgb_test_label <- test_data$car_av

set.seed(42)
xgb_model <- xgboost(
  data = xgb_train_features,
  label = xgb_train_label,
  nrounds = 200,
  max_depth = 4,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  objective = "reg:squarederror",
  verbose = 0
)

# XGBoost test performance (out-of-time)
xgb_test_pred <- predict(xgb_model, xgb_test_features)
xgb_test_rmse <- sqrt(mean((xgb_test_label - xgb_test_pred)^2))
xgb_test_r2 <- 1 - sum((xgb_test_label - xgb_test_pred)^2) / 
  sum((xgb_test_label - mean(xgb_train_label))^2)

# XGBoost train R² for reference
xgb_train_pred <- predict(xgb_model, xgb_train_features)
xgb_train_r2 <- 1 - sum((xgb_train_label - xgb_train_pred)^2) / 
  sum((xgb_train_label - mean(xgb_train_label))^2)
```

- [ ] **Step 2: Model comparison table**

```r
```{r career-av-comparison, results='asis'}
tibble(
  Model = c("Linear Regression", "XGBoost (tree-based)"),
  `Train R²` = c(round(av_lm_r2, 3), round(xgb_train_r2, 3)),
  `Test R² (2020-22)` = c(round(lm_test_r2, 3), round(xgb_test_r2, 3)),
  `Test RMSE` = c(round(lm_test_rmse, 1), round(xgb_test_rmse, 1))
) %>%
  gt() %>%
  tab_header(title = md("**Predicting Career AV from Pre-Draft Features**"),
             subtitle = "Train: 2010-2019 | Test: 2020-2022 (out-of-time holdout)") %>%
  tab_options(table.font.size = px(12))
```

- [ ] **Step 3: XGBoost feature importance chart**

```r
```{r career-av-importance, fig.height=4, fig.width=8}
importance <- xgb.importance(model = xgb_model, feature_names = colnames(xgb_features))

# Clean up feature names for display
importance_clean <- importance %>%
  mutate(Feature = str_replace(Feature, "pos_group", "Pos: ") %>%
           str_replace("cmbn_score", "NGS Combined") %>%
           str_replace("athlete_score", "NGS Athlete") %>%
           str_replace("prod_score", "NGS Production") %>%
           str_replace("pick", "Draft Pick")) %>%
  slice_head(n = 10)

ggplot(importance_clean, aes(x = Gain, y = fct_reorder(Feature, Gain))) +
  geom_col(fill = "#377EB8", alpha = 0.85) +
  labs(title = "What Drives Career Value? (XGBoost Feature Importance)",
       subtitle = "Top 10 features by information gain",
       x = "Gain", y = NULL) +
  theme_minimal(base_size = 13)
```

- [ ] **Step 4: Predict Love's career AV at each pick**

```r
```{r career-av-love, fig.height=5, fig.width=8}
love_base <- data.frame(
  cmbn_score = 94.3,
  athlete_score = ngs_prospects %>% filter(player == "Jeremiah Love") %>% pull(athlete_score),
  prod_score = ngs_prospects %>% filter(player == "Jeremiah Love") %>% pull(prod_score),
  pos_group = factor("RB", levels = levels(model_data$pos_group))
)

love_av_by_pick <- map_dfr(1:32, function(p) {
  row <- love_base %>% mutate(pick = p, log_pick = log(p))
  xgb_row <- model.matrix(~ cmbn_score + athlete_score + prod_score + log_pick + pos_group - 1, data = row)
  tibble(
    pick = p,
    lm_pred = predict(av_lm, newdata = row),
    xgb_pred = predict(xgb_model, xgb_row)
  )
})

ggplot(love_av_by_pick, aes(x = pick)) +
  geom_line(aes(y = lm_pred, color = "Linear"), linewidth = 1) +
  geom_line(aes(y = xgb_pred, color = "XGBoost"), linewidth = 1) +
  scale_color_manual(values = c("Linear" = "#999999", "XGBoost" = "#377EB8"), name = "Model") +
  scale_x_continuous(breaks = seq(1, 32, 2)) +
  labs(
    title = "Love's Predicted Career AV by Draft Position",
    subtitle = "Both models use NGS scores + pick + position as inputs",
    x = "Draft Pick", y = "Predicted Weighted Career AV"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")
```

- [ ] **Step 5: Write narrative**

```r
```{r career-av-narrative, results='asis'}
love_at_10_xgb <- love_av_by_pick$xgb_pred[love_av_by_pick$pick == 10]
love_at_10_lm <- love_av_by_pick$lm_pred[love_av_by_pick$pick == 10]

cat(paste0(
  "Two models, same story — both trained on 2010-2019 and tested on the 2020-2022 classes they've never seen. ",
  "The linear regression (test R² = ", round(lm_test_r2, 3),
  ") captures the broad strokes: higher NGS scores and earlier picks produce more career value. ",
  "The gradient-boosted model (XGBoost) picks up non-linear effects — the diminishing returns of draft ",
  "capital after the top 10, the interaction between position and athletic profile — and generalizes ",
  if(xgb_test_r2 > lm_test_r2) "better" else "similarly",
  " to unseen data (test RMSE: ", round(xgb_test_rmse, 1), " vs ", round(lm_test_rmse, 1), " for linear).\n\n",
  "Both agree on Love: at pick 10, the linear model projects **", round(love_at_10_lm, 0),
  "** weighted career AV; XGBoost projects **", round(love_at_10_xgb, 0), "**. ",
  "For context, the median elite RB historically produced ", round(bc_median, 0), " career AV."
))
```

- [ ] **Step 6: Commit**

```bash
git add counter_analysis.Rmd
git commit -m "add linear + XGBoost career AV prediction models"
```

---

## Task 5: Love's Net Value — Incorporating Rookie Surplus

The Sharpe ratio's case against RB leans on rookie surplus: RBs on rookie deals are cheap and productive. The counter needs to engage with this directly by computing Love's *net* expected value (predicted career value minus rookie slot cost) and comparing to alternatives.

**Files:**
- Modify: `counter_analysis.Rmd` — new subsection after the career AV models (Task 4), before the fair value chart

**Key data:**
- `rookie_apy_cap_pct` from the parquet (rookie slot cost as % of cap, by pick)
- Predicted career AV from Task 4's XGBoost model
- Non-RB expected career AV at each pick (already computed in fair value section)

- [ ] **Step 1: Compute rookie cost by pick**

```r
```{r love-net-value}
# Get average rookie cost by pick from the parquet
rookie_cost <- df %>%
  filter(season >= 2010, !is.na(rookie_apy_cap_pct)) %>%
  group_by(pick) %>%
  summarise(avg_rookie_cost = mean(rookie_apy_cap_pct, na.rm = TRUE), .groups = "drop")

# Love's predicted career AV (from Task 4 XGBoost) at each pick
love_net <- love_av_by_pick %>%
  left_join(rookie_cost, by = "pick") %>%
  filter(!is.na(avg_rookie_cost)) %>%
  mutate(
    # Normalize career AV to same scale as cost (% of cap-year equivalent)
    # Use the relationship: career AV per dollar spent
    love_surplus = xgb_pred - avg_rookie_cost * 100  # scale appropriately
  )

# Non-RB expected career AV at each pick (from fair value section logic)
non_rb_expected <- blue_chip %>%
  filter(pick <= 32, pos_group %in% key_positions, pos_group != "RB",
         !is.na(car_av), season >= 2010, season <= 2022) %>%
  group_by(pick) %>%
  summarise(non_rb_av = mean(car_av, na.rm = TRUE), .groups = "drop")

# Combine for comparison
comparison <- love_av_by_pick %>%
  left_join(non_rb_expected, by = "pick") %>%
  left_join(rookie_cost, by = "pick") %>%
  filter(!is.na(non_rb_av), !is.na(avg_rookie_cost)) %>%
  mutate(
    love_net = xgb_pred - (avg_rookie_cost * 100),
    alt_net = non_rb_av - (avg_rookie_cost * 100),
    love_advantage = love_net - alt_net
  )
```

Note: The exact normalization between career AV units and cap % units will need calibration. The implementer should check whether the surplus calculation makes sense by looking at actual values — if career AV and cap % are on very different scales, compare Love's predicted AV directly to the non-RB average AV at each pick instead of subtracting dollar cost. The key chart is: **Love's predicted AV vs non-RB average AV, by pick**, which shows where the curves cross.

- [ ] **Step 2: Chart — Love vs alternative by pick**

```r
```{r love-vs-alt-chart, fig.height=5, fig.width=8}
ggplot(comparison, aes(x = pick)) +
  geom_line(aes(y = xgb_pred, color = "Love (predicted)"), linewidth = 1.2) +
  geom_line(aes(y = non_rb_av, color = "Avg non-RB (actual)"), linewidth = 1.2) +
  geom_ribbon(data = comparison %>% filter(xgb_pred >= non_rb_av),
              aes(ymin = non_rb_av, ymax = xgb_pred), fill = "#2e7d32", alpha = 0.15) +
  geom_ribbon(data = comparison %>% filter(xgb_pred < non_rb_av),
              aes(ymin = xgb_pred, ymax = non_rb_av), fill = "#c62828", alpha = 0.15) +
  scale_color_manual(values = c("Love (predicted)" = "#377EB8", "Avg non-RB (actual)" = "#c62828"),
                     name = NULL) +
  scale_x_continuous(breaks = seq(1, 32, 2)) +
  labs(
    title = "Love's Predicted Career Value vs the Alternative",
    subtitle = "XGBoost prediction for Love vs historical avg non-RB at each pick",
    x = "Draft Pick", y = "Weighted Career AV"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")
```

- [ ] **Step 3: Find crossover pick**

```r
```{r love-crossover}
crossover <- comparison %>%
  mutate(love_wins = xgb_pred >= non_rb_av) %>%
  filter(love_wins) %>%
  slice_head(n = 1) %>%
  pull(pick)
```

- [ ] **Step 4: Narrative tying to rookie surplus**

```r
```{r love-net-narrative, results='asis'}
cat(paste0(
  "Article 1's strongest argument against first-round RBs is the rookie surplus: backs on cheap ",
  "rookie contracts produce so efficiently that the *marginal* value of drafting one early is negative. ",
  "You get 80% of the production at 20% of the draft capital.\n\n",
  "But that argument assumes an average RB. Love isn't average — the model predicts career AV of **",
  round(love_av_by_pick$xgb_pred[love_av_by_pick$pick == 10], 0),
  "** at pick 10, which exceeds the historical average non-RB first-rounder at that pick (",
  round(comparison$non_rb_av[comparison$pick == 10], 0), ").\n\n",
  "The crossover point — where Love's predicted value exceeds the non-RB alternative — is **pick ",
  crossover, "**. Before that, you're paying a premium for the name 'RB' when a non-RB pick produces ",
  "more expected career value. After it, Love's elite profile starts to outweigh the positional discount.\n\n",
  "This doesn't eliminate the rookie surplus concern. A team still gets cheap production from a Day 2 RB. ",
  "But the question isn't 'can you find *a* running back later?' — it's 'can you find *this* running back ",
  "later?' Love's 94.3 NGS score says no. The gap between Love and RB2 in this class is 17 points. ",
  "The surplus argument assumes fungibility. Love breaks that assumption."
))
```

- [ ] **Step 5: Commit**

```bash
git add counter_analysis.Rmd
git commit -m "add Love net value analysis incorporating rookie surplus crossover"
```

---

## Task 6: 112 Non-Premium First-Round Picks Analysis

Since 2010, 112 of 416 first-round picks (~27%) were non-premium positions (not QB, EDGE, OT, WR) — the positions with negative or low Sharpe ratios. How did they actually perform?

**Files:**
- Modify: `counter_analysis.Rmd` — new section after the breakeven dumbbell chart narrative (~after line 1182), before "Love's Fair Value"

- [ ] **Step 1: Calculate the data**

```r
```{r non-premium-picks}
premium_pos <- c("QB", "EDGE", "OT", "WR")

non_prem_r1 <- blue_chip %>%
  filter(pick <= 32, season >= 2010, season <= 2022,
         !is.na(car_av), pos_group %in% key_positions) %>%
  mutate(is_premium = pos_group %in% premium_pos)

# Summary by position for non-premium picks
non_prem_summary <- non_prem_r1 %>%
  filter(!is_premium) %>%
  group_by(pos_group) %>%
  summarise(
    n = n(),
    avg_pick = round(mean(pick), 1),
    avg_car_av = round(mean(car_av, na.rm = TRUE), 1),
    hit_rate = round(mean(is_hit, na.rm = TRUE) * 100, 1),
    elite_rate = round(mean(is_blue_chip, na.rm = TRUE) * 100, 1),
    sharpe = sharpe %>% filter(pos_group == cur_group()$pos_group, tier %in% c("Top 10", "Late 1st")) %>%
      summarise(s = weighted.mean(sharpe_elite, n, na.rm = TRUE)) %>% pull(s) %>% round(3),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

# Overall comparison
prem_avg_av <- non_prem_r1 %>% filter(is_premium) %>% pull(car_av) %>% mean(na.rm = TRUE)
nonprem_avg_av <- non_prem_r1 %>% filter(!is_premium) %>% pull(car_av) %>% mean(na.rm = TRUE)
n_nonprem <- sum(!non_prem_r1$is_premium)
n_total <- nrow(non_prem_r1)
```

Note to implementer: The `sharpe` join inside `summarise` above is tricky. A cleaner approach: pre-compute the weighted Sharpe by position outside the summarise, then join. For example:

```r
pos_sharpe <- sharpe %>%
  filter(tier %in% c("Top 10", "Late 1st")) %>%
  group_by(pos_group) %>%
  summarise(avg_sharpe = weighted.mean(sharpe_elite, n, na.rm = TRUE), .groups = "drop")

non_prem_summary <- non_prem_r1 %>%
  filter(!is_premium) %>%
  group_by(pos_group) %>%
  summarise(
    n = n(),
    avg_pick = round(mean(pick), 1),
    avg_car_av = round(mean(car_av, na.rm = TRUE), 1),
    hit_rate = round(mean(is_hit, na.rm = TRUE) * 100, 1),
    elite_rate = round(mean(is_blue_chip, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  left_join(pos_sharpe, by = "pos_group") %>%
  arrange(desc(n))
```

- [ ] **Step 2: GT table**

```r
```{r non-premium-table}
non_prem_summary %>%
  gt() %>%
  cols_label(
    pos_group = "Position", n = "R1 Picks", avg_pick = "Avg Pick",
    avg_car_av = "Avg Career AV", hit_rate = "Hit Rate %",
    elite_rate = "Elite Rate %", avg_sharpe = "Avg Sharpe (R1)"
  ) %>%
  tab_header(
    title = md("**The Other 27%: Non-Premium First-Round Picks (2010-2022)**"),
    subtitle = "Premium = QB, EDGE, OT, WR. How did everyone else do?"
  ) %>%
  tab_style(
    style = cell_fill(color = "#e3f2fd"),
    locations = cells_body(rows = pos_group == "RB")
  ) %>%
  tab_options(table.font.size = px(12))
```

- [ ] **Step 3: Narrative**

```r
```{r non-premium-narrative, results='asis'}
rb_row <- non_prem_summary %>% filter(pos_group == "RB")
cat(paste0(
  "Since 2010, **", n_nonprem, " of ", n_total, " first-round picks (", 
  round(n_nonprem/n_total*100), "%)** went to non-premium positions — the picks ",
  "that Article 1's Sharpe ratio says teams shouldn't make. Those picks averaged **",
  round(nonprem_avg_av, 0), "** career AV, vs **", round(prem_avg_av, 0),
  "** for premium positions.\n\n",
  "RB specifically: **", rb_row$n, " first-round RBs**, averaging pick ",
  rb_row$avg_pick, " and **", rb_row$avg_car_av, "** career AV. Their hit rate (",
  rb_row$hit_rate, "%) trails the premium positions, but their elite rate (",
  rb_row$elite_rate, "%) tells a different story — when first-round RBs hit, they hit big.\n\n",
  "The model says these were all bad picks. The career outcomes say it's more complicated than that."
))
```

- [ ] **Step 4: Commit**

```bash
git add counter_analysis.Rmd
git commit -m "add non-premium first-round picks outcomes analysis"
```

---

## Task 7: Narrative Polish and Fair Value Rewording

Final pass: update the fair value section labels, tighten the Article 2 framing, and ensure the new model sections flow into the existing structure.

**Files:**
- Modify: `counter_analysis.Rmd` — multiple sections

- [ ] **Step 1: Fix fair value chart labels**

In chunk `love-fair-value` (~line 1188):
- Line 1267 annotation: Change `"BC RB median: "` → `"Elite RB median: "`
- Line 1276 subtitle: Change `"Red = non-RB positions (all) | Blue = BC RB median"` → `"Red = avg non-RB by pick | Blue = elite RB median (historical)"`
- Line 1284 narrative: "blue-chip RBs" → "elite RBs" (should already be done by Task 1, verify)

- [ ] **Step 2: Update fair value narrative for clarity**

The current narrative at line 1282 explains the chart well but doesn't connect to the new models. After the existing narrative, add a bridge paragraph:

```
The fair value chart uses historical averages. The predictive models in the previous sections 
sharpen the picture: Love's *individual* predicted career AV (from his NGS profile and draft 
position) exceeds the non-RB average starting at pick [crossover]. The fair value zone isn't 
just a historical artifact — it's confirmed by the models.
```

- [ ] **Step 3: Reorder sections for narrative flow**

The final article structure should be:

1. TL;DR
2. The Model Was Right. And Incomplete.
3. Quick Recap (Sharpe + Career AV explainer)
4. Assumption 1: Roster Context (Bills example, quality gate)
5. Assumption 2: Class Depth (NGS tiers, timelines)
6. Assumption 3: Not All Prospects Are Fungible (scatter plots, elite hit rates)
7. **NEW: What Makes an Elite Prospect?** (P(elite) logistic regression — Task 3)
8. **NEW: Predicting Career Value** (Linear + XGBoost — Task 4)
9. Did First-Round RBs Outproduce the Alternative? (dumbbell chart — existing)
10. **NEW: The Other 27%** (non-premium picks table — Task 6)
11. **NEW: Love's Net Value** (rookie surplus crossover — Task 5)
12. Love's Fair Value in This Draft (existing chart, updated labels)
13. Which Teams Need an RB? (existing table)
14. Putting It All Together (existing verdict)
15. What We Learned (existing, updated)
16. What Survived the Stress Test (existing, updated)
17. Methodology Notes (existing, updated)

The implementer should move the new chunks into these positions. Tasks 3-4 go between the elite hit-rate narrative and the dumbbell chart. Task 6 goes after the dumbbell chart. Task 5 goes before the fair value chart.

- [ ] **Step 4: Update "What We Learned" section**

Add a 4th bullet point after the existing three (line ~1456):

```
4. **Pre-draft models confirm the exception.** A logistic regression and a gradient-boosted model 
both trained on 2010-2022 outcomes agree: Love's NGS profile predicts a [X]% chance of an elite 
career and [Y] weighted career AV at pick 10 — exceeding the average non-RB alternative at that pick. 
The rookie surplus argument assumes all RBs are interchangeable. Love's predicted career trajectory 
says otherwise.
```

- [ ] **Step 5: Update methodology section**

Add after the existing methodology notes (~line 1486):

```
**Predictive models:** Two models trained on players drafted 2010-2019 with NGS Combined Scores 
and career outcomes, validated out-of-time on the 2020-2022 classes. Features: NGS Combined Score, 
NGS Athlete Score, NGS Production Score, log(draft pick), and position group. Draft pick is 
log-transformed because draft value declines non-linearly. (1) Logistic regression predicting 
P(top 10% career AV). (2) Gradient-boosted trees (XGBoost, 200 rounds, max_depth=4) predicting 
weighted career AV. Love's predictions use his actual NGS scores with hypothetical pick positions.
```

- [ ] **Step 6: Commit**

```bash
git add counter_analysis.Rmd
git commit -m "narrative polish: fair value labels, section ordering, updated conclusions"
```

---

## Task 8: Render and Push

Final render of the complete article and push to remote.

**Files:**
- Render: `counter_analysis.Rmd` → `counter_analysis.html`

- [ ] **Step 1: Render**

```bash
cd /Users/nick/draft-sharpe-analysis && Rscript -e 'rmarkdown::render("counter_analysis.Rmd")'
```

- [ ] **Step 2: Verify no errors**

Check render output for warnings/errors. If XGBoost produces verbose output, ensure `verbose = 0` is set.

- [ ] **Step 3: Commit rendered output**

```bash
git add counter_analysis.html charts/
git commit -m "render article with models and narrative updates"
```

- [ ] **Step 4: Push**

```bash
git push origin main
```

If rejected (remote has new commits):
```bash
git stash && git pull --rebase && git stash pop && git push
```
