# Methodological Peer Review: Commodity Price Bubble Detection via Machine Learning

**Scope:** Evaluation of modeling design, validation strategy, and statistical assumptions in three supervised-learning scripts predicting binary bubble regimes for lithium, copper, cobalt, and nickel.

**Files reviewed:** `R/metals31_RF.R` (Random Forest), `R/metals32_lightGBM.R` (LightGBM), `R/metals33_Lasso.R` (Logistic LASSO)

---

## 1. Top Methodological Risks (Ranked by Severity)

### RISK 1 — Random K-Fold Cross-Validation on Temporally Ordered Data (**CRITICAL**)

| File | Mechanism |
|---|---|
| `metals32_lightGBM.R` | `folds <- sample(rep(1:n_folds, length.out = nrow(X)))` — random shuffle (L247) |
| `metals33_Lasso.R` | `cv.glmnet(...)` with default `foldid` — random shuffle by default (L509, L373) |
| `metals31_RF.R` | OOB predictions from `ranger` — bootstrap sampling ignores temporal order (L955–L960) |

**Verdict: WRONG for all three models.**

Random k-fold CV and OOB estimation assume i.i.d. observations. Time-series data violates this assumption due to autocorrelation, trend, and regime dependence. Random shuffling allows future observations to train models that are then evaluated on past observations, inflating all reported metrics (AUC, Balanced Accuracy, Sensitivity).

The correct approach is **time-series cross-validation** (expanding window or sliding window), where every training fold precedes every test fold chronologically.

- Tashman (2000), "Out-of-sample tests of forecasting accuracy: an analysis and review," *International Journal of Forecasting*, 16(4), 437–450.
- Bergmeir, Hyndman & Koo (2018), "A note on the validity of cross-validation for evaluating autoregressive time series prediction," *Computational Statistics & Data Analysis*, 120, 70–83.
- Hyndman & Athanasopoulos (2021), *Forecasting: Principles and Practice*, 3rd ed., §5.4 (Time series cross-validation).

**Impact:** All reported performance metrics are unreliable. Results are likely over-optimistic and would not replicate on genuinely out-of-sample data. **This alone is grounds for major revision.**

---

### RISK 2 — Retrospective Target Construction Using Full-Sample Bubble Detection (**CRITICAL**)

In `metals31_RF.R` L185–L186, the `radf()` function (PSY/GSADF test from the `exuber` package) is applied to the **entire** `data_matrix` (constructed at L181–L184 from the full `df_daly`). Monte Carlo critical values are then simulated at L188 using `radf_mc_cv(n = n_obs)` with the full sample size. Bubble datestamping at L193 (`datestamp(est_results, cv = mc_cv)`) assigns start/end indices using the complete-sample test statistics. These labels are then used as the supervised target for all three models.

**Verdict: WRONG.**

The PSY (Phillips, Shi & Yu, 2015) procedure is designed for **real-time sequential monitoring** using recursive right-tailed ADF statistics. When applied retrospectively to the full sample, the critical values, start/end dates, and even the existence of detected bubbles are informed by future data. This is **target leakage at the label level** — the most damaging form of look-ahead bias because it corrupts the ground truth itself.

- Phillips, Shi & Yu (2015), "Testing for multiple bubbles: Historical episodes of exuberance and collapse in the S&P 500," *International Economic Review*, 56(4), 1043–1078.
- The `exuber` package documentation explicitly notes recursive estimation is for monitoring; full-sample application is retrospective analysis only.

**Impact:** The models are not predicting whether a bubble will form; they are classifying periods that were *already labeled using the complete history*. Out-of-sample validity is fundamentally compromised.

---

### RISK 3 — No Temporal Train/Test Split (**SEVERE**)

None of the three scripts implements an explicit temporal holdout or walk-forward evaluation scheme. In all cases, the full dataset is used for both model fitting (or CV-fold construction) and evaluation.

**Verdict: WRONG.**

For publishable forecasting work, a strict temporal split — where the test set consists only of chronologically later observations never seen during training — is a minimum requirement.

- Cerqueira et al. (2020), "Evaluating time series forecasting models: An empirical study on performance estimation methods," *Machine Learning*, 109, 1997–2028.
- Roberts et al. (2017), "Cross-validation strategies for data with temporal, spatial, hierarchical, or phylogenetic structure," *Ecography*, 40(8), 913–929.

---

### RISK 4 — Global Preprocessing Before Any Split (**MODERATE**)

In `metals31_RF.R` L446–L449, `na.spline()` interpolation is applied to the **entire merged dataset** including macro variables:

```r
df_final <- df_merged_raw %>%
  mutate(across(all_of(vars_to_interpolate),
                ~ na.spline(., x = Date, method = "natural", na.rm = FALSE)))
```

Log-returns and rolling volatility (window=10) are also computed globally inside `prepare_ml_data()` (L661–L668). If a temporal split were added, test-period information would already be embedded in:

1. Spline-interpolated values near the split boundary (the cubic spline uses surrounding points).
2. Rolling volatility windows that straddle the boundary.

**Verdict: QUESTIONABLE (becomes WRONG if a temporal split is later added naively).**

Correct practice: all preprocessing (interpolation, scaling, rolling statistics) must be fit only on the training partition and applied independently to the test partition.

- Hastie, Tibshirani & Friedman (2009), *The Elements of Statistical Learning*, 2nd ed., §7.10.2 ("The Wrong and Right Way to Do Cross-Validation").

---

### RISK 5 — In-Sample Evaluation in the RF Script (**SEVERE**)

`metals31_RF.R` L955–L960:

```r
prob_bubble <- model_rf$predictions[, 2]
roc_obj <- roc(true_y, prob_bubble, quiet = TRUE)
auc_val <- as.numeric(auc(roc_obj))
```

This retrieves OOB predictions from `ranger`. While OOB provides *some* out-of-bag correction, the bootstrap resampling is random, not temporal. For temporally dependent data, OOB error is a biased estimator of true out-of-sample performance because temporally adjacent observations (which are correlated) leak across the in-bag / out-of-bag boundary.

**Verdict: WRONG for time-series.**

- Janitza & Hornung (2018), "On the overestimation of random forest's out-of-bag error," *PLOS ONE*, 13(8), e0201904.

---

### RISK 6 — No Hyperparameter Tuning for LightGBM (**MODERATE**)

`metals32_lightGBM.R` L120–L129 uses hardcoded hyperparameters:

```r
params <- list(
  objective = "binary",
  metric = "auc",
  learning_rate = 0.05,
  num_leaves = 31,
  is_unbalance = TRUE,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 5,
  verbosity = -1
)
```

No grid search, Bayesian optimization, or nested CV is performed. The same fixed params are re-used in the CV loop (L234–L243).

**Verdict: QUESTIONABLE.**

Fixed hyperparameters introduce an arbitrary modeler-degree-of-freedom. Without systematic tuning (preferably within a time-series-aware nested CV), results are neither optimal nor reproducible by a researcher who would choose different defaults. Additionally, **no early stopping** is used — `lgb.train()` at L132 runs all 200 rounds with no `valids` argument — boosting without early stopping risks overfitting.

- Probst, Boulesteix & Bischl (2019), "Tunability: Importance of hyperparameters of machine learning algorithms," *JMLR*, 20(53), 1–32.

---

### RISK 7 — Class Imbalance Treatment is Inconsistent (**MODERATE**)

| Model | Imbalance Handling |
|---|---|
| Random Forest (`metals31_RF.R` L725) | None — `ranger()` called with default settings, no `class.weights` or `sample.fraction` |
| LightGBM (`metals32_lightGBM.R` L125) | `is_unbalance = TRUE` — internal instance weighting |
| LASSO (`metals33_Lasso.R` L373, L509) | None — default `glmnet`; `type.measure = "class"` penalizes all errors equally |

**Verdict: QUESTIONABLE.**

Bubble periods are rare events (highly imbalanced). Using classification error (`type.measure = "class"`) for LASSO lambda selection will bias toward the majority class. `type.measure = "auc"` would be more appropriate. The RF script should use `class.weights` or stratified sampling. Inconsistent treatment across models undermines fair comparison.

- He & Garcia (2009), "Learning from imbalanced data," *IEEE Transactions on Knowledge and Data Engineering*, 21(9), 1263–1284.

---

## 2. Correct Methodological Choices Worth Keeping

| Practice | File(s) | Assessment |
|---|---|---|
| **Lag-1 on all features** to prevent direct look-ahead in predictors | All three (e.g., RF L679–L683; LGBM L69–L74; LASSO L79–L84) | ✅ CORRECT — standard practice for 1-step-ahead prediction. |
| **Log-returns** for price transformation | All three (`calc_safe_ret` functions) | ✅ CORRECT — addresses non-stationarity of price levels; standard in financial econometrics (Campbell, Lo & MacKinlay, 1997). |
| **10-day rolling volatility** as a feature | All three (`calc_safe_vol` with `TTR::runSD(..., n=10)`) | ✅ CORRECT — financially meaningful; captures realized volatility dynamics (Andersen et al., 2003). |
| **SHAP values** for model interpretation | RF (L735–L747), LightGBM (L141) | ✅ CORRECT — model-agnostic and theoretically grounded (Lundberg & Lee, 2017). |
| **`keep = TRUE` + `fit.preval`** in LASSO for out-of-fold predictions | `metals33_Lasso.R` L373 and L379–L380 | ✅ CORRECT within the (flawed) random-CV framework — avoids re-predicting on training data. |
| **`standardize = TRUE`** in glmnet | `metals33_Lasso.R` L373 | ✅ CORRECT — glmnet standardizes internally per fold, preventing scaling leakage. |
| **Removing VIX volatility** (VIX is already a vol index) | `metals33_Lasso.R` L73 (`select(-any_of("VIX_Vol"))`) | ✅ CORRECT — demonstrates domain knowledge; reduces feature redundancy. |
| **Peer bubble dummies** as cross-metal contagion features | All three (e.g., RF L637, L683) | ✅ INTERESTING — captures cross-commodity bubble spillovers; lagged appropriately. |
| **Using AUC and Balanced Accuracy** as metrics | All three | ✅ CORRECT — appropriate for imbalanced binary classification (Saito & Rehmsmeier, 2015). |
| **`probability = TRUE`** in ranger | `metals31_RF.R` L726 | ✅ CORRECT — enables proper probabilistic evaluation and AUC computation. |

---

## 3. Missing Diagnostics and Robustness Checks

| Missing Element | Why It Matters | Priority |
|---|---|---|
| **Stationarity tests** (ADF, KPSS) on features | Level features (e.g., `VIX_Level_Lag1`, `YIELD_10Y_Level_Lag1`) may be non-stationary; non-stationary inputs in classification can produce spurious associations. | HIGH |
| **Autocorrelation diagnostics** on residuals / prediction errors | Serial correlation in errors violates the independence assumption underlying all three CV schemes and inflates standard errors. | HIGH |
| **Walk-forward / expanding-window evaluation** | The only scientifically defensible evaluation for time-series prediction. | CRITICAL |
| **Baseline / naïve model** | No comparison against a trivial classifier (always predict majority, or persistence). Without a baseline, AUC and balanced accuracy values are uninterpretable. | HIGH |
| **Confidence intervals / statistical significance** | Point estimates only; no bootstrap CIs, no DeLong test for AUC differences between models. | MODERATE |
| **Calibration assessment** (reliability diagram) | Probability outputs from RF and LightGBM may be poorly calibrated, especially with `is_unbalance`. | MODERATE |
| **Feature stability analysis** | Are SHAP rankings stable across different temporal windows? Without this, feature importance may be period-specific. | MODERATE |
| **Structural break tests** on features | Commodity markets exhibit regime shifts; ignoring this may cause model instability (Bai & Perron, 2003). | MODERATE |
| **Sensitivity to bubble detection parameters** | The GSADF minimum window and critical values affect the number and timing of detected bubbles — this propagates into the target variable. No robustness check is performed. | HIGH |
| **Data frequency documentation** | Never formally stated. The rolling window of 10 implicitly assumes daily data. This must be explicit. | LOW |
| **Forecast horizon documentation** | The lag-1 structure implies 1-day-ahead, but this is never stated or justified. | LOW |

---

## 4. Suggested Improvements

### 4.1 Replace Random CV with Time-Series CV

Implement expanding-window or sliding-window cross-validation. In R, this can be done manually or with `rsample::rolling_origin()`. For `cv.glmnet`, pass time-ordered fold indices via the `foldid` argument.

```
# Example: time-ordered folds for cv.glmnet
n <- nrow(X)
n_folds <- 5
foldid <- cut(1:n, breaks = n_folds, labels = FALSE)
cv_lasso <- cv.glmnet(X, y, ..., foldid = foldid)
```

### 4.2 Implement Walk-Forward Bubble Detection

Instead of applying `radf()` to the full sample, run it recursively up to each time point *t*, so that the bubble label at time *t* only uses information up to *t*. This is how the PSY procedure was intended to be used.

### 4.3 Add a Proper Temporal Holdout

Reserve the final 20–25% of the time series as a strict holdout set that is never touched during training or hyperparameter selection. Report all final metrics on this set.

### 4.4 Perform Nested Time-Series CV for LightGBM

Use an outer time-series CV for evaluation and an inner time-series CV for hyperparameter tuning. Add early stopping with a temporal validation set.

### 4.5 Harmonize Class Imbalance Treatment

Use `type.measure = "auc"` in `cv.glmnet`. Add `class.weights` or `sample.fraction` with stratification in `ranger`. Document the rationale.

### 4.6 Add Diagnostic Tests

- ADF/KPSS on all level features before modeling.
- Ljung-Box test on model residuals (prediction errors) to detect remaining autocorrelation.
- Include a naïve baseline (majority-class classifier and/or unconditional bubble rate).

---

## 5. Detailed File-by-File Analysis

---

### 5A. `metals31_RF.R` — Random Forest Pipeline (1018 lines)

**Intended workflow:** Construct a master dataset by merging four commodity metal prices across exchange sources (DALY, LME, WUXI), detect explosive bubbles via the GSADF test, download macroeconomic and market data (FRED + Yahoo Finance), engineer lag-1 returns / volatility features, train a Random Forest classifier, interpret via SHAP, and report performance metrics.

#### 5A.1 Data Construction — Source Selection and Merge (L55–L175)

- **L62–L87 (`get_longest_series_from_file`):** For each metal, the function selects the price column with the most non-NA observations. This is a defensible heuristic for maximizing sample size, but it introduces an implicit **survivorship/availability bias** — the longest series may not be the most liquid or economically representative exchange. The paper must document which exchange was selected for each metal and justify why length is prioritized over, e.g., trading volume or benchmark status.

- **L89–L102 (merge):** `full_join` followed by `drop_na()` creates a "strict overlap" panel. This is correct for ensuring temporal alignment. However, dropping all rows with any missing value (L104) implicitly truncates the sample to the shortest common window across all four metals. The effective sample period is never printed or documented — this must be stated in the paper.

- **L131–L172 (`build_robustness_dataset`):** The alternative approach selects series by source pattern (DALY, LME, WUXI). This is a good robustness design, and the code builds separate datasets per exchange. However, only `df_daly` is carried forward into bubble detection (L175–L180). The LME and WUXI datasets appear unused in the modeling pipeline, meaning the robustness check across exchanges is **declared but not executed** in the final model.

#### 5A.2 Bubble Detection — Full-Sample GSADF (L182–L230)

- **L181–L186:**
  ```r
  data_matrix <- df_daly %>% select(-Date) %>% as.data.frame()
  est_results <- radf(data_matrix)
  n_obs <- nrow(data_matrix)
  mc_cv <- radf_mc_cv(n = n_obs, seed = 123)
  ```
  `radf()` applies the GSADF test to the **full** panel simultaneously. The Monte Carlo critical values are calibrated to the full sample length `n_obs`. This is the source of the critical target-leakage risk identified in §1 RISK 2.

- **L196–L220 (dummy construction):** The loop iterates over `bubble_dates` (returned by `datestamp()`), extracts start/end row indices, and sets `dummy_matrix[start_idx:end_idx, col_idx] <- 1L`. This is a clean implementation, but the indices are from the full-sample test. A subtle additional issue: `datestamp()` uses the full BSADF statistic path to determine bubble origination and termination; both endpoints are informed by data that postdates them. Even the *start* of a bubble is a retroactively estimated quantity.

- **L188:** `seed = 123` for Monte Carlo CV is good practice for reproducibility. ✅

#### 5A.3 Macro & Market Data Ingestion (L371–L458)

- **L374–L384 (`df_macro` from FRED):**
  ```r
  df_macro <- tq_get(macro_tickers, get = "economic.data",
                     from = min(df_daly$Date, to = max(df_daly$Date))) %>% ...
  ```
  **NEW issue — Parenthetical bug:** The call to `min()` has a mismatched parenthesis: `from = min(df_daly$Date, to = max(df_daly$Date))`. This evaluates `min(df_daly$Date, to = max(df_daly$Date))` — in R, the `to` argument is silently treated as an additional value to `min()`, effectively computing `min(df_daly$Date, max(df_daly$Date))` = `min(df_daly$Date)`. The `to` parameter of `tq_get` is **never set**, so the download extends to today's date. This means macro data extends beyond the metal price sample, which could introduce subtle misalignment if downstream joins are not strictly filtered.

  **Verdict: BUG — likely unintentional.** The same parenthetical error appears in the Yahoo download at L403. The paper should use `from = min(df_daly$Date)` and `to = max(df_daly$Date)` as separate arguments.

- **L383 (`fill(everything(), .direction = "down")`):** LOCF fill for weekends/holidays in macro data. This is standard practice for aligning daily financial data. ✅ However, applying LOCF **before** merging with metals means that gaps due to different holiday calendars between FRED and commodity exchanges are filled unconditionally. In principle, the fill should be applied *after* merging on the metals' date grid to avoid carrying stale values across multi-day gaps (e.g., Christmas breaks).

- **L410 (`right_join(master_dates, by = "date")`):** Forces Yahoo data onto the metals' date grid. Any dates present in metals but absent in Yahoo will be filled with `NA`. This is then interpolated at L446. ✅ Correct intention, but cubic spline interpolation across long gaps (e.g., early years where an ETF didn't exist yet) can produce highly unreliable synthetic data. The paper should report the maximum gap length per variable.

- **L446–L449 (`na.spline()` globally):**
  ```r
  mutate(across(all_of(vars_to_interpolate),
                ~ na.spline(., x = Date, method = "natural", na.rm = FALSE)))
  ```
  Global cubic spline interpolation is a time-leaking operation: the fitted spline at any interior point uses observations from both sides. This is acceptable for filling small (1–3 day) weekend gaps, but problematic for longer stretches. Additionally, for any future train/test split, spline values near the boundary will be contaminated. The paper must either (a) document that only short gaps are filled, or (b) switch to a causal method like LOCF or Kalman filter.

- **L451–L452 (safety floor):**
  ```r
  mutate(across(any_of(c("Oil_WTI", "Oil_Brent", "VIX", "Gas_HenryHub")),
                ~ if_else(. < 0.01, 0.01, .)))
  ```
  Clamping interpolated values at 0.01 prevents log-domain errors. Pragmatic, but the variable names used (`"Oil_Brent"`, `"Gas_HenryHub"`) do not match the column names created earlier (`"OIL_WTI"`, `"GAS_HHUB"`). Unless downstream `any_of()` silently skips missing names, this is a **silent no-op** — the floor is never applied. If negative spline artifacts exist in Oil or Gas, `calc_safe_ret` (L651) would catch them later by replacing ≤0 with 0.001, but this is a fragile double-fallback.

#### 5A.4 Feature Engineering — `prepare_ml_data()` (L630–L700)

- **L637–L644 (predictor identification):**
  ```r
  predictors_numeric <- df_master %>%
    select(where(is.numeric)) %>%
    select(-ends_with("BD")) %>%
    select(-any_of(c("Date", "date", "DATE"))) %>% names()
  ```
  This dynamically selects all numeric columns except targets and dates. The advantage is generality; the risk is that **all price levels of all four metals are included as predictors alongside their own returns and volatility**. For the target metal, this means its own price level (e.g., `CODALY`) is a predictor. Since the target is a bubble dummy derived from the same price series, the model is partially predicting `CODALY_BD` from lagged `CODALY_Level_Lag1`. This is not leakage (the lag prevents it), but it conflates the detection of high price levels with bubble classification, which may produce tautological importance rankings.

- **L651–L660 (`calc_safe_ret`):** Replacing non-positive values with 0.001 before `log()` is a pragmatic fix for spline artifacts or data errors. However, `log(0.001/prev_value)` produces an extremely large negative return (a synthetic crash signal). For rare events, this is unlikely to affect results materially, but the paper should document it. A more robust approach: flag these observations and either impute via LOCF or exclude them.

- **L661–L668 (returns and volatility computation):** `calc_safe_vol` uses `TTR::runSD(c(NA, diff(log(x_num))), n = 10)`. The `runSD` function computes a right-aligned rolling standard deviation. The `n = 10` window implies 10 business days ≈ 2 trading weeks if data is daily. This is standard for short-term realized volatility (Andersen et al., 2003). ✅

- **L679–L683 (lagging):**
  ```r
  across(ends_with("_Ret"), ~ lag(.), .names = "{.col}_Lag1"),
  across(ends_with("_Vol"), ~ lag(.), .names = "{.col}_Lag1"),
  across(all_of(predictors_numeric), ~ lag(.), .names = "{.col}_Level_Lag1"),
  ```
  All features are lagged by 1. This is the correct procedure for avoiding direct look-ahead in the feature matrix. ✅ However, the rolling volatility already has an effective look-back of 10 periods, so `Vol_Lag1` at time *t* uses returns from *t*−11 to *t*−1. This is not leakage but introduces a 10-day information delay that should be acknowledged.

- **L683 (peer dummies):**
  ```r
  across(all_of(peer_dummies), ~ replace_na(lag(.), 0), .names = "{.col}_Lag1")
  ```
  Lagged peer bubble dummies replace NA (from the first observation) with 0. This is conservative and correct. ✅

- **L690 (`drop_na(df_model)`):** Drops the first ~10–11 rows (from lag + rolling window burn-in). This is the expected behavior.

#### 5A.5 Random Forest Training (L712–L760)

- **L725–L729:**
  ```r
  fit_rf <- ranger(
    as.factor(Target_Bubble) ~ .,
    data = select(df_ml, -Date),
    importance = "permutation",
    probability = TRUE,
    num.trees = 500
  )
  ```
  **No class weighting.** `ranger()` with default settings assigns equal prior probability to both classes. For rare bubble events (typically <10% of observations), the model will be biased toward predicting the majority class (no bubble). This depresses sensitivity and inflates specificity — the metrics will reflect this asymmetry. The paper should either add `class.weights` or use `sample.fraction` with stratified bootstrap.

  - Breiman (2001), "Random forests," *Machine Learning*, 45(1), 5–32 — notes that class priors affect classification performance.
  - Chen et al. (2004), "Using random forest to learn imbalanced data," UC Berkeley Tech Report.

- **L725:** `importance = "permutation"` — permutation importance is computed, but never reported or used (only SHAP is plotted). This is not wrong, but the paper could include permutation importance as a complementary robustness check against SHAP. Permutation importance is less sensitive to feature correlation than mean decrease in impurity (Strobl et al., 2007).

- **L731–L733 (SHAP background data):**
  ```r
  bg_X <- X_explain[sample(nrow(X_explain), min(25, nrow(X_explain))), ]
  rows_to_explain <- sample(nrow(X_explain), min(100, nrow(X_explain)))
  ```
  Using only 25 background samples and explaining 100 observations is computationally pragmatic but may produce noisy SHAP estimates, especially for interaction effects. `kernelshap` documentation recommends 100–200 background samples for stability. The paper should report whether SHAP rankings are sensitive to increasing background size.

- **NEW issue — No seed for SHAP sampling.** The `sample()` calls at L731–L732 are not preceded by `set.seed()`. This means SHAP values are not reproducible across runs. For a paper, reproducibility requires fixing the seed.

#### 5A.6 Performance Evaluation (L930–L1018)

- **L940–L960:** The performance loop re-creates the data (`prepare_ml_data()`), extracts `model_rf$predictions[, 2]` (OOB predictions), and computes metrics. As noted in §1 RISK 5, OOB predictions with random bootstrap are not valid out-of-sample estimates for time series.

- **L963:** `pred_class <- ifelse(prob_bubble > 0.5, 1, 0)` — a fixed 0.5 threshold is used. With imbalanced data, this threshold is suboptimal. The Youden index or cost-sensitive thresholding should be considered.

- **No F1-score or Matthews Correlation Coefficient (MCC).** For imbalanced data, MCC is often recommended as a single summary metric (Chicco & Jurman, 2020), because balanced accuracy can still mask poor performance when one class is very rare.

#### 5A.7 Assumptions That Must Be Stated in the Paper

1. Data frequency (daily, weekly, or business-day) and the exact sample period.
2. Which exchange source was selected for each metal and why.
3. The forecast horizon is 1 day ahead (implied by lag-1; never stated).
4. The rolling volatility window of 10 corresponds to approximately 2 trading weeks.
5. Cubic spline interpolation was applied to fill gaps in macro data — maximum gap length should be reported.
6. The 0.001 floor substitution for non-positive prices is an approximation.

#### 5A.8 Suggested Additional Robustness Checks

- Compare GSADF results using different minimum window sizes (e.g., `radf(data_matrix, minw = ...)`) to test sensitivity of bubble labels.
- Re-run the RF with `class.weights = c("0" = 1, "1" = N_0/N_1)` and compare metrics.
- Verify that OOB and true out-of-time performance diverge, as expected for serially correlated data.
- Add permutation-based variable importance alongside SHAP for triangulation.

---

### 5B. `metals32_lightGBM.R` — LightGBM Pipeline (628 lines)

**Intended workflow:** Reuse the RF data pipeline (`prepare_ml_data()` from `metals31_RF.R`); train LightGBM with fixed hyperparameters for binary bubble classification; compute SHAP natively; evaluate via manual 5-fold CV with random fold assignment; report AUC and balanced accuracy.

#### 5B.1 Data Preparation (L12–L80)

- **L14 (comment):** `#old function - use the one from metals31_RF.R` — the function body is present but commented out (L15 begins `target_dummy <-` outside the function braces). The live code depends on `prepare_ml_data()` already existing in the R session from a prior execution of `metals31_RF.R`. This is a **reproducibility risk**: the script is not self-contained. If run in a fresh session, it will fail. A reviewer would flag this as a barrier to independent replication.

- **L17–L80:** The commented-out `prepare_ml_data` body is identical to the RF version (same return computation, volatility, lagging). This confirms the feature sets are comparable across models. ✅

  **NEW observation — Subtle feature-set divergence with LASSO:** Unlike `metals33_Lasso.R`, this version does **not** drop `VIX_Vol` (L73 in LASSO). It also includes `_Level_Lag1` for *all* numeric predictors (L72: `across(all_of(predictors_numeric), ...)`), whereas LASSO restricts levels to a curated list of 8 variables (L31–L33 in LASSO: `vars_for_levels`). This means RF and LightGBM operate on a **larger feature space** than LASSO, which makes the model comparison less controlled. The paper should document the feature-set differences explicitly or standardize them.

#### 5B.2 LightGBM Training — `run_lgbm_pipeline()` (L86–L194)

- **L107–L109 (matrix conversion):**
  ```r
  X_matrix <- as.matrix(select(df_ml, -Date, -Target_Bubble))
  y_vector <- as.numeric(df_ml$Target_Bubble)
  dtrain <- lgb.Dataset(data = X_matrix, label = y_vector)
  ```
  LightGBM requires numeric input; the conversion is correct. ✅

- **L120–L129 (params):** Fixed hyperparameters. `metric = "auc"` is set for internal reporting, but since no `valids` argument is passed to `lgb.train()` (L132), **no validation metric is ever evaluated during training.** The `metric` parameter is effectively dead code.

- **L125 (`is_unbalance = TRUE`):** This internally adjusts instance weights by the inverse class frequency. This is a valid strategy for imbalanced data (LightGBM documentation: "Use this only in cases of severe class imbalance"). ✅ However, it interacts with the classification threshold: the model now outputs calibrated-ish probabilities for the reweighted distribution, not the natural distribution. This can shift the optimal threshold away from 0.5, yet the metrics code at L293 uses 0.5.

- **L126–L128 (`feature_fraction = 0.8`, `bagging_fraction = 0.8`, `bagging_freq = 5`):** Column and row subsampling. These are regularization techniques that reduce overfitting. ✅ However, `bagging_fraction` implements random row sampling at each `bagging_freq`-th iteration — this is another form of random (non-temporal) data partition *within* training. While less damaging than CV shuffling (because it's a training regularizer, not an evaluation strategy), it means the training process itself does not respect temporal ordering.

- **L132–L135:**
  ```r
  fit_lgbm <- lgb.train(
    params = params, data = dtrain, nrounds = 200, verbose = 0
  )
  ```
  **No early stopping.** Without a validation set or `early_stopping_rounds`, the model trains for exactly 200 rounds regardless of convergence. This risks overfitting (too many rounds) or underfitting (too few). The LightGBM documentation recommends always using early stopping with a holdout set. For time-series, the validation set should be the chronologically most recent segment.

  - Ke et al. (2017), "LightGBM: A highly efficient gradient boosting decision tree," *NeurIPS*, 30 — §4.2 discusses early stopping as essential practice.

- **L138 (`shapviz(fit_lgbm, X_pred = X_matrix)`):** SHAP values are computed natively using LightGBM's built-in TreeSHAP. This is computationally efficient and exact for tree models (Lundberg et al., 2020). ✅ However, `X_pred = X_matrix` computes SHAP for **all** observations including those that the model was trained on. SHAP values from training data may emphasize memorized patterns rather than generalizable ones. Computing SHAP on out-of-fold predictions would be more informative.

#### 5B.3 Execution and Saving (L161–L199)

- **L169–L193:** The loop trains one model per metal on the **full dataset**, saves the model and SHAP plot, and stores `X` and `y` in `lgbm_results`. The subsequent CV loop (§5B.4) trains *different* models per fold. This means **the SHAP plots and the reported metrics come from different models**. Specifically:
  - SHAP values are from the full-data model (L138)
  - AUC/metrics are from the CV loop (L248–L262)
  
  This is a **consistency problem**: the model being interpreted (via SHAP) is not the same model being evaluated (via CV). A reviewer should require that SHAP and metrics reference the same modeling procedure.

#### 5B.4 Cross-Validation Loop (L205–L320)

- **L247:**
  ```r
  folds <- sample(rep(1:n_folds, length.out = nrow(X)))
  ```
  This is the critical random-shuffle operation. `sample()` randomizes fold assignment without regard for temporal ordering. Observations from 2023 can train models evaluated on 2018 data. This is the core of RISK 1.

- **NEW issue — No seed for fold assignment.** Unlike LASSO (which sets `set.seed(123)` at L505), the LightGBM CV loop has no seed. Fold assignment changes every run, making reported metrics non-reproducible. This is both a methodological and practical defect.

- **L253–L262 (fold training):**
  ```r
  d_train_fold <- lgb.Dataset(data = X[idx_train, , drop=FALSE], label = y[idx_train])
  bst_fold <- lgb.train(params = params, data = d_train_fold, nrounds = 200, verbose = 0)
  cv_preds[idx_val] <- predict(bst_fold, X[idx_val, , drop=FALSE])
  ```
  Each fold trains a full 200-round model from scratch with the same hyperparameters. No early stopping per fold either. The same overfitting risk from §5B.2 is replicated 5 times.

- **L266–L278 (optimal threshold):**
  ```r
  best_coords <- coords(roc_obj, "best", best.method = "closest.topleft", ...)
  ```
  The optimal threshold is computed but then **not used** — L293 uses the fixed 0.5:
  ```r
  #pred_class <- ifelse(cv_preds > best_thresh, 1, 0)
  pred_class <- ifelse(cv_preds > 0.5, 1, 0)
  ```
  The commented-out line shows the code was at some point intended to use the optimal threshold. This is acceptable (using 0.5 is the more conservative/standard choice), but the paper should justify why 0.5 is preferred over the Youden-optimal threshold, especially under class imbalance. Note: selecting the threshold on the same data used for evaluation would be a form of **threshold-selection leakage**; the commented-out line would have introduced this.

#### 5B.5 Additional Issues Not Covered Elsewhere

- **Duplicated code blocks.** Lines 340–628 contain multiple commented-out versions of the same CV loop and metric computation. These are development artifacts. While not a methodological issue, they indicate the evaluation strategy was iteratively revised, which increases the risk of copy-paste errors between the active and archived code. The active code should be isolated from old versions.

- **No feature importance comparison across metals.** Each metal gets its own SHAP beeswarm plot, but no cross-metal comparison is made. For a paper claiming cross-commodity spillover effects, a summary figure showing whether the same features drive bubbles across all metals would strengthen the narrative.

- **`num_leaves = 31` may be too expressive.** With 31 leaves and daily data, individual trees can model fine-grained interactions. For commodity time series with structural regime shifts, high-complexity trees risk fitting noise in one regime that doesn't generalize to another. A lower `num_leaves` (e.g., 7–15) or `max_depth` constraint would be more conservative.

  - Zhang & Haghani (2021), "Gradient boosted trees for financial time series," *Journal of Financial Data Science*, 3(1) — recommends shallower trees for noisy financial data.

#### 5B.6 Assumptions That Must Be Stated in the Paper

1. The feature set used by LightGBM includes all numeric level-lag features (unlike the curated LASSO set).
2. Hyperparameters were fixed, not tuned — this is a researcher degree of freedom.
3. The CV metrics and SHAP plots come from different models (full-data vs. fold-specific).
4. `is_unbalance = TRUE` changes the effective loss function; the 0.5 classification threshold may not be optimal under reweighting.
5. No early stopping was used; all models trained for exactly 200 rounds.

#### 5B.7 Suggested Additional Robustness Checks

- Implement early stopping with a temporal validation set (last 20% of each fold's training data).
- Run `lgb.cv()` with `stratified = TRUE` to at least stratify folds by class, reducing variance (while still not addressing the temporal issue).
- Compare metrics from random-CV with a simple temporal train-70%/test-30% split to quantify the optimism bias.
- Report `lgb.importance()` alongside SHAP for consistency check.

---

### 5C. `metals33_Lasso.R` — Logistic LASSO Pipeline (614 lines)

**Intended workflow:** Train an L1-penalized logistic regression as a linear benchmark for bubble classification; use `cv.glmnet` with `keep = TRUE` to extract fair out-of-fold predictions; report AUC, balanced accuracy, and LASSO-selected coefficients as feature importance.

#### 5C.1 Data Preparation — `prepare_ml_data()` (L13–L90)

- **L29–L33 (curated level variables):**
  ```r
  vars_for_levels <- c("VIX", "YIELD_10Y", "OIL_WTI", "GAS_HH",
                       "CODALY", "LIDALY", "NIDALY", "CUDALY")
  predictors_for_levels <- intersect(predictors_numeric, vars_for_levels)
  ```
  Unlike the RF and LightGBM versions (which include all numeric levels), LASSO restricts `_Level_Lag1` features to a curated list of 8 variables: 4 metals + VIX + yield + oil + gas. This is a **domain-driven feature selection** step that reduces dimensionality for the linear model. ✅ CORRECT — LASSO is sensitive to high-dimensional correlated inputs; including all levels (as RF/LGBM do) would dilute the penalty across many redundant features.

  However, this means the three models operate on **different feature sets**, which complicates direct comparison. The paper should either (a) standardize features across all models, or (b) explicitly frame the LASSO feature restriction as part of the linear-benchmark design and discuss its impact on comparability.

- **L73:**
  ```r
  select(-any_of("VIX_Vol"))
  ```
  Removes VIX volatility since VIX is itself a volatility index. This avoids a near-tautological feature (volatility of volatility). ✅ GOOD — demonstrates domain knowledge. The RF and LightGBM scripts do not do this, which means they retain a potentially redundant/confusing feature.

- **L79–L84 (lagging):**
  ```r
  across(all_of(predictors_for_levels), ~ lag(.), .names = "{.col}_Level_Lag1"),
  #across(all_of(predictors_numeric), ~ lag(.), .names = "{.col}_Level_Lag1"),
  ```
  The commented-out line shows the developer considered including all levels (as in RF/LGBM) but chose the curated list. The paper should acknowledge this design choice.

- **NEW issue — `GAS_HH` vs. actual column name:** `vars_for_levels` includes `"GAS_HH"`, but the macro data pipeline in `metals31_RF.R` L380 renames the gas column to `"GAS_HHUB"`. If the actual column is `GAS_HHUB`, then `intersect(predictors_numeric, vars_for_levels)` will **silently exclude gas**, and no gas level-lag feature will be created. This is a potential silent data bug that could affect LASSO results. Verify with `names(df_master)`.

#### 5C.2 Commented-Out First Pipeline (L96–L210)

- **L96–L197:** An entire first pipeline is commented out. Key observation: this version used `cv.glmnet(X, y, alpha = 1, family = "binomial", type.measure = "class")` without `keep = TRUE`, then predicted on the training set:
  ```r
  probs <- predict(cv_lasso, newx = X, s = "lambda.min", type = "response")
  ```
  This is **in-sample evaluation** — the model is evaluated on the same data used to select `lambda.min`. The AUC from this version would be inflated. The developer correctly identified this problem and replaced it with the `keep = TRUE` approach (L373). The commented code serves as documentation of the iterative improvement. ✅ Good scientific practice to move away from in-sample evaluation.

#### 5C.3 Active Pipeline — Corrected Version (L320–L430)

- **L373:**
  ```r
  cv_lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial",
                        type.measure = "class", standardize = TRUE,
                        maxit = 1000000, keep = TRUE)
  ```
  
  **`keep = TRUE`:** This is the key methodological improvement. With `keep = TRUE`, `cv.glmnet` stores the out-of-fold predicted values for each lambda in `fit.preval`. This allows extraction of "fair" (out-of-fold) predictions without re-predicting on training data. ✅ CORRECT within the random-CV framework.

  **`standardize = TRUE`:** `glmnet` standardizes features internally, computing mean and standard deviation within each CV fold's training partition. This prevents normalization leakage across folds. ✅ CORRECT — this is one of the few places where the code properly handles within-fold preprocessing.

  **`type.measure = "class"`:** Selects lambda by minimizing misclassification error. For imbalanced data, this optimizes for overall accuracy, which is dominated by the majority class. The model may select a lambda that predicts "no bubble" for all observations, achieving ~90%+ accuracy while having 0% sensitivity. **`type.measure = "auc"` would be more appropriate** for bubble detection, as it evaluates the model's discriminative ability across all thresholds.

  - Friedman, Hastie & Tibshirani (2010), "Regularization paths for generalized linear models via coordinate descent," *Journal of Statistical Software*, 33(1), 1–22 — §3 discusses the choice of `type.measure`.

  **`alpha = 1` (pure LASSO):** With highly correlated features (e.g., multiple metal returns that co-move), pure LASSO may arbitrarily select one variable from a correlated group and zero out the others. The elastic net (`alpha` ∈ (0, 1)) would be more stable. The paper should either justify `alpha = 1` or include an elastic-net robustness check.

  - Zou & Hastie (2005), "Regularization and variable selection via the elastic net," *JRSS Series B*, 67(2), 301–320 — introduces elastic net as a solution to LASSO's instability under multicollinearity.

  **`maxit = 1000000`:** Very high maximum iterations. This ensures convergence but is likely overkill for most lambda values. Not a methodological issue, just a computational one.

  **No `foldid` argument:** `cv.glmnet` uses random 10-fold CV by default (the `nfolds` argument defaults to 10). For time-series data, this must be replaced with time-ordered fold indices. The fix is simple:
  ```r
  foldid <- cut(seq_len(nrow(X)), breaks = 10, labels = FALSE)
  cv_lasso <- cv.glmnet(X, y, ..., foldid = foldid)
  ```

- **L379–L381 (extracting out-of-fold predictions):**
  ```r
  idx_min <- which(cv_lasso$lambda == cv_lasso$lambda.min)
  log_odds <- cv_lasso$fit.preval[, idx_min]
  probs_vec <- exp(log_odds) / (1 + exp(log_odds))
  ```
  The manual logistic transformation is correctly applied: `fit.preval` returns log-odds (the linear predictor), not probabilities, for `family = "binomial"`. ✅ CORRECT.

  **NEW subtle issue:** `fit.preval` stores predictions for all lambda values, but the predictions are **from the CV used to select lambda.min**. This means `lambda.min` was selected and the out-of-fold predictions for that lambda were computed using the *same* CV partition. There is no separate "model selection" vs. "model evaluation" partition. This is a mild form of **selection bias** — the lambda that looks best on these particular folds is then evaluated on those same folds' out-of-fold predictions. The bias is typically small for large samples but should be acknowledged. A proper solution is nested CV (outer folds for evaluation, inner folds for lambda selection).

  - Varma & Simon (2006), "Bias in error estimation when using cross-validation for model selection," *BMC Bioinformatics*, 7, 91 — quantifies the optimism from single-loop CV when both selecting and evaluating.

#### 5C.4 Third Pipeline with Coefficient Plots (L450–L614)

- **L505:**
  ```r
  set.seed(123)
  ```
  This seeds the random fold assignment. ✅ GOOD for reproducibility. However, it only applies to this pipeline iteration; the earlier corrected pipeline (L320–L430) does not set a seed, so those results are not reproducible.

- **L507–L509:**
  ```r
  cv_lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial",
                        type.measure = "class", standardize = TRUE,
                        maxit = 1000000, keep = TRUE)
  ```
  Identical call to L373. The third pipeline is a near-duplicate of the second but adds coefficient-based feature importance plots (L553–L575). This duplication means the same model is trained twice per metal, wasting computation and introducing the risk of divergent results if seeds differ. The pipelines should be consolidated.

- **L553–L575 (coefficient importance plot):**
  ```r
  coef_df <- coef_df %>%
    filter(Feature != "(Intercept)", Coefficient != 0) %>%
    mutate(Feature = gsub("_Lag1", "", Feature),
           Feature = gsub("_Level", "", Feature)) %>%
    arrange(desc(abs(Coefficient))) %>% head(15)
  ```
  Plotting the top 15 non-zero LASSO coefficients is a valid way to show feature importance for linear models. ✅ However, LASSO coefficient magnitudes are sensitive to the scale of the input features. Even though `standardize = TRUE` rescales features during fitting, the reported coefficients are **back-transformed** to the original scale (this is `glmnet`'s default behavior). This means features with larger natural variance will have smaller coefficients even if they are equally important. For interpretability, the paper should either report standardized coefficients or note this caveat.

  - Tibshirani (1996), "Regression shrinkage and selection via the lasso," *JRSS Series B*, 58(1), 267–288 — foundational reference; §2 discusses coefficient interpretation under standardization.

#### 5C.5 Additional Issues Not Covered Elsewhere

- **No intercept handling for imbalanced data.** `glmnet` with `family = "binomial"` estimates an intercept that reflects the training-set class distribution. With severe imbalance, the intercept shifts the decision boundary toward predicting the majority class. Combined with `type.measure = "class"`, this double-penalizes the minority class. Using `type.measure = "auc"` and/or `weights` argument would mitigate this.

- **NEW issue — Potential column mismatch across the two active pipelines.** The corrected version (L320–L430) and the coefficient version (L450–L614) both call `prepare_ml_data()` independently. If `df_master` is modified between the two runs (e.g., via interactive session changes), the feature matrices may differ. This is an execution-order dependency that would not arise in a single-pipeline design.

- **No regularization path visualization.** Plotting `cv_lasso` (which shows cross-validated error as a function of log(lambda)) is standard diagnostic practice for LASSO. It reveals whether the lambda selection is stable (flat curve ≈ robust) or sensitive (sharp minimum ≈ fragile). This diagnostic is absent.

- **NEW issue — LASSO assumes a linear decision boundary.** Logistic LASSO models `log(p/(1-p))` as a linear function of features. Bubble dynamics are likely **non-linear** — e.g., a metal's volatility may only matter above a certain threshold, or contagion effects may be interaction-driven. LASSO cannot capture these effects. The paper should explicitly frame LASSO as a linear benchmark and discuss its limitations for this application.

  - Hastie, Tibshirani & Friedman (2009), *ESL*, §4.4 — discusses limitations of linear classifiers for non-linear decision boundaries.

#### 5C.6 Assumptions That Must Be Stated in the Paper

1. LASSO operates on a restricted feature set (8 level variables) compared to RF/LGBM (all numeric levels).
2. `VIX_Vol` is excluded; RF/LGBM retain it.
3. `alpha = 1` (pure L1) was chosen over elastic net — this should be justified or tested.
4. `type.measure = "class"` was used for lambda selection; `"auc"` would better reflect the research objective.
5. Lambda selection and metric evaluation share the same CV folds (no nested CV).
6. Coefficient magnitudes are on the original (not standardized) scale.

#### 5C.7 Suggested Additional Robustness Checks

- Compare `alpha = 1` (LASSO) vs. `alpha = 0.5` (elastic net) vs. `alpha = 0` (Ridge) to assess sensitivity to regularization type.
- Use `type.measure = "auc"` and compare the selected lambda and number of non-zero coefficients.
- Plot the regularization path (`plot(cv_lasso)`) to assess lambda stability.
- Implement time-ordered `foldid` and compare metrics against the random-fold results to quantify optimism.
- Report standardized coefficients for interpretability.
- Add an L2-penalized logistic regression (Ridge) as a comparator that retains all features.

---

## 6. Cross-File Methodological Coherence

### 6.1 Feature-Set Inconsistency Across Models

| Feature Category | RF (`metals31_RF.R`) | LightGBM (`metals32_lightGBM.R`) | LASSO (`metals33_Lasso.R`) |
|---|---|---|---|
| Returns (`_Ret_Lag1`) | All numeric | All numeric | All numeric |
| Volatility (`_Vol_Lag1`) | All numeric | All numeric | All numeric **minus `VIX_Vol`** |
| Levels (`_Level_Lag1`) | All numeric | All numeric | **8 curated variables only** |
| Peer bubble dummies | ✅ | ✅ | ✅ |

This asymmetry means the models are not directly comparable: RF and LightGBM see a richer feature space. LASSO's relative under-performance (if any) could be attributed to either its linearity or its smaller feature set, and the two effects are confounded. For a fair benchmark, features should be held constant; model capacity should be the only variable.

### 6.2 Evaluation Protocol Inconsistency

| Model | Evaluation Type | Seed Set? | Temporal? |
|---|---|---|---|
| RF | OOB (random bootstrap) | N/A (ranger internal) | ❌ No |
| LightGBM | 5-fold CV (random) | ❌ No seed | ❌ No |
| LASSO | 10-fold CV (random, default `cv.glmnet`) | ✅ `set.seed(123)` (third pipeline only) | ❌ No |

None uses temporal ordering. The number of folds differs (5 vs. 10). Reproducibility varies. A defensible comparison requires identical evaluation protocols.

### 6.3 Missing Cross-Model Statistical Comparison

No statistical test is applied to compare AUC values across models. The DeLong test (DeLong et al., 1988) is standard for comparing correlated ROC curves and should be included.

---

## 7. Overall Assessment

The research question — using ML to detect commodity price bubbles with cross-metal and macroeconomic features — is well-motivated and timely. The feature engineering reflects solid financial domain knowledge (log-returns, realized volatility, VIX handling, peer contagion dummies). SHAP-based interpretation adds scientific value.

However, **two critical flaws** prevent the methodology from being publishable:

1. **Random cross-validation on time-series data** — all reported metrics are unreliable.
2. **Full-sample bubble label construction** — the target variable contains future information.

Additionally, several moderate flaws weaken the defensibility of the results:

3. **Inconsistent feature sets** across models prevent fair comparison.
4. **No hyperparameter tuning** for RF or LightGBM, and no early stopping for LightGBM.
5. **No baseline model** — metrics cannot be contextualized.
6. **No stationarity diagnostics** on level features.
7. **Global preprocessing** (spline interpolation) creates latent leakage.
8. **Potential data bug** in `tq_get()` parenthetical syntax (RF L374, L403) may cause unintended download ranges.

Fixing the two critical issues (time-series CV + recursive bubble detection) would require re-running all experiments but would make the methodology defensible. The moderate issues are addressable in revision without fundamental re-design. The strengths — domain-informed features, SHAP interpretation, multi-model comparison, and peer-contagion modeling — form a solid foundation for a publishable contribution once the evaluation framework is corrected.

---

## References

1. Andersen, T. G., Bollerslev, T., Diebold, F. X., & Labys, P. (2003). Modeling and forecasting realized volatility. *Econometrica*, 71(2), 579–625.
2. Bai, J., & Perron, P. (2003). Computation and analysis of multiple structural change models. *Journal of Applied Econometrics*, 18(1), 1–22.
3. Bergmeir, C., Hyndman, R. J., & Koo, B. (2018). A note on the validity of cross-validation for evaluating autoregressive time series prediction. *Computational Statistics & Data Analysis*, 120, 70–83.
4. Breiman, L. (2001). Random forests. *Machine Learning*, 45(1), 5–32.
5. Campbell, J. Y., Lo, A. W., & MacKinlay, A. C. (1997). *The Econometrics of Financial Markets*. Princeton University Press.
6. Cerqueira, V., Torgo, L., & Mozetič, I. (2020). Evaluating time series forecasting models: An empirical study on performance estimation methods. *Machine Learning*, 109, 1997–2028.
7. Chen, C., Liaw, A., & Breiman, L. (2004). Using random forest to learn imbalanced data. UC Berkeley Technical Report 666.
8. Chicco, D., & Jurman, G. (2020). The advantages of the Matthews correlation coefficient (MCC) over F1 score and accuracy in binary classification evaluation. *BMC Genomics*, 21, 6.
9. DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing the areas under two or more correlated receiver operating characteristic curves. *Biometrics*, 44(3), 837–845.
10. Friedman, J., Hastie, T., & Tibshirani, R. (2010). Regularization paths for generalized linear models via coordinate descent. *Journal of Statistical Software*, 33(1), 1–22.
11. Hastie, T., Tibshirani, R., & Friedman, J. (2009). *The Elements of Statistical Learning*, 2nd ed. Springer.
12. He, H., & Garcia, E. A. (2009). Learning from imbalanced data. *IEEE TKDE*, 21(9), 1263–1284.
13. Hyndman, R. J., & Athanasopoulos, G. (2021). *Forecasting: Principles and Practice*, 3rd ed. OTexts.
14. Janitza, S., & Hornung, R. (2018). On the overestimation of random forest's out-of-bag error. *PLOS ONE*, 13(8), e0201904.
15. Ke, G., et al. (2017). LightGBM: A highly efficient gradient boosting decision tree. *NeurIPS*, 30.
16. Lundberg, S. M., & Lee, S.-I. (2017). A unified approach to interpreting model predictions. *NeurIPS*, 30.
17. Lundberg, S. M., et al. (2020). From local explanations to global understanding with explainable AI for trees. *Nature Machine Intelligence*, 2, 56–67.
18. Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for multiple bubbles: Historical episodes of exuberance and collapse in the S&P 500. *International Economic Review*, 56(4), 1043–1078.
19. Probst, P., Boulesteix, A.-L., & Bischl, B. (2019). Tunability: Importance of hyperparameters of machine learning algorithms. *JMLR*, 20(53), 1–32.
20. Roberts, D. R., et al. (2017). Cross-validation strategies for data with temporal, spatial, hierarchical, or phylogenetic structure. *Ecography*, 40(8), 913–929.
21. Saito, T., & Rehmsmeier, M. (2015). The precision-recall plot is more informative than the ROC plot when evaluating binary classifiers on imbalanced datasets. *PLOS ONE*, 10(3), e0118432.
22. Strobl, C., Boulesteix, A.-L., Zeileis, A., & Hothorn, T. (2007). Bias in random forest variable importance measures. *BMC Bioinformatics*, 8, 25.
23. Tashman, L. J. (2000). Out-of-sample tests of forecasting accuracy: an analysis and review. *International Journal of Forecasting*, 16(4), 437–450.
24. Tibshirani, R. (1996). Regression shrinkage and selection via the lasso. *JRSS Series B*, 58(1), 267–288.
25. Varma, S., & Simon, R. (2006). Bias in error estimation when using cross-validation for model selection. *BMC Bioinformatics*, 7, 91.
26. Zhang, Z., & Haghani, S. (2021). Gradient boosted trees for financial time series. *Journal of Financial Data Science*, 3(1).
27. Zou, H., & Hastie, T. (2005). Regularization and variable selection via the elastic net. *JRSS Series B*, 67(2), 301–320.
