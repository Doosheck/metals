# Script Walkthrough: Commodity Bubble Detection via Machine Learning

> **What is this project about?**
> These three scripts try to predict whether a critical metal (cobalt, lithium, nickel, copper) is currently in a **price bubble** — an abnormal price run-up beyond what fundamentals can justify. Three different machine learning models are trained and compared: a Random Forest, a gradient-boosted tree model (LightGBM), and a penalized linear model (LASSO). The results are reported as performance tables ready for a paper.

---

## Table of Contents

1. [Shared Concepts: What All Three Scripts Do](#shared-concepts)
2. [metals31_RF.R — Random Forest](#metals31_rfr--random-forest)
3. [metals32_lightGBM.R — LightGBM](#metals32_lightgbmr--lightgbm)
4. [metals33_Lasso.R — LASSO Logistic Regression](#metals33_lassor--lasso-logistic-regression)
5. [Cross-Script Problems](#cross-script-problems)
6. [Summary: What Is Good, What Is Broken](#summary)

---

## Shared Concepts

Before diving into the individual scripts, here are the core ideas that appear in all three.

### The Task: Binary Classification
Each model answers one question per day, per metal:

> *"Is this metal currently in a bubble? Yes (1) or No (0)?"*

This is called **binary classification**. The models are trained on historical data where we already know the answer (a 0/1 label), and then evaluated on how well they can reproduce those labels.

### How Features Are Built (The Same in All Three)
All three scripts use the same `prepare_ml_data()` function (defined in `metals31_RF.R`, reused by the other two). It turns raw price and macro data into model-ready inputs:

| Raw Input | What Is Done | Why |
|---|---|---|
| Price level (e.g., cobalt price) | Converted to **log-return**: $\ln(P_t / P_{t-1})$ | Removes the upward trend; makes the series stationary |
| Log-return series | **10-day rolling standard deviation** | Captures recent volatility — a classic signal that markets are stressed |
| Every feature | **Shifted back by 1 day** (lagged) | Prevents the model from "cheating" by seeing today's data to predict today's outcome |
| Other metals' bubble dummies | Also lagged by 1 day and added as features | Captures contagion: if lithium had a bubble yesterday, does that predict cobalt's bubble today? |

The lag is the most important safeguard. Without it, the model would be trained on data it could never have in a real prediction scenario.

### Metrics Used
- **Balanced Accuracy** — average of "what fraction of bubbles did we catch" and "what fraction of non-bubbles did we correctly ignore". Better than plain accuracy when bubbles are rare.
- **Sensitivity** (Recall) — of all the actual bubble periods, how many did the model detect?
- **Specificity** — of all the normal periods, how many did the model correctly call normal?
- **Precision** — of everything the model flagged as a bubble, how many were real bubbles?
- **AUC** (Area Under the ROC Curve) — a threshold-free summary. 0.5 = random guessing, 1.0 = perfect.

---

## metals31_RF.R — Random Forest

**1,018 lines. This is the "master" script. It builds the entire dataset and defines the feature engineering function used by all three models.**

### Step 1 — Load Packages and Set Up

The script starts by defining a helper `install_and_load()` function that checks which packages are missing, installs them, and loads them. Key packages: `tidyverse` (data manipulation), `ranger` (fast Random Forest), `shapviz`/`kernelshap` (SHAP explanations), `TTR` (rolling statistics), `exuber` (bubble detection).

### Step 2 — Select the Best Price Series for Each Metal

Four CSV files are loaded (cobalt, copper, lithium, nickel), each containing price data from multiple exchanges. For each metal, the script picks the column with the **most non-missing observations** — the longest series.

```
Metal: Cobalt   | Best Series: CODALY | Observations: 2187
```

These four series are then merged into a single table by date. A "strict overlap" version is created by dropping any row where any metal is missing — this ensures all four metals are observed on the same dates.

> ⚠️ **Issue (from METHOD_REVIEW):** The script picks the longest series automatically, but never explains *which exchange* that corresponds to. In a paper, this choice needs to be justified — is the longest series also the most liquid or most representative?

### Step 3 — Detect Bubbles Using the GSADF Test

This is where the **target variable** (what the models must predict) is created.

The `radf()` function from the `exuber` package applies the **GSADF test** (Generalized Supremum Augmented Dickey-Fuller), developed by Phillips, Shi & Yu (2015). The logic:
- A standard price series is roughly random-walk: tomorrow's price is today's price plus noise.
- In a bubble, the series starts **accelerating faster than a random walk** — prices grow explosively.
- The GSADF test detects windows of time where this explosive behavior occurs.

For each metal, the test returns start and end dates of detected bubbles. These are converted into a **0/1 dummy column** per metal: `1` on every day inside a detected bubble, `0` otherwise.

```r
est_results <- radf(data_matrix)          # Run the test
mc_cv       <- radf_mc_cv(n = n_obs)      # Simulate critical values via Monte Carlo
bubble_dates <- datestamp(est_results, cv = mc_cv)  # Find start/end dates
```

> ⚠️ **Critical Issue (RISK 2 in METHOD_REVIEW):** The test is run on the **entire dataset at once**, meaning it uses data from 2024 to determine whether 2018 was a bubble. This is called **look-ahead bias at the label level** — the most serious form, because it corrupts the thing the model is trying to learn. Correct practice: run the test recursively up to each time point, using only past data to label that date.

### Step 4 — Download and Merge Macro/Market Variables

Macro variables come from **FRED** (the St. Louis Fed database): WTI Oil price, Henry Hub natural gas price, US Dollar Index, VIX (fear index), and 10-year Treasury yield.

Market variables come from **Yahoo Finance**: gold futures, S&P 500, MSCI World ETF, MSCI Emerging Markets, a clean energy ETF (QCLN), carbon credits (EUA), and two coal stocks.

All these are merged with the metals prices by date, with weekend/holiday gaps filled using LOCF (Last Observation Carried Forward — repeating the last known value forward).

Any remaining gaps are filled using **cubic spline interpolation** — a smooth curve fitted through the known points, which fills in the blanks.

> ⚠️ **Issue (RISK 4 + Bug in METHOD_REVIEW):** 
> 1. The spline uses data from *both sides* of a gap — meaning it uses future values to fill past gaps. This is subtle leakage.
> 2. There is a **parenthesis bug** in the `tq_get()` call: `from = min(df_daly$Date, to = max(df_daly$Date))` — the `to` argument is accidentally placed *inside* `min()`. R silently ignores it, and the download runs to today's date instead of stopping at the end of the metals sample. This could pull in extra data rows that create misalignment.

### Step 5 — Build the Master Dataset

The metals prices, bubble dummies, macro data, and market data are joined into one big table called `df_master`. Bubble dummy columns are renamed with a `_BD` suffix (e.g., `CODALY_BD`). This dataset is saved to disk.

### Step 6 — Feature Engineering (`prepare_ml_data()`)

This function is the heart of the pipeline. For a given target metal (e.g., Cobalt), it:

1. Takes all numeric columns *except* the bubble dummies
2. Computes log-returns and 10-day rolling volatility for each
3. Collects the bubble dummies of the *other* three metals (these become "peer contagion" features)
4. **Lags everything by 1 day** — so the model at day *t* only sees information from day *t−1* and earlier
5. Drops the first ~11 rows (the rolling window needs 10 days of history to compute volatility)

The output is a clean matrix: rows = days, columns = features, plus the target column `Target_Bubble`.

### Step 7 — Train the Random Forest

```r
fit_rf <- ranger(
  as.factor(Target_Bubble) ~ .,
  data = select(df_ml, -Date),
  importance = "permutation",
  probability = TRUE,
  num.trees = 500
)
```

A **Random Forest** is an ensemble of decision trees. Each tree is trained on a random bootstrap sample (random rows) using a random subset of features at each split. The final prediction is the average across all 500 trees. `probability = TRUE` means it outputs a probability (0 to 1) rather than a hard yes/no.

> ⚠️ **Issue (RISK 7 in METHOD_REVIEW):** No class weighting is applied. Bubble days might be only 10% of all days. Without correction, the model will be biased toward predicting "no bubble" and will miss many real bubbles (low sensitivity).

### Step 8 — Compute SHAP Values

SHAP (SHapley Additive exPlanations) answers: *"For this specific prediction, how much did each feature contribute?"*

It is computed using `kernelshap`, using 25 background samples and explaining 100 random observations. The result is visualized as a **beeswarm plot**: each dot is one observation, the x-axis is the SHAP value (positive = pushed toward bubble prediction), and color shows the feature's actual value.

> ⚠️ **Issue (METHOD_REVIEW §5A.5):** The 25 background samples may produce noisy SHAP estimates. Also, no `set.seed()` is set before the SHAP sampling — results will differ between runs.

### Step 9 — Compute Performance Metrics

The model's OOB (Out-Of-Bag) predictions are used: each observation's prediction comes from trees that *did not* see it during training. AUC, Sensitivity, Specificity, Balanced Accuracy, and Precision are computed and formatted into a LaTeX table.

> ⚠️ **Critical Issue (RISK 1 + RISK 5 in METHOD_REVIEW):** OOB estimates use **random bootstrap sampling**, which ignores time ordering. A tree trained on 2023 data can "explain" a 2019 observation. For time-series data, this inflates all metrics. The reported AUC is likely too optimistic.

---

## metals32_lightGBM.R — LightGBM

**This script reuses the `prepare_ml_data()` function from `metals31_RF.R` and adds a gradient-boosted model as a second, stronger benchmark.**

### What Is LightGBM?

LightGBM is a **gradient boosting** algorithm. Unlike a Random Forest (which trains all trees independently), LightGBM trains trees **sequentially**: each new tree tries to fix the mistakes of the previous one. This generally gives better accuracy but requires more careful tuning.

### Step 1 — Dependencies

The script comments out a copy of `prepare_ml_data()` with the note "use the one from metals31_RF.R." This means the script **requires metals31_RF.R to have been run first** in the same R session. If you start a fresh R session and run this file alone, it will crash.

> ⚠️ **Issue (METHOD_REVIEW §5B.1):** Not self-contained — a reproducibility risk if someone else tries to replicate the results.

### Step 2 — Train LightGBM on the Full Dataset (`run_lgbm_pipeline()`)

For each metal:
1. Data is prepared via `prepare_ml_data()`
2. Converted to a numeric matrix (LightGBM requires this)
3. A LightGBM model is trained with these fixed settings:
   - `objective = "binary"` — it's a yes/no classification
   - `is_unbalance = TRUE` — automatically upweights the minority class (bubble days)
   - `feature_fraction = 0.8`, `bagging_fraction = 0.8` — subsampling for regularization
   - `nrounds = 200` — exactly 200 boosting rounds, always

> ⚠️ **Issues (RISK 6 + §5B.2 in METHOD_REVIEW):**
> 1. **No hyperparameter tuning** — the values are guesses, not optimized. Different choices could give very different results.
> 2. **No early stopping** — the model always runs 200 rounds regardless of whether it has already converged or started to overfit.

### Step 3 — Compute SHAP Values from the Full Model

SHAP values are computed natively by LightGBM's built-in TreeSHAP (faster and exact for tree models compared to the kernel method used in the RF script). A beeswarm plot is generated and saved.

> ⚠️ **Issue (§5B.3 in METHOD_REVIEW):** The SHAP plot comes from the model trained on **100% of the data**. The performance metrics (Step 4 below) come from a different set of models trained on **80% of the data** in cross-validation. The model being explained is not the model being evaluated.

### Step 4 — Evaluate via 5-Fold Cross-Validation

To get honest performance estimates, the script runs 5-fold cross-validation manually:
1. Split the dataset into 5 equal parts
2. For each part: train on the other 4 parts, predict on this part
3. Collect all out-of-fold predictions
4. Compute AUC and classification metrics on the full set of out-of-fold predictions

```r
folds <- sample(rep(1:n_folds, length.out = nrow(X)))   # Assign fold numbers
for (k in 1:n_folds) {
  # Train on 80%, predict on 20%
  bst_fold <- lgb.train(...)
  cv_preds[idx_val] <- predict(bst_fold, X[idx_val, ])
}
```

> ⚠️ **Critical Issue (RISK 1 in METHOD_REVIEW):** The fold assignment uses `sample()` with **no seed**. This shuffles the data randomly, ignoring time order. A model trained on 2022–2024 data is evaluated on its predictions for 2017 data — which is backwards in time. For time series, you should only ever test a model on data that comes *after* its training data.
>
> Additionally, because there is no seed, running the script twice gives different numbers.

### Step 5 — Compute Metrics and Generate LaTeX Table

AUC, Balanced Accuracy, Sensitivity, Specificity, and Precision are computed from the out-of-fold predictions. The optimal threshold from the ROC curve is computed but **commented out** — a fixed 0.5 threshold is used instead.

The results are formatted into a LaTeX table with `xtable`.

---

## metals33_Lasso.R — LASSO Logistic Regression

**This script serves as the "linear benchmark." Where RF and LightGBM can model complex nonlinear patterns, LASSO is restricted to straight-line relationships. If the linear model performs well, it suggests the patterns are simple. If it performs poorly, it suggests nonlinearity is important.**

### What Is LASSO?

LASSO (Least Absolute Shrinkage and Selection Operator) is **logistic regression with a penalty** that shrinks weak predictors to exactly zero. This means it automatically performs **feature selection**: out of potentially 60+ input features, it keeps only the handful that most improve predictions.

The penalty is controlled by a parameter $\lambda$ (lambda). A larger $\lambda$ means more features are zeroed out (simpler model); a smaller $\lambda$ keeps more features (more complex model).

### Step 1 — Feature Engineering (Slightly Different from RF/LightGBM)

LASSO's `prepare_ml_data()` includes one important difference: it only uses the **price levels** (not just returns/volatility) of a curated set of 8 variables — the 4 metals, VIX, 10Y yield, oil, and gas. RF and LightGBM include price levels for *all* numeric variables.

Also, it drops `VIX_Vol` (volatility of VIX), because VIX is already a volatility index — computing the volatility of volatility is redundant and noisy.

> ⚠️ **Issue (§6.1 in METHOD_REVIEW):** Because LASSO uses a smaller feature set than RF and LightGBM, any performance difference between models could be due to either the *model type* or the *different features*. This makes the three models not directly comparable as a fair benchmark.

### Step 2 — Train LASSO with Cross-Validation (`cv.glmnet`)

The `glmnet` package handles LASSO:

```r
cv_lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial",
                      type.measure = "class", standardize = TRUE,
                      maxit = 1000000, keep = TRUE)
```

- `alpha = 1` — pure LASSO (as opposed to Ridge or elastic net)
- `family = "binomial"` — logistic regression (0/1 output)
- `standardize = TRUE` — features are internally scaled to mean 0, std 1 before fitting (correct practice to ensure the penalty treats all features equally)
- `keep = TRUE` — stores the out-of-fold predictions for every tested lambda value (this is what enables honest evaluation)
- `type.measure = "class"` — selects the best lambda by minimizing classification error

> ⚠️ **Issue (RISK 7 + §5C.3 in METHOD_REVIEW):** `type.measure = "class"` optimizes for overall accuracy, which is dominated by the majority class. When 90% of days are non-bubbles, a model that always says "no bubble" achieves 90% accuracy with zero bubble detection. `type.measure = "auc"` would be more appropriate for this imbalanced problem.

### Step 3 — Extract Honest (Out-of-Fold) Predictions

This is the best part of the LASSO script. Instead of re-predicting on the training data (which would be cheating), the code extracts the stored out-of-fold predictions using `fit.preval`:

```r
idx_min    <- which(cv_lasso$lambda == cv_lasso$lambda.min)
log_odds   <- cv_lasso$fit.preval[, idx_min]
probs_vec  <- exp(log_odds) / (1 + exp(log_odds))   # Convert log-odds to probabilities
```

Each observation's prediction came from a fold where that observation was *held out during training*. This is the correct way to evaluate a model inside cross-validation.

> ⚠️ **Critical Issue (RISK 1 in METHOD_REVIEW):** Even though the predictions are out-of-fold, the folds themselves are **randomly assigned**, not time-ordered. Future data still leaks into the training of each fold.

### Step 4 — Compute Metrics and Count Selected Variables

Standard metrics (AUC, Sensitivity, etc.) are computed from the honest predictions. The number of non-zero LASSO coefficients is also reported — this shows how many features the model actually used.

### Step 5 — Plot Non-Zero Coefficients (Third Pipeline)

The third version of the pipeline adds a bar chart of the top 15 non-zero LASSO coefficients, colored by direction (orange = positive effect on bubble probability, purple = negative). This is LASSO's version of feature importance.

> ⚠️ **Issue (§5C.4 in METHOD_REVIEW):** The coefficient magnitudes are in the original data scale, not the standardized scale. A feature with a naturally large range (e.g., the S&P500 level) will have a small coefficient even if it is very influential, while a feature with a small range will have a large coefficient. This makes the bars misleading without that caveat.

---

## Cross-Script Problems

These issues span all three files and affect how the results compare to each other.

### Problem 1 — Random Cross-Validation on Time-Series Data (CRITICAL, all 3 files)

All three scripts evaluate their models by randomly shuffling observations into folds. For daily financial data, this is wrong because:
- Observations close in time are **correlated** (prices autocorrelate)
- A fold trained on 2023 data and tested on 2019 data has seen the "future" of what it is predicting

**The fix:** Use time-ordered folds — every training set must come *entirely before* the test set.

```
# Example: sequential fold assignment (not random)
foldid <- cut(seq_len(nrow(X)), breaks = 10, labels = FALSE)
cv_lasso <- cv.glmnet(X, y, ..., foldid = foldid)
```

**Impact:** All reported AUC and Balanced Accuracy values are likely inflated. They would be lower with proper time-series CV.

### Problem 2 — Bubble Labels Were Created Using the Full Dataset (CRITICAL, metals31_RF.R)

The GSADF test labels were created by looking at the entire price history at once. The start and end of a bubble in 2018 was determined using data from 2018 through 2024. In reality, you would not have known in 2018 whether that was a bubble until later.

**The fix:** Run the GSADF test **recursively** — up to each date $t$, re-run the test using only data from the beginning up to $t$. This is actually how the PSY procedure was designed to be used.

### Problem 3 — Inconsistent Feature Sets Across Models (MODERATE)

| Feature type | RF | LightGBM | LASSO |
|---|---|---|---|
| Log-returns | All variables | All variables | All variables |
| Rolling volatility | All variables | All variables | All minus VIX_Vol |
| Price levels | All variables | All variables | 8 curated only |

This means if LASSO performs worse than RF, it might be because LASSO is a linear model *or* because it had fewer features. The two causes cannot be separated.

### Problem 4 — Inconsistent Evaluation Protocols (MODERATE)

| Model | Method | Folds | Seed |
|---|---|---|---|
| RF | OOB from bootstrap | N/A | No |
| LightGBM | 5-fold CV (random) | 5 | No |
| LASSO | 10-fold CV (random) | 10 | Only in third pipeline |

Different numbers of folds and different randomization seeds make the metrics not directly comparable.

### Problem 5 — The SHAP Plots and the Metrics Come from Different Models (LightGBM only)

The SHAP plot is made from a model trained on 100% of the data. The metrics (AUC, Balanced Accuracy) come from 5 smaller models trained on 80% of the data. These are not the same model.

---

## Summary

### What Is Correctly Done ✅

| Practice | Why It's Good |
|---|---|
| **Lag-1 on all features** | Prevents the most obvious look-ahead bias |
| **Log-returns** instead of price levels | Addresses non-stationarity (trending prices) |
| **10-day rolling volatility** | A financially meaningful signal of market stress |
| **Peer bubble dummies** as features | Captures cross-metal contagion effects |
| **SHAP values** for interpretation | Theoretically grounded feature importance |
| **`keep = TRUE` in LASSO** | Correctly avoids re-predicting on training data |
| **`standardize = TRUE` in LASSO** | Prevents scale leakage between CV folds |
| **Removing VIX_Vol in LASSO** | Shows domain knowledge (VIX is already volatility) |
| **AUC and Balanced Accuracy** as metrics | Appropriate for imbalanced binary classification |

### What Is Broken ❌

| Problem | Severity | Impact |
|---|---|---|
| **Random CV folds on time-series** | 🔴 Critical | All reported metrics are unreliable/inflated |
| **Full-sample bubble detection** | 🔴 Critical | Target variable contains future information |
| **No temporal train/test split** | 🔴 Severe | No genuine out-of-sample evaluation |
| **No class weighting in RF** | 🟡 Moderate | RF biased toward predicting "no bubble" |
| **`type.measure = "class"` in LASSO** | 🟡 Moderate | Lambda selected to maximize accuracy, not bubble detection |
| **No early stopping in LightGBM** | 🟡 Moderate | Model may overfit |
| **Inconsistent feature sets** | 🟡 Moderate | Models not fairly compared |
| **SHAP from full model, metrics from CV model** | 🟡 Moderate | Inconsistency between interpretation and evaluation |
| **No seed for LightGBM CV** | 🟡 Moderate | Results not reproducible |
| **Parenthesis bug in `tq_get()`** | 🟡 Moderate | Data may extend past intended date range |
| **Global spline interpolation** | 🟡 Moderate | Subtle future leakage into features near any split boundary |
| **No baseline model** | 🟡 Moderate | No way to tell if the models beat random guessing |

### The Minimum Fixes for a Publishable Paper

1. **Replace random CV with time-ordered folds** in all three models.
2. **Re-run bubble detection recursively** (PSY procedure as intended) so labels only use past data.
3. **Add a strict temporal holdout** (last 20–25% of data, never used in training).
4. **Harmonize feature sets** across models, or clearly document and justify the differences.
5. **Add a naïve baseline** (e.g., always predict "no bubble") to contextualize the metrics.
