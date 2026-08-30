# Data Preparation Review — `metals30_data_prep.R`

## Critical Finding: Lithium Bubble Over-Detection

The GSADF test flags **50.6%** of all lithium days as "bubble" — half the dataset.
By comparison: Cobalt 10.7%, Copper 1.8%, Nickel 1.2%.

**Root cause:** `radf()` is applied to **raw price levels** instead of log prices.
Lithium went from ~$10k → $80k → $10k. The test interprets the entire uptrend as explosive,
because raw prices amplify scale differences vs the random-walk null.

**Fix:** Standard practice is to apply GSADF to `log(price)` or price-to-fundamental ratios.

## Other Issues Found

### Bug: `tq_get()` date range (lines ~393, 416)
```r
from = min(df_daly$Date, to = max(df_daly$Date))
```
The `to =` is inside `min()`. R reads this as `min(date1, date2)` = earliest date.
The `to` parameter never reaches `tq_get()`, so all data from start-of-time is downloaded.
**Impact:** Cosmetic — downloads extra data, but `left_join` trims it to `df_daly` dates.

### Double interpolation (lines ~400 → 468)
1. `df_macro` gets LOCF (`fill(..., .direction = "down")`)
2. Then `df_final` applies `na.spline()` to **all** numeric columns

Macro data is forward-filled then spline-interpolated again. No data corruption since
LOCF already fills gaps, but it's redundant and the spline could overshoot for
series with large gaps.

### Safety floor mismatch (line ~473)
```r
mutate(across(any_of(c("Oil_WTI", "Oil_Brent", "VIX", "Gas_HenryHub")), ...))
```
Column names in the dataset are `OIL_WTI`, `GAS_HHUB`, `VIX` — the `any_of()` silently
skips non-matching names, so the floor only applies to `VIX`.

## What's Fine

- Metal series selection (longest DALY series per metal) ✓
- Macro/market variable selection ✓
- Feature engineering in the ML notebook ✓
- Time-series CV with expanding window ✓

## Recommendation

The data preparation pipeline is usable. The core issue is the **bubble labeling methodology**.
Rather than re-running the R pipeline, we implement **3 alternative detection methods** in Python
that properly use log prices. See `notebooks/bubble_detection_alternatives.ipynb`.

### Alternative Methods Results (bubble % of days)

| Method | Cobalt | Copper | Lithium | Nickel |
|--------|--------|--------|---------|--------|
| GSADF (original) | 10.7% | 1.8% | **50.6%** ❌ | 1.2% |
| HMM Regime Switching | 0.0% | 0.0% | 16.7% | 0.0% |
| LPPLS (Sornette) | 0.7% | 1.7% | 1.0% | 1.3% |
| CUSUM (rolling) | 13.0% | 14.2% | 23.6% | 8.3% |

**CUSUM** provides the most balanced detection across all metals.
