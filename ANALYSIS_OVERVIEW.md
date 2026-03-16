# Metals Repository: Analysis Overview

**Repository:** Econometric analysis of metals markets (Cobalt, Copper, Lithium, Nickel)  
**Last Updated:** February 10, 2026

---

## Table of Contents

1. [Data Loading & Preprocessing](#1-data-loading--preprocessing)
2. [Descriptive Statistics](#2-descriptive-statistics)
3. [Correlation Analysis](#3-correlation-analysis)
4. [Cointegration Analysis](#4-cointegration-analysis)
5. [Speculative Bubble Detection](#5-speculative-bubble-detection)
6. [DCC GARCH Modeling](#6-dcc-garch-modeling)
7. [Unit Root Tests](#7-unit-root-tests)
8. [Volatility Estimation](#8-volatility-estimation)
9. [Plotting & Visualization](#9-plotting--visualization)
10. [Other Analyses](#10-other-analyses)
11. [Data Files](#11-data-files)

---

## 1. Data Loading & Preprocessing

### Primary Files

#### [fetch_dailymetalprice.py](fetch_dailymetalprice.py)
**Purpose:** Web scraping and data collection  
**Methods:**
- Automated scraping from dailymetalprice.com using Selenium/Firefox
- Collects daily price data for Nickel, Copper, Lithium, and Cobalt
- Automated date range collection

#### [data_overview2.ipynb](data_overview2.ipynb)
**Purpose:** Data consolidation  
**Methods:**
- Merges multiple CSV files for each metal into single datasets
- Date synchronization across different series

#### [data_overview.ipynb](data_overview.ipynb)
**Purpose:** Missing data handling comparison  
**Methods:**
- Compares interpolation methods:
  - Linear interpolation
  - Cubic spline interpolation
  - Kalman filter interpolation
  - Outer join approach
- LISAME series exclusion (problematic lithium series)
- Missing value visualization

#### barb_experiments/ Scripts
**Files:**
- `cobalt_data_extraction.py`
- `copper_data_extraction.py`
- `lithium_data_extraction.py`
- `plot_nickel_series.py`
- `rename_nickel_series.py`

**Purpose:** Individual metal data extraction and processing

**Key Operations Across All Files:**
- Date synchronization across metals
- Missing value handling (forward filling, max 5 days)
- Weekend exclusion (trading days only)
- Data quality assessment

---

## 2. Descriptive Statistics

### Primary Files

#### [descriptive_statistics.py](descriptive_statistics.py)
**Purpose:** Comprehensive returns statistics computation  
**Output:**
- Mean returns per series
- Standard deviation
- Minimum, Q1, Q3, Maximum
- Observation counts
- LaTeX table generation for publication

**Data:** All metal price series (cubic spline interpolated)

#### [gold_overview.ipynb](gold_overview.ipynb)
**Purpose:** Gold market analysis  
**Data Sources:**
- Osaka Exchange
- Multi Commodity Exchange of India

**Metrics:**
- Quartiles and extremes
- Outlier detection
- Volatility levels
- Ticker mapping for display names

---

## 3. Correlation Analysis

### Primary Files

#### [cross_metal_analysis.ipynb](cross_metal_analysis.ipynb) ⭐ **COMPREHENSIVE**
**Purpose:** Cross-metal correlation analysis and network visualization  
**Methods:**
- Full correlation matrices (24 series across all 4 metals)
- Kendall correlation coefficient
- Network visualization using NetworkX
- Community detection (Louvain method)
- Heatmaps with RdYlGn colormap
- Within-metal vs cross-metal correlation comparison
- Long-history series filtering (≥3 years of data)

**Outputs:**
- Correlation matrices
- Network graphs (spring layout)
- Community structure analysis
- Statistical comparisons

#### [time_horizon_correlation_analysis.ipynb](time_horizon_correlation_analysis.ipynb)
**Purpose:** Time horizon-specific correlation patterns  
**Methods:**
- Analysis across different time periods
- Correlation stability assessment

#### [barb_experiments/metals_rolling_cor.py](barb_experiments/metals_rolling_cor.py)
**Purpose:** Rolling window correlation analysis  
**Windows:**
- 30-day rolling correlation
- 60-day rolling correlation
- 90-day rolling correlation

**Methods:**
- Time-varying correlation computation
- Correlation dynamics visualization

#### [barb_experiments/correlation_graphs.py](barb_experiments/correlation_graphs.py)
**Purpose:** Advanced correlation visualization  
**Outputs:**
- Heatmaps
- Network graphs with threshold-based edges
- Hierarchical clustering dendrograms
- Top correlated pairs identification

---

## 4. Cointegration Analysis

### Primary Files

#### [cointegration.ipynb](cointegration.ipynb) ⭐ **DEDICATED**
**Purpose:** Comprehensive cointegration testing  
**Methods:**
- Engle-Granger cointegration tests
- Tests performed on **log-prices** (not returns)
- All pairwise combinations within each metal
- Separate analysis for:
  - Cobalt pairs
  - Copper pairs
  - Lithium pairs
  - Nickel pairs

**Features:**
- LISAME exclusion
- Statistical significance testing (p < 0.05)
- LaTeX tables for results
- P-value interpretation with color coding
- **Note:** Within-metal pairs only (no cross-metal testing)

---

## 5. Speculative Bubble Detection

### Primary Files

#### [exuber.R](exuber.R) ⭐ **PRIMARY**
**Purpose:** Statistical bubble detection using explosive root testing  
**Methods:**
- RADF (Recursive Augmented Dickey-Fuller) estimation
- SADF statistic computation
- GSADF statistic computation
- Monte Carlo critical value generation
- Bubble period datestamping

**Implementation:**
- Log-price transformation
- Multiple series testing (NI, CU, LI, CO)
- Specific column exclusions (NIETFN, CUETFC, etc.)
- Date synchronization (2021-07-19 to 2025-07-21)
- Seed=123 for reproducibility

**Outputs:**
- 0-1 dummy variables for detected bubble periods
- Visualization with grey shaded bubble periods
- Export to [bubble_dummies.csv](data/bubble_dummies.csv) for GARCH modeling
- Stored results in `mc_cv.rds`

---

## 6. DCC GARCH Modeling

### Primary Files

#### [DCC_GARCH_new.ipynb](DCC_GARCH_new.ipynb) ⭐ **COMPREHENSIVE**
**Purpose:** Dynamic Conditional Correlation GARCH modeling  
**Methods:**
1. **Univariate GARCH(1,1)** for each series
   - Using `arch_model` from Python's arch package
   - Standardized residuals extraction
2. **DCC Estimation:**
   - DCC parameter estimation (alpha, beta)
   - Negative log-likelihood optimization
3. **Dynamic Correlations:**
   - Time-varying conditional correlations
   - Dynamic covariance matrices

**Features:**
- Log returns calculation
- Multivariate GARCH with dynamic correlations
- Visualization of time-varying correlations

#### [DCC_GARCH.ipynb](DCC_GARCH.ipynb)
**Purpose:** Earlier version of DCC-GARCH implementation

#### [GARCH.Rmd](GARCH.Rmd)
**Purpose:** R implementation of GARCH analysis  
**Methods:**
- ARMA model selection for returns (ARMA(1,0) vs ARMA(0,1))
- Univariate GARCH fitting using rugarch package
- Within-metal correlation analysis
- AIC-based model selection

**Outputs:**
- Stored in `garch_results_list.rds`

---

## 7. Unit Root Tests

### Primary Files

#### [unit_root_tests.Rmd](unit_root_tests.Rmd) ⭐ **COMPREHENSIVE**
**Purpose:** Stationarity testing for all metal series  
**Methods:**
- **ADF (Augmented Dickey-Fuller) tests**
- **KPSS tests**
- Tests on **percentage returns** (not prices)

**Implementation:**
- R packages: urca, tseries
- Returns calculation: `(Price_t - Price_{t-1}) / Price_{t-1} * 100`
- NA handling with omission
- Separate analysis for all 4 metals

**Features:**
- Critical value comparisons at 1%, 5%, 10% levels
- Detailed interpretation functions
- Null hypothesis rejection analysis

**Output:**
- HTML report: [unit_root_tests.html](unit_root_tests.html)

---

## 8. Volatility Estimation

### Primary Files

#### [GK_estimator.ipynb](GK_estimator.ipynb)
**Purpose:** Garman-Klass volatility estimator implementation  
**Data Required:**
- High, Low, Open, Close prices (HLOC data from Bloomberg)
- Fields: PX_HIGH, PX_LOW, PX_OPEN, PX_LAST

**Methods:**
- Garman-Klass formula implementation
- Log returns calculation
- Rolling 252-day window standard deviation
- Annualized volatility (scaling by 252^0.5)

**Metals Analyzed:**
- Cobalt
- Copper

**Features:**
- Data cleaning for European number formats (comma to period conversion)
- Rolling window volatility computation

---

## 9. Plotting & Visualization

### Primary Files

#### [all_metals_synchronized_plots.py](all_metals_synchronized_plots.py)
**Purpose:** Synchronized multi-panel time series visualization  
**Features:**
- 2x3 subplot layout
- Missing value indicators (red markers)
- Synchronized time scales across all metals
- Blue color palette for price series

#### barb_experiments/ Visualization Scripts
**Files:**
- `cobalt_data_extraction synchronized plots.py`
- `copper_data_extraction synchronized plots.py`
- `lithium_data_extraction synchronized plots.py`
- `nickel_data_extraction synchronized plots.py`
- `plot_nickel_series.py`

**Purpose:** Individual metal synchronized plots

#### [barb_experiments/SVB.py](barb_experiments/SVB.py)
**Purpose:** Correlation transformation visualization

### Visualization Types Across Repository

1. **Time Series Plots:**
   - Price series over time
   - Missing data indicators
   - Bubble period shading (grey zones)

2. **Correlation Visualizations:**
   - Heatmaps (RdYlGn colormap)
   - Network graphs (spring layout, force-directed)
   - Rolling correlation plots
   - Hierarchical dendrograms

3. **Statistical Plots:**
   - ACF/PACF plots
   - Scatter plots with regression lines
   - Distribution histograms

4. **Comparative Plots:**
   - Within-metal vs cross-metal correlations
   - All series vs long-history series comparisons

**Output Directories:**
- `barb_experiments/plots_moving_cor/`
- `graphsR/`
- `img/`
- `outputs/`

---

## 10. Other Analyses

### Additional Statistical Methods

#### Autocorrelation Testing
**File:** [unit_root_tests.Rmd](unit_root_tests.Rmd)  
**Method:** Ljung-Box tests for returns autocorrelation

#### Model Selection
**File:** [GARCH.Rmd](GARCH.Rmd)  
**Method:** AIC-based ARMA order selection

#### Date Range Identification
**File:** [data_overview.ipynb](data_overview.ipynb)  
**Method:** Common timeframe finding with 90% coverage threshold

#### Data Quality Tools
- Forward filling (max 5 days gap)
- Weekend exclusion (trading day filtering)
- Ticker mapping (ticker_mapping.json)
- Series name standardization

#### LaTeX Table Generation
**Files:** Multiple files generate publication-ready tables
- Descriptive statistics tables
- Cointegration test results
- GARCH model outputs

---

## 11. Data Files

### Input Data Structure

**Location:** `data/` folder

#### Processed Price Data (by interpolation method):
For each metal (Cobalt, Copper, Lithium, Nickel):
- `ALL_{metal}_prices_cubic_spline.csv` ⭐ **PRIMARY**
- `ALL_{metal}_prices_linear.csv`
- `ALL_{metal}_prices_interpolated.csv`
- `ALL_{metal}_prices_kalman.csv`
- `ALL_{metal}_prices_inner.csv`
- `ALL_{metal}_prices_outer.csv`

#### Special Files:
- `ALL_cobalt_prices_PREPARED.csv` - Final prepared Cobalt data
- `battery_grade_overlap.csv` - Battery-grade metal overlap analysis
- `bubble_dummies.csv` - Bubble period indicators (from exuber.R)

#### Raw Data Files:
- Individual metal price files by date range
- Example: `Cobalt_prices_2017-01-01_to_2024-12-31_merged.csv`

### Metadata Files

- `barb_experiments/ticker_mapping.json` - Series name mappings
- `barb_experiments/ticker_mapping.md` - Documentation
- `requirements.txt` - Python dependencies
- `environment.yml` - Conda environment
- `metals.Rproj` - R project file

### Saved Analysis Results

- `garch_results_list.rds` - Saved GARCH model results
- `mc_cv.rds` - Monte Carlo critical values for bubble tests
- `sp500prCl.csv` - S&P 500 data for comparison

---

## Summary: Analysis Pipeline

### Workflow Overview

```
1. DATA COLLECTION (fetch_dailymetalprice.py)
   ↓
2. DATA PREPROCESSING (data_overview.ipynb, data_overview2.ipynb)
   ↓
3. DESCRIPTIVE STATISTICS (descriptive_statistics.py)
   ↓
4. PARALLEL ANALYSES:
   ├─ CORRELATION (cross_metal_analysis.ipynb, rolling correlations)
   ├─ COINTEGRATION (cointegration.ipynb)
   ├─ BUBBLES (exuber.R)
   ├─ UNIT ROOTS (unit_root_tests.Rmd)
   └─ VOLATILITY (GK_estimator.ipynb)
   ↓
5. ADVANCED MODELING (DCC_GARCH_new.ipynb, GARCH.Rmd)
   ↓
6. VISUALIZATION (multiple plotting scripts)
```

### Key Methodological Choices

1. **Interpolation:** Cubic spline preferred for missing data
2. **Returns Calculation:** Percentage returns for most analyses; log-prices for cointegration
3. **Correlation Method:** Kendall correlation (robust to outliers)
4. **Stationarity:** Tests on returns (not prices)
5. **GARCH:** Univariate GARCH(1,1) as building block for DCC
6. **Bubble Detection:** RADF/SADF/GSADF with Monte Carlo critical values
7. **Network Analysis:** Louvain community detection, force-directed layouts

---

## File Count Summary

- **Python Scripts:** ~10 files
- **Jupyter Notebooks:** ~10 files
- **R Scripts/Markdown:** ~3 files
- **Data Files:** ~60+ CSV files
- **Output Plots:** Stored in multiple directories

---

## Notes

- **LISAME series** consistently excluded from Lithium analysis (data quality issues)
- **Long-history filtering** (≥3 years) used for robust correlation analysis
- **Weekends excluded** from all time series analyses
- **Reproducibility:** Seeds set for stochastic procedures (e.g., seed=123 in bubble tests)
- **Multiple implementations:** Some analyses have both Python and R versions for validation

---

## Repository Structure Highlights

```
metals/
├── Data Processing Scripts (Python)
├── Analysis Notebooks (Jupyter)
├── Statistical Tests (R/Rmd)
├── data/ (Processed datasets)
├── barb_experiments/ (Auxiliary analyses)
├── outputs/ (Results)
└── Visualization directories (plots_moving_cor/, graphsR/, img/)
```

---

**End of Analysis Overview**
