# Changed Files - Repository Reorganization

## Files with Path Changes

### Python Scripts (4 files)
1. **scripts/data_acquisition/fetch_dailymetalprice.py**
   - Changed: `"data/"` → `"../../data/"`
   - Lines: 104, 105

2. **scripts/analysis/descriptive_statistics.py**
   - Added: `PROJECT_ROOT = Path(__file__).parent.parent`
   - Changed: `'barb_experiments/ticker_mapping.json'` → `PROJECT_ROOT / 'config' / 'ticker_mapping.json'`
   - Changed: `'data/...'` → `PROJECT_ROOT / 'data' / '...'`
   - Lines: Multiple

3. **scripts/analysis/all_metals_synchronized_plots.py**
   - Added: `PROJECT_ROOT = Path(__file__).parent.parent`
   - Changed: `'barb_experiments/ticker_mapping.json'` → `PROJECT_ROOT / 'config' / 'ticker_mapping.json'`
   - Changed: `'outputs/synchronized_plots'` → `PROJECT_ROOT / 'outputs' / 'synchronized_plots'`
   - Changed: `'data/...'` → `PROJECT_ROOT / 'data' / '...'`
   - Lines: Multiple

4. **scripts/utils/merge_csv.py**
   - Changed: `"data"` → `"../../data"` (3 occurrences)
   - Lines: 12, 22, 30

### R Scripts (5 files)
1. **R/johan_cointegration.R**
   - Changed: `here("johansen_results_final.tex")` → `here("outputs", "tables", "johansen_results_final.tex")`
   - Line: 480

2. **R/exuber.R**
   - Changed: `"mc_cv.rds"` → `here("outputs", "R_objects", "mc_cv.rds")` (2 occurrences)
   - Changed: `here("graphsR", ...)` → `here("outputs", "figures", "bubble_tests", ...)` (5 occurrences)
   - Changed: `"graphsR/bubble_*.pdf"` → `here("outputs", "figures", "bubble_tests", "bubble_*.pdf")` (3 occurrences)
   - Lines: 159, 162, 254, 346, 488, 677, 682, 687

3. **R/dcc_garch.R**
   - No changes (already uses `here()` correctly)

4. **R/unit_root_tests.Rmd**
   - Changed: `knitr::opts_knit$set(root.dir = getwd())` → `knitr::opts_knit$set(root.dir = here::here())`
   - Changed: `read.csv("data/...")` → `read.csv(here::here("data", "..."))` (4 occurrences)
   - Changed: `save_kable("outputs/...")` → `save_kable(here::here("outputs", ...))` (4 occurrences)
   - Lines: 10, 23-26, 264, 414, 564, 714

5. **R/GARCH.Rmd**
   - Changed: `read.csv("data/...")` → `read.csv(here::here("data", "..."))` (4 occurrences)
   - Changed: `ggsave("graphsR/...")` → `ggsave(here::here("outputs", "figures", "dcc_garch", "..."))` (2 occurrences)
   - Lines: 17-20, 605, 1061

### Jupyter Notebooks (7 files)
1. **notebooks/exploratory/data_overview.ipynb**
   - Changed: `'data/'` → `'../../data/'` (48+ occurrences)
   - Changed: `'img/'` → `'../../img/'` (6 occurrences)
   - Changed: `'barb_experiments/ticker_mapping.json'` → `'../../config/ticker_mapping.json'`
   - Total: 66+ path references updated

2. **notebooks/exploratory/data_overview2.ipynb**
   - Changed: `'data/'` → `'../../data/'` (4 occurrences)

3. **notebooks/exploratory/gold_overview.ipynb**
   - Changed: Hardcoded `/Users/michal/Documents/Code/metals/...` → Relative paths (6 occurrences)

4. **notebooks/analysis/time_horizon_correlation_analysis.ipynb**
   - Already had correct `'../../data/'` paths (no changes needed)

5. **notebooks/analysis/cross_metal_analysis.ipynb**
   - Already had correct `'../../data/'` paths for data loading
   - Changed: `'barb_experiments/plots_moving_cor/'` → `'../../barb_experiments/plots_moving_cor/'` (2 cells)

6. **notebooks/analysis/cointegration.ipynb**
   - Already had correct `'../../data/'` paths (no changes needed)

7. **notebooks/analysis/GK_estimator.ipynb**
   - Changed: Hardcoded `/Users/michal/Documents/Code/metals/bloomberg_data/...` → Relative paths (5 occurrences)

### barb_experiments/ Scripts (5 files)
1. **barb_experiments/copper_data_extraction synchronized plots.py**
   - Changed: `'barb_data/ticker_mapping.json'` → `'../config/ticker_mapping.json'`
   - Line: 413

2. **barb_experiments/cobalt_data_extraction synchronized plots.py**
   - Changed: `'barb_data/ticker_mapping.json'` → `'../config/ticker_mapping.json'`
   - Line: 413

3. **barb_experiments/correlation_graphs.py**
   - Changed: `os.path.join('barb_experiments', 'ticker_mapping.json')` → `os.path.join('config', 'ticker_mapping.json')`
   - Line: 248

4. **barb_experiments/lithium_data_extraction synchronized plots.py**
   - Changed: `'ticker_mapping.json'` → `'../config/ticker_mapping.json'`
   - Line: 419

5. **barb_experiments/nickel_data_extraction synchronized plots.py**
   - Changed: `'barb_data/ticker_mapping.json'` → `'../config/ticker_mapping.json'`
   - Line: 413

## Total Files Modified
- **Python Scripts**: 4
- **R Scripts**: 5
- **Jupyter Notebooks**: 7
- **barb_experiments/ Scripts**: 5
- **Total**: 21 files

## Files That Reference Moved Files

### Files Referencing ticker_mapping.json (now in config/)
1. scripts/analysis/descriptive_statistics.py
2. scripts/analysis/all_metals_synchronized_plots.py
3. barb_experiments/copper_data_extraction synchronized plots.py
4. barb_experiments/cobalt_data_extraction synchronized plots.py
5. barb_experiments/correlation_graphs.py
6. barb_experiments/lithium_data_extraction synchronized plots.py
7. barb_experiments/nickel_data_extraction synchronized plots.py
8. notebooks/exploratory/data_overview.ipynb

### Files Referencing outputs/ folder (moved/reorganized)
1. R/johan_cointegration.R → outputs/tables/
2. R/exuber.R → outputs/R_objects/, outputs/figures/bubble_tests/
3. R/unit_root_tests.Rmd → outputs/unit_root_tables/
4. R/GARCH.Rmd → outputs/figures/dcc_garch/
5. scripts/analysis/all_metals_synchronized_plots.py → outputs/synchronized_plots/
6. notebooks/analysis/cross_metal_analysis.ipynb → barb_experiments/plots_moving_cor/

### Files Referencing data/ folder (no move, but paths updated)
- All 4 Python scripts
- All 5 R scripts
- All 7 Jupyter notebooks

## Testing Status
- ✅ scripts/analysis/descriptive_statistics.py - TESTED, WORKING
- ⏳ All other files - PENDING TESTING
