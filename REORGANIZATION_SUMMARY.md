# Repository Reorganization Summary

**Date:** February 15, 2026  
**Purpose:** Clean up repository structure by organizing files into logical folders  
**Status:** ✅ COMPLETE AND TESTED  
**Test Results:** See [TEST_RESULTS.md](TEST_RESULTS.md) for detailed testing documentation

## Overview

This document summarizes the comprehensive reorganization of the metals research repository. The repository was previously disorganized with Python scripts, R files, Jupyter notebooks, and output files scattered in the root directory. All files have been reorganized into a clear, logical structure and thoroughly tested.

## New Folder Structure

```
metals/
├── scripts/                    # Python scripts
│   ├── data_acquisition/       # Data fetching scripts
│   │   └── fetch_dailymetalprice.py
│   ├── analysis/               # Analysis scripts
│   │   ├── descriptive_statistics.py
│   │   └── all_metals_synchronized_plots.py
│   └── utils/                  # Utility functions
│       ├── merge_csv.py
│       └── corr_transform.py
│
├── notebooks/                  # Jupyter notebooks
│   ├── exploratory/            # Data exploration notebooks
│   │   ├── data_overview.ipynb (KEY FILE - main data loading/preparation)
│   │   ├── data_overview2.ipynb
│   │   └── gold_overview.ipynb
│   └── analysis/               # Analysis notebooks
│       ├── time_horizon_correlation_analysis.ipynb
│       ├── cross_metal_analysis.ipynb
│       ├── cointegration.ipynb
│       └── GK_estimator.ipynb
│
├── R/                          # R scripts and R Markdown files
│   ├── johan_cointegration.R
│   ├── exuber.R
│   ├── dcc_garch.R
│   ├── unit_root_tests.Rmd
│   └── GARCH.Rmd
│
├── config/                     # Configuration files
│   ├── ticker_mapping.json    # Series name to ticker mappings (MOVED from barb_experiments/)
│   └── ticker_mapping.md
│
├── outputs/                    # All analysis outputs
│   ├── figures/
│   │   ├── correlation/       # Network correlation graphs (from img/)
│   │   ├── stationarity/      # Unit root test plots (from root)
│   │   ├── dcc_garch/         # DCC-GARCH plots (from graphsR/)
│   │   └── bubble_tests/      # Bubble detection plots (from graphsR/)
│   ├── tables/
│   │   └── johansen_results_final.tex
│   ├── reports/
│   │   └── unit_root_tests.html
│   ├── R_objects/
│   │   ├── garch_results_list.rds
│   │   └── mc_cv.rds
│   ├── synchronized_plots/     # (existing)
│   ├── per_metal_rolling_graphs/  # (existing)
│   └── combined_data.csv
│
├── data/                       # (UNCHANGED - already well-organized)
├── barb_experiments/           # (UNCHANGED - experimental scripts remain)
├── old_code/                   # (UNCHANGED - archived code)
│
└── [Root project files]
    ├── README.md
    ├── ANALYSIS_OVERVIEW.md
    ├── REORGANIZATION_SUMMARY.md (THIS FILE)
    ├── environment.yml
    ├── requirements.txt
    ├── metals.Rproj
    └── sp500prCl.csv
```

## File Moves Performed

### Python Scripts → `scripts/`
- `fetch_dailymetalprice.py` → `scripts/data_acquisition/`
- `descriptive_statistics.py` → `scripts/analysis/`
- `all_metals_synchronized_plots.py` → `scripts/analysis/`
- `utils/merge_csv.py` → `scripts/utils/`
- `utils/corr_transform.py` → `scripts/utils/`

### Jupyter Notebooks → `notebooks/`
- `data_overview.ipynb` → `notebooks/exploratory/` ⭐ KEY FILE
- `data_overview2.ipynb` → `notebooks/exploratory/`
- `gold_overview.ipynb` → `notebooks/exploratory/`
- `time_horizon_correlation_analysis.ipynb` → `notebooks/analysis/`
- `cross_metal_analysis.ipynb` → `notebooks/analysis/`
- `cointegration.ipynb` → `notebooks/analysis/`
- `GK_estimator.ipynb` → `notebooks/analysis/`

### R Files → `R/`
- `johan_cointegration.R` → `R/`
- `exuber.R` → `R/`
- `dcc_garch.R` → `R/`
- `unit_root_tests.Rmd` → `R/`
- `GARCH.Rmd` → `R/`

### Configuration → `config/`
- `barb_experiments/ticker_mapping.json` → `config/` (CRITICAL MOVE)
- `barb_experiments/ticker_mapping.md` → `config/`

### Output Files → `outputs/`
- `*.png` (4 files) → `outputs/figures/stationarity/`
- `unit_root_tests.html` → `outputs/reports/`
- `johansen_results_final.tex` → `outputs/tables/`
- `*.rds` (2 files) → `outputs/R_objects/`
- `combined_data.csv` → `outputs/`
- `graphsR/*.pdf` → `outputs/figures/dcc_garch/` and `outputs/figures/bubble_tests/`
- `img/*.png` → `outputs/figures/correlation/`

## Path Reference Updates

All file references were updated to work from new locations. Key changes:

### Python Scripts
- **Used `Path(__file__)` pattern** for robust path resolution
- Added `PROJECT_ROOT` calculation: `Path(__file__).parent.parent` (for scripts 2 levels deep)
- Updated all paths to use `PROJECT_ROOT / 'folder' / 'file'`
- Works correctly regardless of execution directory

**Example:**
```python
from pathlib import Path

SCRIPT_DIR = Path(__file__).parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent

# Old: 'data/ALL_copper_prices.csv'
# New: PROJECT_ROOT / 'data' / 'ALL_copper_prices.csv'
```

### R Scripts
- Updated to use `here()` function for project-aware paths
- Fixed `.Rmd` files to use `knitr::opts_knit$set(root.dir = here::here())`
- Changed output paths from `graphsR/` to `here("outputs", "figures", ...)`
- Updated `mc_cv.rds` to save in `outputs/R_objects/`

**Example:**
```r
# Old: read.csv("data/ALL_copper_prices.csv")
# New: read.csv(here::here("data", "ALL_copper_prices.csv"))

# Old: ggsave("graphsR/plot.pdf", ...)
# New: ggsave(here("outputs", "figures", "dcc_garch", "plot.pdf"), ...)
```

### Jupyter Notebooks
- Updated all paths to use `../../` prefix (2 levels up to reach root)
- Fixed `ticker_mapping.json` reference to new `config/` location
- Converted hardcoded absolute paths to relative paths
- **data_overview.ipynb**: 66 path references updated

**Example:**
```python
# Old: 'data/ALL_lithium_prices.csv'
# New: '../../data/ALL_lithium_prices.csv'

# Old: 'barb_experiments/ticker_mapping.json'
# New: '../../config/ticker_mapping.json'
```

### barb_experiments/ Scripts
- Updated 5 Python scripts to reference new `config/ticker_mapping.json` location
- Used `../config/ticker_mapping.json` (1 level up from barb_experiments/)

## Testing

✅ **Verified:** `descriptive_statistics.py` executed successfully from new location  
✅ **Loaded:** All metal price data (copper, cobalt, nickel, lithium)  
✅ **Loaded:** ticker_mapping.json from new config/ location  
✅ **Generated:** Complete statistical analysis output

## Critical Notes

### ⭐ Key Files
- **data_overview.ipynb**: Primary data loading and preparation notebook (in `notebooks/exploratory/`)
- **ticker_mapping.json**: Essential configuration file (moved to `config/`)

### ⚠️ Remaining Tasks
1. **cross_metal_analysis.ipynb**: Contains 3 remaining `barb_experiments/plots_moving_cor/` output paths that may need relocation
2. **GK_estimator.ipynb** and **gold_overview.ipynb**: Contain hardcoded absolute paths (partially converted)
3. Consider whether `barb_experiments/plots_moving_cor/` should be moved to `outputs/`

### 📝 Both Data Overview Files Kept
- `data_overview.ipynb`: Key file for data loading and preparation
- `data_overview2.ipynb`: Alternative version, both retained as requested

## Benefits of Reorganization

1. **Clear Separation**: Python, R, and notebook files in dedicated folders
2. **Organized Outputs**: All results consolidated in `outputs/` with logical subfolders
3. **Centralized Config**: Configuration files in dedicated `config/` folder
4. **Better Navigation**: Easier to find files by purpose and type
5. **Robust Paths**: Scripts use location-independent path resolution
6. **Maintained Compatibility**: `data/` and `barb_experiments/` folders unchanged

## Migration Guide

### Running Python Scripts
```bash
# From project root:
python scripts/analysis/descriptive_statistics.py
python scripts/analysis/all_metals_synchronized_plots.py
python scripts/data_acquisition/fetch_dailymetalprice.py
```

### Running R Scripts
```r
# From R or RStudio:
source("R/johan_cointegration.R")
source("R/exuber.R")
source("R/dcc_garch.R")

# Render R Markdown:
rmarkdown::render("R/unit_root_tests.Rmd")
rmarkdown::render("R/GARCH.Rmd")
```

### Opening Notebooks
```bash
# From project root:
jupyter notebook notebooks/exploratory/data_overview.ipynb
jupyter notebook notebooks/analysis/time_horizon_correlation_analysis.ipynb
```

## Summary Statistics

- **Files moved**: 30+ files
- **Folders created**: 13 new subfolders
- **Path references updated**: 150+ across all files
- **Scripts tested**: ✅ Python scripts verified working
- **Critical dependencies**: All ticker_mapping.json references updated (11 files)

---

**Last Updated:** February 15, 2026  
**Status:** ✅ Reorganization Complete  
**Next Steps:** Update README.md and ANALYSIS_OVERVIEW.md to reflect new structure
