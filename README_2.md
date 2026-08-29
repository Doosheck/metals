# Tracing Financialisation in Critical Metal Markets

Replication package for the paper *"Tracing Financialisation in Critical Metal Markets:
Correlations, Bubbles, and Dynamic Linkages"* by Barbara Będowska-Sójka (Poznań University
of Economics and Business) and Michał Kasperek.

<!-- TODO before making the repository public: add the journal, year, DOI and the final
     citation once the paper is accepted. -->

The study tests four hypotheses on the financialisation of copper, cobalt, lithium and
nickel across sixteen daily price series from 19 July 2021 to 21 July 2025, combining a
correlation network, the GSADF bubble test, Johansen cointegration with bubble episodes
filtered out, and DCC-GARCH models.

---

## Repository contents

```
.
├── data/
│   └── combined_metals_cleaned3.csv    # the price panel (the only input file)
├── R/
│   ├── m11_exuber.R                    # network, GSADF bubbles, descriptives, H1
│   ├── m12_johansen.R                  # cointegration, raw vs bubble-filtered
│   ├── m13_dcc_garch.R                 # univariate GARCH-X and DCC-GARCH
│   ├── results_R/metals1/              # tables and CSV output (generated)
│   └── graphs_R/metals1/               # figures (generated)
├── run_all.R                           # runs the three scripts in order
├── README.md
├── LICENSE
└── .gitignore
```

Everything under `R/results_R/metals1/` and `R/graphs_R/metals1/` is produced by the
scripts. Only `data/combined_metals_cleaned3.csv` is an input.

The `metals1/` level exists so that the same scripts can run unchanged inside a larger
working directory shared with other projects, without output file names colliding. Each
script sets it once, at the top:

```r
RESULT_DIR <- here("R", "results_R", "metals1")
GRAPH_DIR  <- here("R", "graphs_R",  "metals1")
```

Generated CSV, `.tex` and PDF files are tracked in this repository so that the published
numbers can be compared against a fresh run. Fitted model objects (`.rds`) and the 300 dpi
TIFF versions of the figures are not tracked; both are rebuilt by the scripts.

---

## How to run

Open `metals1-replication.Rproj` in RStudio — this sets the working directory, which the
scripts resolve with `here()` — then:

```r
source("run_all.R")
```

or run the three scripts individually, in this order:

| Script | Produces | Approximate runtime |
|---|---|---|
| `R/m11_exuber.R` | Figures 1–2, Tables 2–3, `series_and_bubble_up.csv` | minutes; the first run is longer because it simulates the GSADF critical values |
| `R/m12_johansen.R` | Table 4 | minutes |
| `R/m13_dcc_garch.R` | Figures 3–5, DCC parameters | tens of minutes |

`m11` must run first: `m12` and `m13` both read the bubble-dummy file it writes. They are
independent of each other and can run in either order.

`m11` caches the GSADF Monte Carlo critical values in
`R/results_R/metals1/R_objects/mc_cv_2021_2025.rds` (seed 123). The first run computes
them; later runs load the cache. The `exuberdata` package is therefore not required.

### Requirements

R 4.2 or later and the following packages:

```r
install.packages(c(
  "tidyverse", "here",                    # all scripts
  "exuber", "moments", "FinTS", "urca",   # m11
  "knitr", "kableExtra", "igraph", "ggraph",
  "vars",                                 # m12
  "rugarch", "rmgarch"                    # m13
))
```

<!-- Optional but recommended: run renv::init() and renv::snapshot() once, then commit
     renv.lock so that package versions are pinned. -->

---

## Data

Sixteen daily price series, four per metal, drawn from exchanges and commercial data
providers. Series names are constructed as a two-letter metal prefix plus a source code.

| Metal | Series | Source |
|---|---|---|
| Cobalt | CODALY, COLMEX, COWUXI, COCOMX | dailymetalprice.com, LME, WUXI, COMEX |
| Copper | CUDALY, CUCOMX, CULMEX, CUSHFE | dailymetalprice.com, COMEX, LME, SHFE |
| Lithium | LIDALY, LICOMX, LILMEX, LIANTH | dailymetalprice.com, COMEX, LME, Antaike |
| Nickel | NIDALY, NILMEX, NISHFE, NIWUXI | dailymetalprice.com, LME, SHFE, WUXI |

LME = London Metal Exchange, COMEX = Commodity Exchange (CME Group), SHFE = Shanghai
Futures Exchange, WUXI = Wuxi Stainless Steel Exchange, Antaike = Chinese metals
information provider. Prices were retrieved via Bloomberg, Reuters and
dailymetalprice.com; full units and maturities are given in Table 1 of the paper.

The sample window 2021-07-19 to 2025-07-21 is the longest over which daily data are
simultaneously available for all sixteen series, bounded by the July 2021 launch of the
lithium hydroxide instruments. Missing observations were filled by cubic-spline
interpolation, leaving 1,043 daily prices and 1,042 daily log returns per series.

`combined_metals_cleaned3.csv` is comma-separated with a dot decimal mark: a `Date` column
(`YYYY-MM-DD`) followed by the sixteen price columns. **The column order matters**: the
correlation network uses a seeded force-directed layout, so reordering the columns produces
a different — equally valid, but not identical — Figure 1.

---

## Output map

| Paper object | Script | File |
|---|---|---|
| Table 2, descriptive statistics | m11 | `metals1_tab2.csv`, `metals1_tab2.tex` |
| Table 3, bubble episodes | m11 | `bubble_summary_up.csv`, `metals1_tab3.tex` |
| Table 4, Johansen tests | m12 | `metals1_tab4.csv`, `metals1_tab4.tex` |
| Figure 1, correlation network | m11 | `Metal_Network_21_25_thresh_75p_5color1.pdf` |
| Figure 2, prices and bubble episodes | m11 | `bubbles_up_only1.pdf` |
| Figures 3–4, DCC within copper and nickel | m13 | `correlation_copper_final.pdf`, `correlation_nickel_final.pdf` |
| Figure 5, cross-metal DCC | m13 | `DCC_4metals.pdf` |

Results quoted in the text but not tabulated:

| Statement | File |
|---|---|
| H1: illiquidity vs bubble intensity (Pearson, Spearman) | `metals1_h1_correlations.csv` |
| ADF and Phillips–Perron unit-root tests | `metals1_unitroot_tests.csv` |
| Adjustment matrices and weak-exogeneity tests | `metals1_tab4_alpha.csv` |
| Eigenvalues and their contribution to the trace statistic | `metals1_tab4_eigen.csv` |
| AIC lag selection, raw vs filtered | `metals1_tab4_lags.csv` |
| All quantities behind Table 4 | `metals1_tab4_full.csv` |
| DCC parameters a, b, a+b for all three systems | `metals1_dcc_parameters.csv` |
| Monthly mean correlations behind the nickel narrative | `metals1_dcc_nickel_dates.csv` |
| Lithium and cobalt within-metal DCC convergence attempts | `metals1_dcc_lico_attempts.csv` |
| Univariate GARCH-X margin diagnostics | `metals1_garch_diagnostics.csv`, `metals1_garch_bubble_comparison.csv` |

Each script has a configuration block at the top holding paths, seeds and switches. Two
switches in `m13` control optional blocks: `RUN_GARCHX_DIAGNOSTICS` (univariate margin
diagnostics, not reported in the paper, but they validate the specification the DCC systems
are built on) and `RUN_LICO_ATTEMPT` (the lithium and cobalt convergence attempts that
document the limitation stated in the paper). Both default to `TRUE`.

---

## Reproducibility notes

- Monte Carlo critical values for the GSADF test use `seed = 123`.
- The correlation network layout uses `set.seed(42)` and depends on the column order of the
  input file.
- `m11` writes `series_and_bubble_up.csv` — the hand-off file read by `m12` and `m13` — with
  a semicolon separator and a decimal comma. Both readers detect the separator rather than
  assuming it, so a copy that has been through a spreadsheet application or a different
  locale is still read correctly.
- `m12` and `m13` slice that file positionally, columns 2–17 for prices and 18–33 for the
  matching bubble dummies. All three scripts validate this contract — series names, the
  `<TICKER>_BD` correspondence, 0/1 dummies, no missing values, positive prices — before
  computing anything.
- Figures are written as PDF (tracked here) and 300 dpi LZW TIFF (not tracked).

## Troubleshooting

**`Expected 33 columns (Date + 16 prices + 16 dummies)`** — `m12` or `m13` was pointed at
something other than the file `m11` writes. Check that all three scripts use the same
`RESULT_DIR`, and that `m11` completed. If the report says the header has one field under
both separators, the file is neither semicolon- nor comma-delimited and has probably been
re-saved by another application.

**`Input file not found`** — `m11` has not run yet, or it ran with a different `RESULT_DIR`.

**A DCC system does not converge** — `m13` tries the multivariate-t specification first,
then a multivariate normal, then multivariate-t without standard errors, and records which
one succeeded in the `Variant` column of `metals1_dcc_parameters.csv`. Within-metal systems
for lithium and cobalt are expected to fail; `metals1_dcc_lico_attempts.csv` records the
reason for each pair.

---

## Licence

Code in this repository is released under the MIT Licence (see `LICENSE`).

Derived output — tables, figures and the summary CSV files — may be reused with attribution
to the paper.

The underlying price series were obtained from commercial data providers and exchanges.
Redistribution of the price panel is subject to those providers' terms; users who wish to
extend the analysis to other series or periods should obtain the data from the original
sources listed above.

---

## Funding

This work was supported by the National Science Centre, Poland, grant no.
2024/53/B/HS4/03300, "Critical Metal Pricing Dynamics in the Transition to a Net Zero
Economy: Exploring Financial Market Interdependencies".

## Contact

Barbara Będowska-Sójka — barbara.bedowska-sojka@ue.poznan.pl
