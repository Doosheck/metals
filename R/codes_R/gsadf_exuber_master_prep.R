# GSADF (exuber) bubble dummies on the same master panel as lppls_data_prep / cusum_data_prep
#
# Reads an existing df_master.csv (prices + macro + OLD _BD columns), replaces only
# CODALY_BD, CUDALY_BD, LIDALY_BD, NIDALY_BD using exuber::radf + datestamp.
#
# Two outputs (same csv2 layout as metals30), from ONE radf + ONE MC critical-value run:
#   - df_master_gsadf.csv       = upward-focused: keep a datestamped episode only if OLS
#                                 slope on *levels* over that episode is > 0 (same idea
#                                 as metals30_data_prep.R). NOT the 5-day lag rule.
#   - df_master_gsadf_updown.csv = all days inside every datestamped episode (no OLS
#                                 direction filter). Broader / "up + down" episodes.
#
# Optional 5-day day filter (LPPLS/CUSUM style): set WRITE_5DAY_VARIANT = TRUE; writes
# R/df_master_gsadf_5dlag.csv (still no extra radf_mc_cv if cache exists).
#
# Runtime: radf_mc_cv() dominates the first run (~tens of min); cached by n and seed.
#          Building extra dummy variants is cheap (seconds).
#
# Run from repo root (directory containing `R/` and `data/`):
#   Rscript R/codes_R/gsadf_exuber_master_prep.R
#
# Requires: tidyverse, exuber, here

suppressPackageStartupMessages({
  library(tidyverse)
  library(exuber)
  library(here)
})

# --- Config -----------------------------------------------------------------
PRICE_COLS <- c("CODALY", "CUDALY", "LIDALY", "NIDALY")
BD_COLS     <- paste0(PRICE_COLS, "_BD")

# Alternative (matches lppls_data_prep / cusum day filter): keep day t only if price[t] > price[t - 5]
TREND_LAG <- 5L

# Extra file with 5-day lag filter applied to raw datestamp episodes (after optional trim)
WRITE_5DAY_VARIANT <- FALSE

# Monte Carlo CV cache (depends on nrow, minw, seed — edit if you change radf())
MC_SEED <- 123L

# Significance for datestamp()
DATESTAMP_P <- 0.05

# Input / output paths (first existing master wins)
MASTER_CANDIDATES <- c(
  here("R/data_R/df_master.csv"),
  here("R/df_master.csv")
)
OUT_PATH_UPWARD <- here("R/df_master_gsadf.csv")
OUT_PATH_UPDOWN <- here("R/df_master_gsadf_updown.csv")
OUT_PATH_5DLAG  <- here("R/df_master_gsadf_5dlag.csv")

# --- Locate master file ------------------------------------------------------
path_master <- MASTER_CANDIDATES[file.exists(MASTER_CANDIDATES)][1]
if (is.na(path_master)) {
  stop(
    "No df_master.csv found. Tried:\n  ",
    paste(MASTER_CANDIDATES, collapse = "\n  "),
    "\nRun metals30_data_prep.R first or copy the master into R/."
  )
}
message("Using master: ", path_master)

# read.csv2: sep ';', dec ',' — first column is row index (matches write.csv2(df_master) from metals30)
df <- read.csv2(path_master, row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)
df$Date <- as.Date(df$Date)

missing_price <- setdiff(PRICE_COLS, names(df))
if (length(missing_price) > 0) {
  stop("Master is missing price columns: ", paste(missing_price, collapse = ", "))
}

df <- df %>%
  arrange(Date) %>%
  drop_na(all_of(PRICE_COLS))

# exuber: numeric matrix, row order = time order (same as metals30: levels, not log)
data_matrix <- df %>%
  select(all_of(PRICE_COLS)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

if (any(!is.finite(data_matrix))) {
  stop("Non-finite values in price matrix after drop_na; check master.")
}

message("Panel: ", nrow(df), " rows, ", min(df$Date), " — ", max(df$Date))

# --- radf + MC critical values ----------------------------------------------
est_results <- radf(data_matrix)

mc_dir <- here("outputs", "R_objects")
dir.create(mc_dir, recursive = TRUE, showWarnings = FALSE)
mc_cv_path <- file.path(mc_dir, sprintf("mc_cv_radf_n%s_minwDefault_seed%s.rds", nrow(data_matrix), MC_SEED))

if (file.exists(mc_cv_path)) {
  mc_cv <- readRDS(mc_cv_path)
  message("Loaded MC critical values: ", mc_cv_path)
} else {
  message("Computing radf_mc_cv (may take a minute) …")
  mc_cv <- radf_mc_cv(n = nrow(data_matrix), seed = MC_SEED)
  saveRDS(mc_cv, mc_cv_path)
  message("Saved: ", mc_cv_path)
}

bubble_dates <- datestamp(est_results, cv = mc_cv, p = DATESTAMP_P)

# --- Build dummies -----------------------------------------------------------
is_positive_trend <- function(start_idx, end_idx, prices) {
  p <- prices[start_idx:end_idx]
  if (length(p) > 2) {
    return(coef(lm(p ~ seq_along(p)))[2] > 0)
  }
  tail(p, 1) > head(p, 1)
}

fill_episodes <- function(upward_ols_only) {
  m <- matrix(0L, nrow = nrow(df), ncol = length(PRICE_COLS))
  colnames(m) <- BD_COLS
  for (col in PRICE_COLS) {
    periods <- bubble_dates[[col]]
    prices <- as.numeric(df[[col]])
    if (is.null(periods) || nrow(periods) == 0) next
    for (i in seq_len(nrow(periods))) {
      s <- periods[i, "Start"]
      e <- periods[i, "End"]
      if (upward_ols_only) {
        if (is_positive_trend(s, e, prices)) {
          m[s:e, paste0(col, "_BD")] <- 1L
        }
      } else {
        m[s:e, paste0(col, "_BD")] <- 1L
      }
    }
  }
  m
}

apply_5day_lag_filter <- function(dummy_matrix) {
  m <- dummy_matrix
  for (col in PRICE_COLS) {
    raw <- m[, paste0(col, "_BD")]
    pvec <- as.numeric(df[[col]])
    for (t in seq_along(raw)) {
      if (raw[t] == 1L && t > TREND_LAG && pvec[t] <= pvec[t - TREND_LAG]) {
        raw[t] <- 0L
      }
    }
    m[, paste0(col, "_BD")] <- raw
  }
  m
}

dummy_up <- fill_episodes(upward_ols_only = TRUE)
dummy_ud <- fill_episodes(upward_ols_only = FALSE)

# --- Write upward (OLS) master ----------------------------------------------
df_up <- df
for (nm in BD_COLS) {
  df_up[[nm]] <- as.integer(dummy_up[, nm])
}

message("\nGSADF — upward (OLS episode filter), df_master_gsadf.csv:")
for (col in PRICE_COLS) {
  nm <- paste0(col, "_BD")
  message(sprintf("  %-8s: %5d days (%.1f%%)", col, sum(df_up[[nm]]), 100 * mean(df_up[[nm]])))
}
write.csv2(df_up, OUT_PATH_UPWARD, row.names = TRUE)
message("Wrote: ", OUT_PATH_UPWARD)

# --- Write up+down (all datestamped days) ------------------------------------
df_ud <- df
for (nm in BD_COLS) {
  df_ud[[nm]] <- as.integer(dummy_ud[, nm])
}

message("\nGSADF — all datestamped episodes (no OLS), df_master_gsadf_updown.csv:")
for (col in PRICE_COLS) {
  nm <- paste0(col, "_BD")
  message(sprintf("  %-8s: %5d days (%.1f%%)", col, sum(df_ud[[nm]]), 100 * mean(df_ud[[nm]])))
}
write.csv2(df_ud, OUT_PATH_UPDOWN, row.names = TRUE)
message("Wrote: ", OUT_PATH_UPDOWN)

# --- Optional: 5-day lag on raw episodes ------------------------------------
if (isTRUE(WRITE_5DAY_VARIANT)) {
  dummy_5d <- apply_5day_lag_filter(dummy_ud)
  df_5 <- df
  for (nm in BD_COLS) {
    df_5[[nm]] <- as.integer(dummy_5d[, nm])
  }
  message("\nGSADF — datestamp + 5-day price filter, df_master_gsadf_5dlag.csv:")
  for (col in PRICE_COLS) {
    nm <- paste0(col, "_BD")
    message(sprintf("  %-8s: %5d days (%.1f%%)", col, sum(df_5[[nm]]), 100 * mean(df_5[[nm]])))
  }
  write.csv2(df_5, OUT_PATH_5DLAG, row.names = TRUE)
  message("Wrote: ", OUT_PATH_5DLAG)
}
