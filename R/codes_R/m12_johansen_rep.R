# =====================================================================
#  m12_johansen.R  --  Johansen cointegration: raw vs bubble-filtered
#
#  Paper: "Tracing Financialisation in Critical Metal Markets"
#  Cleaned replication version -- 27 August 2026
#  (supersedes m12_johan_cointegration_FIXED_v2.R)
#
#  ------------------------------------------------------------------
#  INPUT
#    R/results_R/series_and_bubble_up.csv   -- produced by m11_exuber.R
#      semicolon-separated, decimal comma, 33 columns:
#      [1] Date, [2:17] price series, [18:33] matching "<TICKER>_BD" dummies.
#
#  OUTPUT
#    R/results_R/metals1_tab4.csv / .tex    <- Table 4 in the manuscript
#    R/results_R/metals1_tab4_full.csv      <- all extracted quantities
#    R/results_R/metals1_tab4_alpha.csv     <- full adjustment matrices +
#                                              weak-exogeneity tests
#    R/results_R/metals1_tab4_eigen.csv     <- eigenvalues and their
#                                              contribution to the trace
#    R/results_R/metals1_tab4_lags.csv      <- AIC lag selection control
#
#  The alpha and eigenvalue files back two statements in the Cointegration
#  subsection: the ~80% increase in the adjustment speed of NILMEX under
#  filtering, and the concentration of nickel's trace increase in the
#  largest eigenvalue.
#
#  Run order: m11 -> m12. m12 and m13 are independent of each other.
#
#  Packages: vars, urca, tidyverse, here, kableExtra
# =====================================================================

# `vars` pulls in MASS, which masks dplyr::select; tidyverse is attached
# afterwards so the dplyr verbs win. Calls are qualified anyway.
library(vars)
library(urca)
library(tidyverse)
library(here)
library(kableExtra)


# ---------------------------------------------------------------------
#  1. CONFIGURATION
# ---------------------------------------------------------------------

# Must match RESULT_DIR in m11_exuber.R: that is where the input file is
# written. Derived, not repeated, so the two cannot drift apart.
RESULT_DIR <- here("R", "results_R", "metals1")
DATA_FILE  <- file.path(RESULT_DIR, "series_and_bubble_up.csv")
dir.create(RESULT_DIR, recursive = TRUE, showWarnings = FALSE)

# VECM parameterisation.
#   "transitory" -> Pi multiplies y_{t-1}, so alpha is the speed of
#                   adjustment to last period's disequilibrium. This is
#                   how alpha is described in the manuscript.
#   "longrun"    -> ca.jo's default; Pi multiplies y_{t-K}. Different
#                   interpretation, do not mix the two.
SPEC <- "transitory"

# TRUE  -> the same lag order in the raw and the filtered model, as stated
#          in the Methods section.
# FALSE -> AIC selected separately for each model.
SAME_LAG_BOTH_MODELS <- TRUE

MAX_LAG <- 10

# Significance level used for the sequential trace test (column of @cval).
CVAL_10PCT <- 1
CVAL_5PCT  <- 2
CVAL_1PCT  <- 3

# Series expected in the input file.
EXPECTED_SERIES <- c(
  "NIDALY", "NILMEX", "NISHFE", "NIWUXI",
  "CUCOMX", "CUDALY", "CULMEX", "CUSHFE",
  "LIDALY", "LICOMX", "LILMEX", "LIANTH",
  "COCOMX", "CODALY", "COLMEX", "COWUXI"
)

# One system per metal. The FIRST ticker of each vector is the series the
# cointegrating vector is normalised on -- do not reorder these without
# re-reading the reported alpha coefficients.
metal_configs <- list(
  Cobalt  = c("COCOMX", "CODALY", "COLMEX", "COWUXI"),
  Copper  = c("CUCOMX", "CUDALY", "CULMEX", "CUSHFE"),
  Lithium = c("LICOMX", "LIDALY", "LILMEX", "LIANTH"),
  Nickel  = c("NIDALY", "NILMEX", "NISHFE", "NIWUXI")
)


# ---------------------------------------------------------------------
#  2. DATA
# ---------------------------------------------------------------------

# m11 writes this file with write_csv2(): semicolon separator, decimal
# comma. A round trip through Excel, a different locale or a different
# readr version can turn it into comma/dot, which read.csv2() then parses
# as a single column. The separator is detected rather than assumed, so
# either convention is read correctly.
read_panel <- function(path, expected_cols = 33) {

  if (!file.exists(path)) {
    stop("\n  Input file not found:\n    ", path,
         "\n  It is written by m11_exuber.R -- run that first, and check that",
         "\n  both scripts use the same RESULT_DIR.", call. = FALSE)
  }

  n_fields <- function(sep) {
    tryCatch(utils::count.fields(path, sep = sep, quote = "\"")[1],
             error = function(e) NA_integer_)
  }
  n_semi  <- n_fields(";")
  n_comma <- n_fields(",")

  if (isTRUE(n_semi == expected_cols)) {
    utils::read.csv2(path, stringsAsFactors = FALSE)
  } else if (isTRUE(n_comma == expected_cols)) {
    message("Note: input is comma-separated with a dot decimal mark. ",
            "Reading it as such.")
    utils::read.csv(path, stringsAsFactors = FALSE)
  } else {
    stop(
      "\n  Expected ", expected_cols, " columns (Date + 16 prices + 16 dummies).",
      "\n  File:      ", path,
      "\n  Modified:  ", format(file.mtime(path)),
      "\n  Fields in the header: ", n_semi, " with ';', ", n_comma, " with ','.",
      "\n  Header:    ", substr(readLines(path, n = 1, warn = FALSE), 1, 200),
      "\n",
      "\n  Neither separator gives ", expected_cols, " columns, so this is",
      "\n  probably not metals1's file. Check that m11 ran with the same",
      "\n  RESULT_DIR and that no other project writes a file of this name",
      "\n  into that folder.",
      call. = FALSE)
  }
}

df_raw <- read_panel(DATA_FILE)
df_raw$Date <- as.Date(df_raw$Date, format = "%Y-%m-%d")

price_cols <- names(df_raw)[2:17]
dummy_cols <- names(df_raw)[18:33]

stopifnot(
  "Input must have 33 columns (Date + 16 prices + 16 dummies)." =
    ncol(df_raw) == 33,
  "Date column failed to parse." = !anyNA(df_raw$Date),
  "Columns 2:17 are not the 16 expected price series." =
    setequal(price_cols, EXPECTED_SERIES),
  "Columns 18:33 do not match the price columns." =
    identical(dummy_cols, paste0(price_cols, "_BD")),
  "Bubble dummies must be 0/1." =
    all(as.matrix(df_raw[dummy_cols]) %in% c(0L, 1L)),
  "Some series in metal_configs are missing from the input." =
    all(unlist(metal_configs) %in% price_cols)
)

message("Loaded ", nrow(df_raw), " observations, ",
        min(df_raw$Date), " to ", max(df_raw$Date))


# ---------------------------------------------------------------------
#  3. HELPERS
# ---------------------------------------------------------------------

# Sequential trace test at 5%: start from the last (largest r) null and
# count rejections until the first non-rejection.
johansen_rank <- function(jo) {
  ts <- jo@teststat
  cv <- jo@cval[, CVAL_5PCT]
  r  <- 0
  for (i in length(ts):1) {
    if (ts[i] > cv[i]) r <- r + 1 else break
  }
  r
}

add_stars <- function(stat, c10, c5, c1) {
  if (stat > c1)  return(paste0(sprintf("%.2f", stat), "***"))
  if (stat > c5)  return(paste0(sprintf("%.2f", stat), "**"))
  if (stat > c10) return(paste0(sprintf("%.2f", stat), "*"))
  sprintf("%.2f", stat)
}

# Likelihood-ratio test of weak exogeneity, one series at a time.
# H0: the series' row of the adjustment matrix is zero, i.e. it does not
# respond to disequilibrium and can be read as a price leader. df = r.
weak_exogeneity <- function(jo, r, k) {
  stat <- vapply(seq_len(k), function(i) {
    A   <- diag(k)[, -i, drop = FALSE]
    out <- try(urca::alrtest(z = jo, A = A, r = r), silent = TRUE)
    if (inherits(out, "try-error")) return(NA_real_)
    as.numeric(out@teststat)[1]
  }, numeric(1))
  list(stat = stat, pval = pchisq(stat, df = r, lower.tail = FALSE))
}


# ---------------------------------------------------------------------
#  4. ESTIMATION, ONE METAL AT A TIME
# ---------------------------------------------------------------------

run_metal_analysis <- function(metal_name, columns) {

  message("\n=== Analysing: ", metal_name, " ===")

  tryCatch({

    # Prices and dummies are cut together, matched on Date, so the two
    # can never drift apart.
    cols_bd <- paste0(columns, "_BD")

    dat <- df_raw %>%
      dplyr::select(Date, dplyr::all_of(columns), dplyr::all_of(cols_bd)) %>%
      tidyr::drop_na()

    price_raw <- as.matrix(dat[, columns, drop = FALSE])
    if (any(price_raw <= 0, na.rm = TRUE)) {
      stop("Non-positive prices found - log() would give NaN/-Inf.")
    }

    metal_data <- log(price_raw)
    metal_dummies <- as.matrix(dat[, cols_bd, drop = FALSE])
    colnames(metal_dummies) <- paste0(columns, "_DV")

    stopifnot(nrow(metal_data) == nrow(metal_dummies))
    message("  ", nrow(metal_data), " x ", ncol(metal_data),
            ", sample ", min(dat$Date), " to ", max(dat$Date))

    # Series with no detected bubble contribute a constant column, which
    # would be perfectly collinear with the intercept.
    constant_cols <- which(apply(metal_dummies, 2, var, na.rm = TRUE) < 1e-10)
    if (length(constant_cols) > 0) {
      message("  Dropping ", length(constant_cols), " constant dummy column(s): ",
              paste(colnames(metal_dummies)[constant_cols], collapse = ", "))
      metal_dummies <- metal_dummies[, -constant_cols, drop = FALSE]
    }

    has_dummies <- ncol(metal_dummies) > 0
    if (has_dummies) {
      message("  Using ", ncol(metal_dummies), " bubble dummy variable(s)")
    } else {
      message("  WARNING: no bubbles detected - estimating the raw model only")
    }

    # --- lag order (K >= 2 is required by ca.jo) ---
    best_lag_basic <- max(2, as.integer(
      VARselect(metal_data, lag.max = MAX_LAG, type = "const")$selection["AIC(n)"]))

    lag_ex_aic <- if (has_dummies) {
      max(2, as.integer(
        VARselect(metal_data, lag.max = MAX_LAG, type = "const",
                  exogen = metal_dummies)$selection["AIC(n)"]))
    } else {
      best_lag_basic
    }

    best_lag_ex <- if (SAME_LAG_BOTH_MODELS) best_lag_basic else lag_ex_aic

    if (lag_ex_aic != best_lag_basic) {
      message("  NOTE: AIC lag differs (raw = ", best_lag_basic,
              ", filtered = ", lag_ex_aic, "). Using ",
              if (SAME_LAG_BOTH_MODELS) "the raw lag for both." else "separate lags.")
    }

    # --- Johansen trace test, restricted constant, no trend ---
    jo_basic <- ca.jo(metal_data, type = "trace", ecdet = "const",
                      K = best_lag_basic, spec = SPEC)

    jo_ex <- if (has_dummies) {
      ca.jo(metal_data, type = "trace", ecdet = "const",
            K = best_lag_ex, spec = SPEC, dumvar = metal_dummies)
    } else NULL

    results <- tibble(
      Metal          = metal_name,
      N_Series       = ncol(metal_data),
      N_Obs          = nrow(metal_data),
      N_Bubble_Vars  = if (has_dummies) ncol(metal_dummies) else 0L,
      Lag_Basic      = best_lag_basic,
      Lag_Ex         = best_lag_ex,
      Lag_Ex_AIC     = lag_ex_aic,
      Rank_Basic     = johansen_rank(jo_basic),
      Rank_Ex        = if (!is.null(jo_ex)) johansen_rank(jo_ex) else NA_integer_,
      Trace_r0_Basic = jo_basic@teststat[length(jo_basic@teststat)],
      Trace_r0_Ex    = if (!is.null(jo_ex)) jo_ex@teststat[length(jo_ex@teststat)] else NA_real_,
      Normalised_On  = columns[1],
      Status         = if (has_dummies) "Complete" else "No bubbles - raw only"
    )

    message("  OK - ", metal_name)

    list(results = results, jo_basic = jo_basic, jo_ex = jo_ex, data = metal_data)

  }, error = function(e) {
    message("  ERROR in ", metal_name, ": ", e$message)
    list(results = tibble(Metal = metal_name, N_Series = length(columns),
                          Status = paste("ERROR:", e$message)),
         jo_basic = NULL, jo_ex = NULL, data = NULL)
  })
}


# ---------------------------------------------------------------------
#  5. RESULT EXTRACTION
# ---------------------------------------------------------------------

extract_results <- function(metal_name, jo_obj, model_name, price_data) {

  if (is.null(jo_obj)) return(NULL)

  trace_stats <- jo_obj@teststat
  n_tests     <- length(trace_stats)
  cv10 <- jo_obj@cval[, CVAL_10PCT]
  cv5  <- jo_obj@cval[, CVAL_5PCT]
  cv1  <- jo_obj@cval[, CVAL_1PCT]

  r      <- johansen_rank(jo_obj)
  k_ser  <- ncol(price_data)
  safe_r <- max(1, min(r, k_ser - 1))
  if (r == 0) message("  WARNING [", metal_name, " ", model_name,
                      "]: r = 0; alpha reported for r = 1 illustratively only.")

  vecm           <- cajorls(jo_obj, r = safe_r)
  exchange_names <- colnames(price_data)

  # --- adjustment matrix ---
  # cajorls returns the ECT loadings as rows of coef(vecm$rlm); alpha is
  # the transpose, k series x r cointegrating vectors.
  cf        <- coef(vecm$rlm)
  alpha_mat <- t(cf[grep("^ect", rownames(cf)), , drop = FALSE])
  rownames(alpha_mat) <- exchange_names
  colnames(alpha_mat) <- paste0("ect", seq_len(safe_r))

  # The sign of a single alpha depends on the normalisation of the
  # cointegrating vector, and alpha has r columns when r > 1, so overall
  # adjustment speed is summarised by the Euclidean norm of each row.
  adj_speed <- sqrt(rowSums(alpha_mat^2))

  leader_idx   <- which.min(adj_speed)
  follower_idx <- which.max(adj_speed)

  own_alpha <- alpha_mat[1, 1]
  if (own_alpha > 0) {
    message("  NOTE [", metal_name, " ", model_name, "]: alpha on the ",
            "normalising series (", exchange_names[1], ") = ",
            round(own_alpha, 4), " (positive) - check the sign convention.")
  }

  # --- weak exogeneity ---
  wx <- weak_exogeneity(jo_obj, r = safe_r, k = k_ser)
  names(wx$stat) <- names(wx$pval) <- exchange_names

  wx_leader <- if (all(is.na(wx$pval))) NA_character_ else
    exchange_names[which.max(replace(wx$pval, is.na(wx$pval), -Inf))]

  # --- eigenvalues and their contribution to the trace statistic ---
  Tobs      <- nrow(jo_obj@Z0)
  lam       <- head(jo_obj@lambda, k_ser)
  trace_con <- -Tobs * log(1 - lam)

  # --- trace statistics, starred, blanked past the sequence terminus ---
  trace_formatted <- character(n_tests)
  for (i in seq_len(n_tests)) {
    if (i <= r + 1) {
      j <- n_tests - i + 1
      trace_formatted[i] <- add_stars(trace_stats[j], cv10[j], cv5[j], cv1[j])
    } else trace_formatted[i] <- ""
  }

  row <- tibble(Metal = metal_name, Model = model_name, Rank = r)
  for (i in 0:(n_tests - 1)) {
    col_name    <- if (i == 0) "r=0" else paste0("r<=", i)
    row[[col_name]] <- trace_formatted[i + 1]
  }

  row$`Max |a|`     <- sprintf("%.3f", adj_speed[follower_idx])
  row$Follower      <- exchange_names[follower_idx]
  row$`Min |a|`     <- sprintf("%.3f", adj_speed[leader_idx])
  row$Leader        <- exchange_names[leader_idx]
  row$WX_Leader     <- wx_leader
  row$WX_p          <- sprintf("%.3f", suppressWarnings(max(wx$pval, na.rm = TRUE)))
  row$WX_p_leader   <- sprintf("%.3f", wx$pval[leader_idx])
  row$WX_p_follower <- sprintf("%.3f", wx$pval[follower_idx])
  row$Normalised    <- exchange_names[1]

  alpha_tbl <- as_tibble(alpha_mat, rownames = "Series") %>%
    mutate(Metal    = metal_name,
           Model    = model_name,
           Alpha_Norm = as.numeric(adj_speed),
           WX_stat  = as.numeric(wx$stat),
           WX_pval  = as.numeric(wx$pval)) %>%
    dplyr::select(Metal, Model, Series, dplyr::starts_with("ect"),
                  Alpha_Norm, WX_stat, WX_pval)

  eigen_tbl <- tibble(
    Metal              = metal_name,
    Model              = model_name,
    Eigenvalue_Index   = seq_along(lam),
    Lambda             = as.numeric(lam),
    Trace_Contribution = as.numeric(trace_con),
    T_Obs              = Tobs
  )

  list(row = row, alpha = alpha_tbl, eigen = eigen_tbl)
}


# ---------------------------------------------------------------------
#  6. RUN
# ---------------------------------------------------------------------

all_results <- map(names(metal_configs),
                   ~ run_metal_analysis(.x, metal_configs[[.x]])) %>%
  set_names(names(metal_configs))

summary_table <- map_dfr(all_results, "results")
print(summary_table, width = Inf)

# Control: did AIC want a different lag order once the dummies entered?
lag_check <- summary_table %>%
  dplyr::select(Metal, Lag_Basic, Lag_Ex, Lag_Ex_AIC, Rank_Basic, Rank_Ex)
message("\n=== Lag check ===")
print(lag_check)
write.csv2(lag_check, file.path(RESULT_DIR, "metals1_tab4_lags.csv"),
           row.names = FALSE)

# Each metal is extracted once, raw and filtered; nothing is re-estimated
# for the diagnostics below.
extracted <- imap(all_results, function(res, metal) {
  list(
    Raw      = extract_results(toupper(metal), res$jo_basic, "Raw",      res$data),
    Filtered = extract_results(toupper(metal), res$jo_ex,    "Filtered", res$data)
  )
}) %>%
  flatten() %>%
  compact()

latex_table_data <- map_dfr(extracted, "row") %>%
  mutate(Model = factor(Model, levels = c("Raw", "Filtered"))) %>%
  arrange(Model)     # stable: metal order preserved within each block

print(latex_table_data, width = Inf)


# ---------------------------------------------------------------------
#  7. DIAGNOSTIC EXPORTS
# ---------------------------------------------------------------------

alpha_table <- map_dfr(extracted, "alpha")
eigen_table <- map_dfr(extracted, "eigen")

write.csv2(latex_table_data, file.path(RESULT_DIR, "metals1_tab4_full.csv"),
           row.names = FALSE)
write.csv2(alpha_table %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
           file.path(RESULT_DIR, "metals1_tab4_alpha.csv"), row.names = FALSE)
write.csv2(eigen_table %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
           file.path(RESULT_DIR, "metals1_tab4_eigen.csv"), row.names = FALSE)

message("\n=== Adjustment matrices and weak-exogeneity tests (raw) ===")
alpha_table %>%
  dplyr::filter(Model == "Raw") %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  print(n = Inf, width = Inf)

message("\n=== Change in the adjustment norm, raw -> filtered ===")
alpha_table %>%
  dplyr::select(Metal, Model, Series, Alpha_Norm) %>%
  tidyr::pivot_wider(names_from = Model, values_from = Alpha_Norm) %>%
  mutate(Pct_Change = round(100 * (Filtered - Raw) / Raw, 1)) %>%
  print(n = Inf)

message("\n=== Eigenvalue contributions to the trace statistic ===")
eigen_table %>%
  mutate(across(c(Lambda, Trace_Contribution), ~ round(.x, 4))) %>%
  tidyr::pivot_wider(id_cols = c(Metal, Eigenvalue_Index),
                     names_from = Model, values_from = Trace_Contribution) %>%
  print(n = Inf)


# ---------------------------------------------------------------------
#  8. TABLE 4
# ---------------------------------------------------------------------
# Asterisks on Max |a| and Min |a| come from the weak-exogeneity test, not
# from the significance of alpha itself:
#   on Max |a| -> the fastest series does adjust (expected for a follower)
#   on Min |a| -> even the slowest series adjusts, so it is NOT a price
#                 leader; an unstarred Min |a| is the leadership claim.

trace_cols   <- names(latex_table_data)[grepl("^r(=|<=)", names(latex_table_data))]
trace_labels <- ifelse(trace_cols == "r=0", "$r=0$",
                       paste0("$r \\leq ", sub("^r<=", "", trace_cols), "$"))

add_wx_stars <- function(val, p) {
  p <- as.numeric(p)
  v <- sprintf("%.3f", as.numeric(val))
  ifelse(is.na(p), v,
  ifelse(p < 0.01, paste0(v, "***"),
  ifelse(p < 0.05, paste0(v, "**"),
  ifelse(p < 0.10, paste0(v, "*"), v))))
}

table_4 <- latex_table_data %>%
  mutate(MaxA = add_wx_stars(`Max |a|`, WX_p_follower),
         MinA = add_wx_stars(`Min |a|`, WX_p_leader)) %>%
  dplyr::select(Metal, Model, Rank, dplyr::all_of(trace_cols),
                MaxA, Follower, MinA, Leader)

print(table_4, width = Inf)
write.csv2(table_4, file.path(RESULT_DIR, "metals1_tab4.csv"), row.names = FALSE)

n_raw      <- sum(table_4$Model == "Raw")
n_filtered <- sum(table_4$Model == "Filtered")

latex_tab4 <- table_4 %>%
  dplyr::select(-Model) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      align = c("l", "c", rep("c", length(trace_cols)), "c", "l", "c", "l"),
      col.names = c("Metal", "$r$", trace_labels,
                    "Max $|\\alpha|$", "Follower",
                    "Min $|\\alpha|$", "Leader"),
      caption = "Results of the Johansen Cointegration Tests",
      # knitr prepends "tab:" itself, so pass the label without it
      label   = "johansen") %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c(" " = 2,
                     "Trace statistics" = length(trace_cols),
                     "Adjustment speed" = 4), escape = FALSE) %>%
  pack_rows("Raw series", 1, n_raw, bold = TRUE, indent = FALSE) %>%
  pack_rows("Filtered", n_raw + 1, n_raw + n_filtered, bold = TRUE, indent = FALSE)

cat(latex_tab4)
writeLines(as.character(latex_tab4), file.path(RESULT_DIR, "metals1_tab4.tex"))


# ---------------------------------------------------------------------
#  9. PRICE LEADERSHIP CONTROL
# ---------------------------------------------------------------------
# Two criteria nominate a leader: the smallest adjustment norm, and the
# largest weak-exogeneity p-value. Where they disagree, price discovery is
# not sharply located. The manuscript reports that they disagree for
# nickel in the raw model and agree once bubbles are filtered out.

message("\n=== Leader (min |alpha|) vs leader (weak exogeneity) ===")
latex_table_data %>%
  dplyr::select(Metal, Model, Leader, WX_Leader,
                WX_p, WX_p_leader, WX_p_follower) %>%
  mutate(Agree = Leader == WX_Leader) %>%
  print(n = Inf, width = Inf)

# =====================================================================
#  END
# =====================================================================
