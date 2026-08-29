# =====================================================================
#  m13_dcc_garch.R  --  Univariate GARCH-X diagnostics and DCC-GARCH
#
#  Paper: "Tracing Financialisation in Critical Metal Markets"
#  Cleaned replication version -- 27 August 2026
#  (supersedes m13_dcc_garch_FIXED.R)
#
#  ------------------------------------------------------------------
#  INPUT
#    R/results_R/series_and_bubble_up.csv   -- produced by m11_exuber.R
#      semicolon-separated, decimal comma, 33 columns:
#      [1] Date, [2:17] price series, [18:33] matching "<TICKER>_BD" dummies.
#
#  OUTPUT
#    R/graphs_R/correlation_copper_final.{pdf,tiff}   <- Figure 3
#    R/graphs_R/correlation_nickel_final.{pdf,tiff}   <- Figure 4
#    R/graphs_R/DCC_4metals.{pdf,tiff}                <- Figure 5
#    R/results_R/metals1_dcc_parameters.csv   <- a, b, a+b for all systems
#    R/results_R/metals1_dcc_nickel_dates.csv <- monthly mean correlations
#                                                behind the nickel narrative
#    R/results_R/metals1_garch_diagnostics.csv     <- univariate margins
#    R/results_R/metals1_garch_bubble_comparison.csv
#    R/results_R/dcc_all_results.rds          <- fitted objects + pair data
#    R/results_R/dcc_4d_results.rds
#
#  Run order: m11 -> m13. m12 and m13 are independent of each other.
#
#  RUNTIME: the univariate block fits 2 x 16 GARCH models and the DCC block
#  three 4-variable systems, plus (optionally) twelve bivariate systems for
#  lithium and cobalt. Expect tens of minutes, not seconds.
#
#  Packages: tidyverse, rugarch, rmgarch, here
# =====================================================================

library(rugarch)
library(rmgarch)
library(tidyverse)
library(here)


# ---------------------------------------------------------------------
#  1. CONFIGURATION
# ---------------------------------------------------------------------

# Must match RESULT_DIR / GRAPH_DIR in m11_exuber.R: that is where the
# input file is written. Derived, not repeated, so they cannot drift apart.
RESULT_DIR <- here("R", "results_R", "metals1")
GRAPH_DIR  <- here("R", "graphs_R",  "metals1")
DATA_FILE  <- file.path(RESULT_DIR, "series_and_bubble_up.csv")

for (d in c(RESULT_DIR, GRAPH_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Univariate GARCH-X block: not reported as a table in the manuscript, but
# it validates the margin specification the DCC systems are built on and
# answers whether the bubble dummy belongs in the mean or the variance.
RUN_GARCHX_DIAGNOSTICS <- TRUE

# Attempt within-metal DCC for lithium and cobalt. These are expected to
# fail to converge; the manuscript states that as a limitation, and this
# block is what documents the attempt. Set FALSE to skip the runtime cost.
RUN_LICO_ATTEMPT <- TRUE

EXPECTED_SERIES <- c(
  "NIDALY", "NILMEX", "NISHFE", "NIWUXI",
  "CUCOMX", "CUDALY", "CULMEX", "CUSHFE",
  "LIDALY", "LICOMX", "LILMEX", "LIANTH",
  "COCOMX", "CODALY", "COLMEX", "COWUXI"
)

# Within-metal systems. The ORDER of each vector fixes the pair labels in
# the figures (pairs are generated as combn of the vector), and those
# labels are quoted in the text -- do not reorder. Note this differs from
# metal_configs in m12, where the first element sets the normalisation of
# the cointegrating vector; the two orders serve different purposes.
metal_groups <- list(
  Nickel  = c("NIDALY", "NILMEX", "NISHFE", "NIWUXI"),
  Copper  = c("CUDALY", "CUCOMX", "CULMEX", "CUSHFE"),
  Lithium = c("LIDALY", "LIANTH", "LICOMX", "LILMEX"),
  Cobalt  = c("CODALY", "COLMEX", "COWUXI", "COCOMX")
)

DCC_GROUPS <- c("Nickel", "Copper")   # the two that converge

# Cross-commodity system: the four series from the same independent
# provider, one per metal.
CROSS_METALS <- c("CUDALY", "NIDALY", "LIDALY", "CODALY")

# Shared univariate margin. The DCC margins and the "clean" model of the
# GARCH-X diagnostics are the SAME specification, defined once.
MARGIN_SPEC <- ugarchspec(
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(5, 0), include.mean = TRUE),
  distribution.model = "std"
)

save_figure <- function(plot, stem, width = 10, height = 6) {
  ggsave(file.path(GRAPH_DIR, paste0(stem, ".pdf")), plot,
         width = width, height = height, bg = "white")
  ggsave(file.path(GRAPH_DIR, paste0(stem, ".tiff")), plot,
         width = width, height = height, dpi = 300,
         compression = "lzw", bg = "white")
  message("Saved figure: ", stem, " (.pdf, .tiff)")
}

# rugarch can return a uGARCHfit whose @fit slot is empty when the solver
# gives up on a near-degenerate series. `fit@fit$convergence == 0` is then
# logical(0), and any surrounding if() raises "argument is of length zero"
# instead of simply being FALSE.
converged <- function(fit) {
  if (is.null(fit)) return(FALSE)
  isTRUE(tryCatch(fit@fit$convergence == 0, error = function(e) FALSE))
}


# ---------------------------------------------------------------------
#  2. DATA
# ---------------------------------------------------------------------

# m11 writes this file with write_csv2(): semicolon separator, decimal
# comma. A round trip through Excel, a different locale or a different
# readr version can turn it into comma/dot, which a semicolon reader then
# parses as a single column. The separator is detected, not assumed.
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
      "\n  This script slices columns positionally, [2:17] prices and [18:33]",
      "\n  dummies, so a different count would silently pair the wrong series",
      "\n  with the wrong bubble dummy. Fix the input rather than the check.",
      call. = FALSE)
  }
}

df_metals <- as_tibble(read_panel(DATA_FILE)) %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)

df_prices  <- df_metals[c(1, 2:17)]
df_dummies <- df_metals[c(1, 18:33)]

series_names <- names(df_prices)[-1]

stopifnot(
  "Input must have 33 columns (Date + 16 prices + 16 dummies)." =
    ncol(df_metals) == 33,
  "Date column failed to parse." = !anyNA(df_metals$Date),
  "Columns 2:17 are not the 16 expected price series." =
    setequal(series_names, EXPECTED_SERIES),
  "Columns 18:33 do not match the price columns." =
    identical(names(df_dummies)[-1], paste0(series_names, "_BD")),
  "Some series in metal_groups or CROSS_METALS are missing." =
    all(c(unlist(metal_groups), CROSS_METALS) %in% series_names)
)

# Log returns. The dummies are re-aligned on Date rather than by dropping a
# fixed number of rows, so they cannot drift out of step with the returns.
df_returns <- df_prices %>%
  mutate(across(-Date, ~ log(.x / dplyr::lag(.x)))) %>%
  drop_na()

df_dummies_aligned <- df_dummies %>%
  semi_join(df_returns, by = "Date") %>%
  arrange(Date)

stopifnot(
  nrow(df_dummies_aligned) == nrow(df_returns),
  all(df_dummies_aligned$Date == df_returns$Date)
)

message("Returns: ", nrow(df_returns), " observations, ",
        min(df_returns$Date), " to ", max(df_returns$Date))


# ---------------------------------------------------------------------
#  3. UNIVARIATE GARCH-X DIAGNOSTICS
# ---------------------------------------------------------------------
# For each series: the plain margin used by the DCC systems, and the same
# margin with the bubble dummy as an external regressor in both the mean
# and the variance equation.

if (RUN_GARCHX_DIAGNOSTICS) {

  garch_results <- map(series_names, function(m_name) {

    y            <- df_returns[[m_name]]
    dummy_active <- as.numeric(df_dummies_aligned[[paste0(m_name, "_BD")]])
    has_bubbles  <- sum(dummy_active, na.rm = TRUE) > 0

    spec_bubble <- if (has_bubbles) {
      ext_reg <- matrix(dummy_active, ncol = 1, dimnames = list(NULL, "Bubble"))
      ugarchspec(
        variance.model     = list(model = "sGARCH", garchOrder = c(1, 1),
                                  external.regressors = ext_reg),
        mean.model         = list(armaOrder = c(5, 0), include.mean = TRUE,
                                  external.regressors = ext_reg),
        distribution.model = "std"
      )
    } else NULL

    fit_clean <- tryCatch(ugarchfit(MARGIN_SPEC, data = y, solver = "hybrid"),
                          error = function(e) NULL)
    fit_bubble <- if (has_bubbles) {
      tryCatch(ugarchfit(spec_bubble, data = y, solver = "hybrid"),
               error = function(e) NULL)
    } else NULL

    if (!converged(fit_clean))
      message("Clean model failed to converge for: ", m_name)
    if (has_bubbles && !converged(fit_bubble))
      message("Bubble model failed to converge for: ", m_name)

    list(name = m_name, clean = fit_clean, bubble = fit_bubble,
         has_bubbles = has_bubbles)
  }) %>% set_names(series_names)

  # A non-converged fit still yields a row, with the numbers set to NA, so a
  # single bad series cannot abort the sweep over all sixteen.
  garch_diagnostics <- function(fit, series, model_type) {
    if (is.null(fit)) return(NULL)

    empty <- tibble(AIC = NA_real_, BIC = NA_real_, LogLik = NA_real_,
                    Alpha1 = NA_real_, Beta1 = NA_real_, AlphaBeta = NA_real_,
                    LB_P_Value = NA_real_, ARCH_P_Value = NA_real_,
                    P_Bubble_Mean = NA_real_, P_Bubble_Var = NA_real_)

    detail <- tryCatch({
      ic    <- infocriteria(fit)
      res   <- as.numeric(residuals(fit, standardize = TRUE))
      coefs <- fit@fit$matcoef
      pick  <- function(nm, col) if (nm %in% rownames(coefs)) coefs[nm, col] else NA_real_
      tibble(
        AIC           = ic[1],
        BIC           = ic[2],
        LogLik        = fit@fit$LLH,
        Alpha1        = pick("alpha1", 1),
        Beta1         = pick("beta1",  1),
        AlphaBeta     = pick("alpha1", 1) + pick("beta1", 1),
        LB_P_Value    = Box.test(res,   lag = 10, type = "Ljung-Box")$p.value,
        ARCH_P_Value  = Box.test(res^2, lag = 10, type = "Ljung-Box")$p.value,
        P_Bubble_Mean = pick("mxreg1", 4),
        P_Bubble_Var  = pick("vxreg1", 4)
      )
    }, error = function(e) empty)

    bind_cols(
      tibble(Series    = series,
             Model_Type = model_type,
             Converged  = if (converged(fit)) "Yes" else "No"),
      detail
    )
  }

  diagnostic_summary <- map_dfr(garch_results, function(r) bind_rows(
    garch_diagnostics(r$clean,  r$name, "Clean"),
    garch_diagnostics(r$bubble, r$name, "Bubble")
  ))

  print(diagnostic_summary, n = 40, width = Inf)
  write_csv2(diagnostic_summary %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
             file.path(RESULT_DIR, "metals1_garch_diagnostics.csv"))

  # Model_Type is made a factor with both levels so that the _Bubble columns
  # exist even when no series carries a bubble dummy.
  model_comparison <- diagnostic_summary %>%
    dplyr::select(Series, Model_Type, AIC, BIC, P_Bubble_Mean, P_Bubble_Var) %>%
    mutate(Model_Type = factor(Model_Type, levels = c("Clean", "Bubble"))) %>%
    pivot_wider(id_cols      = Series,
                names_from   = Model_Type,
                names_expand = TRUE,
                values_from  = c(AIC, BIC, P_Bubble_Mean, P_Bubble_Var)) %>%
    transmute(
      Series,
      AIC_Clean, AIC_Bubble,
      Better_AIC    = if_else(AIC_Bubble < AIC_Clean, "Bubble", "Clean", missing = "Clean only"),
      BIC_Clean, BIC_Bubble,
      Better_BIC    = if_else(BIC_Bubble < BIC_Clean, "Bubble", "Clean", missing = "Clean only"),
      P_Bubble_Mean = round(P_Bubble_Mean_Bubble, 3),
      P_Bubble_Var  = round(P_Bubble_Var_Bubble,  3)
    )

  print(model_comparison, n = 20, width = Inf)
  write_csv2(model_comparison, file.path(RESULT_DIR, "metals1_garch_bubble_comparison.csv"))
}


# ---------------------------------------------------------------------
#  4. DCC HELPERS
# ---------------------------------------------------------------------

# One fitting routine for every system. The multivariate-t model with
# standard errors is the specification reported; the fallbacks exist so a
# system that will not converge degrades in a documented way rather than
# silently returning nothing. Returns list(fit, variant), or NULL.
fit_dcc <- function(returns_matrix, label) {

  k     <- ncol(returns_matrix)
  Tn    <- nrow(returns_matrix)
  uspec <- multispec(replicate(k, MARGIN_SPEC, simplify = FALSE))

  attempts <- list(
    list(tag = "mvt",             dist = "mvt",    se = TRUE),
    list(tag = "mvnorm",          dist = "mvnorm", se = TRUE),
    list(tag = "mvt, no std.err", dist = "mvt",    se = FALSE)
  )

  for (a in attempts) {

    # The whole attempt is guarded, not just dccfit(). On a near-degenerate
    # system dccfit() can return a DCCfit object *without* raising an error
    # but with no estimated correlation array, and the failure then surfaces
    # deep inside rcor() as "1:m -- argument of length 0". Calling rcor()
    # here turns that into an ordinary failed attempt.
    fit <- tryCatch({
      spec <- dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = a$dist)
      f    <- dccfit(spec = spec, data = returns_matrix, solver = "solnp",
                     fit.control = list(eval.se = a$se))
      if (!inherits(f, "DCCfit")) stop("dccfit() did not return a DCCfit object")
      R <- rcor(f)
      if (!is.array(R) || length(dim(R)) != 3L)
        stop("no conditional correlation array was estimated")
      if (dim(R)[3] != Tn)
        stop("correlation array has ", dim(R)[3], " periods, expected ", Tn)
      f
    }, error = function(e) {
      message("  [", label, "] ", a$tag, " failed: ", conditionMessage(e))
      NULL
    })

    if (!is.null(fit)) {
      if (a$tag != "mvt") message("  [", label, "] fitted with fallback: ", a$tag)
      return(list(fit = fit, variant = a$tag))
    }
  }

  message("  [", label, "] all specifications failed to converge.")
  NULL
}

# a, b, persistence and the t shape parameter from a fitted DCC model.
get_dcc_params <- function(fit, system_name, variant = NA_character_) {
  mc  <- fit@mfit$matcoef
  ia  <- grep("dcca",   rownames(mc))[1]
  ib  <- grep("dccb",   rownames(mc))[1]
  ish <- grep("mshape", rownames(mc))[1]
  tibble(
    System      = system_name,
    Variant     = variant,
    dcc_a       = mc[ia, 1],
    dcc_a_p     = mc[ia, 4],
    dcc_b       = mc[ib, 1],
    dcc_b_p     = mc[ib, 4],
    persistence = mc[ia, 1] + mc[ib, 1],
    shape       = if (!is.na(ish)) mc[ish, 1] else NA_real_,
    loglik      = tryCatch(as.numeric(fit@mfit$llh), error = function(e) NA_real_)
  )
}

# Long frame of the conditional correlation of every pair, one row per day.
# Labels are "<A> - <B>" with spaces; section 8 matches on them exactly.
dcc_pair_frame <- function(fit, series_names, dates) {
  R   <- rcor(fit)
  idx <- combn(seq_along(series_names), 2)
  map_dfr(seq_len(ncol(idx)), function(p) {
    i <- idx[1, p]; j <- idx[2, p]
    tibble(Date        = dates,
           Pair        = paste(series_names[i], "-", series_names[j]),
           Correlation = R[i, j, ])
  })
}

theme_article <- theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
    panel.grid.minor = element_line(color = "#f8f8f8", linewidth = 0.2),
    strip.background = element_blank(),
    strip.text       = element_text(face = "plain", size = 14, color = "black"),
    axis.title       = element_blank(),
    axis.text        = element_text(size = 12, color = "gray30"),
    panel.spacing    = unit(1.2, "lines"),
    plot.title       = element_blank(),
    plot.subtitle    = element_blank()
  )

plot_dcc <- function(pair_data, y_limits, y_breaks, zero_line = FALSE) {
  ggplot(pair_data, aes(x = Date, y = Correlation)) +
    {if (zero_line)
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = "gray80", linewidth = 0.3)} +
    geom_line(color = "black", linewidth = 0.4) +
    facet_wrap(~ Pair, ncol = 3, scales = "fixed") +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    theme_article
}


# ---------------------------------------------------------------------
#  5. WITHIN-METAL DCC: NICKEL AND COPPER  ->  Figures 3 and 4
# ---------------------------------------------------------------------

dcc_all_results <- map(DCC_GROUPS, function(group_name) {

  series <- metal_groups[[group_name]]
  message("Estimating DCC for: ", group_name)

  returns_matrix <- df_returns %>%
    dplyr::select(dplyr::all_of(series)) %>%
    as.matrix()

  res <- fit_dcc(returns_matrix, group_name)
  if (is.null(res)) return(NULL)
  fit <- res$fit

  params <- get_dcc_params(fit, group_name, res$variant)
  message("  a = ", round(params$dcc_a, 4),
          " (p = ", signif(params$dcc_a_p, 3), "), b = ", round(params$dcc_b, 4),
          " (p = ", signif(params$dcc_b_p, 3), "), a+b = ",
          round(params$persistence, 4))

  list(model      = fit,
       data       = dcc_pair_frame(fit, series, df_returns$Date),
       dcc_params = params)
}) %>% set_names(DCC_GROUPS) %>% compact()

stopifnot("Nickel or copper DCC did not converge - cannot build Figures 3-4." =
            all(DCC_GROUPS %in% names(dcc_all_results)))

saveRDS(dcc_all_results, file.path(RESULT_DIR, "dcc_all_results.rds"))

p_nickel <- plot_dcc(dcc_all_results$Nickel$data,
                     y_limits = c(-0.4, 1), y_breaks = seq(-0.2, 1, 0.2))
p_copper <- plot_dcc(dcc_all_results$Copper$data,
                     y_limits = c(-0.4, 1), y_breaks = seq(-0.2, 1, 0.2))

print(p_nickel)
print(p_copper)

save_figure(p_copper, "correlation_copper_final")
save_figure(p_nickel, "correlation_nickel_final")


# ---------------------------------------------------------------------
#  6. CROSS-METAL DCC  ->  Figure 5
# ---------------------------------------------------------------------

df_cross <- df_returns %>%
  dplyr::select(Date, dplyr::all_of(CROSS_METALS)) %>%
  na.omit()

message("Estimating the cross-metal DCC system...")
res_4d <- fit_dcc(as.matrix(df_cross[, CROSS_METALS]), "Cross-metal")

if (!is.null(res_4d)) {

  fit_4d    <- res_4d$fit
  params_4d <- get_dcc_params(fit_4d, "Cross-metal", res_4d$variant)
  print(params_4d)

  df_cross_correlations <- dcc_pair_frame(fit_4d, CROSS_METALS, df_cross$Date)

  saveRDS(list(model = fit_4d, data = df_cross_correlations, dcc_params = params_4d),
          file.path(RESULT_DIR, "dcc_4d_results.rds"))

  p_cross <- plot_dcc(df_cross_correlations,
                      y_limits = c(-0.5, 1), y_breaks = seq(-0.5, 1, 0.25),
                      zero_line = TRUE)
  print(p_cross)
  save_figure(p_cross, "DCC_4metals")

} else {
  message("Cross-metal system did not converge; Figure 5 not produced.")
}


# ---------------------------------------------------------------------
#  7. DCC PARAMETER SUMMARY
# ---------------------------------------------------------------------
# The a, b and a+b values quoted in the Dynamic Conditional Correlation
# subsection for copper, nickel and the cross-metal system.

dcc_summary <- bind_rows(
  map_dfr(dcc_all_results, "dcc_params"),
  if (!is.null(res_4d)) params_4d else NULL
)

print(dcc_summary, width = Inf)
write_csv2(dcc_summary %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
           file.path(RESULT_DIR, "metals1_dcc_parameters.csv"))


# ---------------------------------------------------------------------
#  8. NICKEL CORRELATION EPISODES
# ---------------------------------------------------------------------
# Dates behind the two statements in the DCC paragraph:
#   (a) NILMEX-NIWUXI declines through 2022
#   (b) NIDALY-NILMEX falls towards / below zero in late 2023
# No re-estimation needed; reload with
#   dcc_all_results <- readRDS(here("R/results_R", "dcc_all_results.rds"))

ni <- dcc_all_results$Nickel$data
NI_PAIRS <- c("NIDALY - NILMEX", "NILMEX - NIWUXI")
stopifnot(all(NI_PAIRS %in% unique(ni$Pair)))

pair_profile <- function(df, pair_label) {
  s   <- dplyr::filter(df, Pair == pair_label)
  neg <- dplyr::filter(s, Correlation < 0)
  tibble(
    Pair            = pair_label,
    min_rho         = round(min(s$Correlation), 3),
    min_date        = s$Date[which.min(s$Correlation)],
    days_below_zero = nrow(neg),
    first_neg       = if (nrow(neg)) min(neg$Date) else as.Date(NA),
    last_neg        = if (nrow(neg)) max(neg$Date) else as.Date(NA)
  )
}

map_dfr(NI_PAIRS, ~ pair_profile(ni, .x)) %>% print(width = Inf)

# Monthly means: this is what settles "declines through 2022".
nickel_monthly <- ni %>%
  dplyr::filter(Pair %in% NI_PAIRS) %>%
  mutate(Month = format(Date, "%Y-%m")) %>%
  group_by(Pair, Month) %>%
  summarise(mean_rho = round(mean(Correlation), 3), .groups = "drop") %>%
  pivot_wider(names_from = Pair, values_from = mean_rho)

print(nickel_monthly, n = Inf)
write_csv2(nickel_monthly, file.path(RESULT_DIR, "metals1_dcc_nickel_dates.csv"))

# Screen check only: the published figure has sparse x-breaks, which makes
# the timing of these episodes easy to misread.
ni %>%
  dplyr::filter(Pair %in% NI_PAIRS) %>%
  ggplot(aes(Date, Correlation)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 0.4) +
  facet_wrap(~ Pair, ncol = 1) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_article +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ---------------------------------------------------------------------
#  9. LITHIUM AND COBALT: DOCUMENTED CONVERGENCE FAILURE
# ---------------------------------------------------------------------
# The manuscript states that within-metal DCC systems cannot be estimated
# for lithium and cobalt because their thin trading and high incidence of
# zero returns make the covariance matrix near-singular. This block runs
# the attempt so the claim is reproducible rather than asserted. Pairwise
# rather than 4-variable, which is the most favourable case for convergence.

if (RUN_LICO_ATTEMPT) {

  lico_attempts <- map_dfr(c("Lithium", "Cobalt"), function(group_name) {
    series <- metal_groups[[group_name]]
    message("\nConvergence attempt: ", group_name)

    map_dfr(combn(series, 2, simplify = FALSE), function(pair) {

      label <- paste(pair, collapse = " - ")

      # The whole pair is guarded: the point of this block is to RECORD
      # non-convergence, so a failure must produce a row, never abort the
      # sweep. Note carries the reason, which is the reportable result here.
      tryCatch({

        m_data <- df_returns %>%
          dplyr::select(dplyr::all_of(pair)) %>%
          na.omit() %>%
          as.matrix()

        u1 <- tryCatch(ugarchfit(MARGIN_SPEC, m_data[, 1], solver = "hybrid"),
                       error = function(e) NULL)
        u2 <- tryCatch(ugarchfit(MARGIN_SPEC, m_data[, 2], solver = "hybrid"),
                       error = function(e) NULL)
        margins_ok <- converged(u1) && converged(u2)

        res <- if (margins_ok) fit_dcc(m_data, label) else NULL

        rho <- if (is.null(res)) NA_real_ else
          tryCatch(round(mean(rcor(res$fit)[1, 2, ]), 4), error = function(e) NA_real_)

        tibble(
          Metal      = group_name,
          Pair       = label,
          Margins_OK = margins_ok,
          DCC_OK     = !is.null(res),
          Variant    = if (!is.null(res)) res$variant else NA_character_,
          Mean_Rho   = rho,
          Note       = if (!is.null(res)) "converged"
                       else if (!margins_ok) "univariate margin did not converge"
                       else "DCC did not converge"
        )

      }, error = function(e) {
        message("  [", label, "] aborted: ", conditionMessage(e))
        tibble(Metal = group_name, Pair = label, Margins_OK = NA,
               DCC_OK = FALSE, Variant = NA_character_, Mean_Rho = NA_real_,
               Note = paste("error:", conditionMessage(e)))
      })
    })
  })

  print(lico_attempts, n = Inf, width = Inf)
  write_csv2(lico_attempts, file.path(RESULT_DIR, "metals1_dcc_lico_attempts.csv"))

  message("\nLithium/cobalt pairs that converged: ",
          sum(lico_attempts$DCC_OK), " of ", nrow(lico_attempts))
}

# =====================================================================
#  END
# =====================================================================
