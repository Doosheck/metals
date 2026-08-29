# =====================================================================
#  m11_exuber.R  --  Bubble detection (GSADF), correlation network,
#                    descriptive statistics and the H1 illiquidity test
#
#  Paper: "Tracing Financialisation in Critical Metal Markets"
#  Cleaned replication version -- 27 August 2026
#
#  ------------------------------------------------------------------
#  INPUT (single file)
#    data/combined_metals_cleaned3.csv
#      comma-separated, dot decimal, column `Date` (YYYY-MM-DD)
#      + exactly 16 price columns, one per series.
#      Already synchronised to the common window 2021-07-19..2025-07-21
#      and gap-filled by cubic-spline interpolation.
#
#      NOTE: keep the column order of this file unchanged. The network
#      layout is a seeded force-directed algorithm, so a different
#      column order reproduces a different (equally valid) picture.
#
#  OUTPUT
#    R/results_R/series_and_bubble_up.csv   <- input for m12 and m13
#    R/results_R/metals1_tab2.csv / .tex    <- Table 2 (descriptive stats)
#    R/results_R/bubble_summary_up.csv      <- Table 3 (bubble episodes)
#    R/results_R/metals1_tab3.tex
#    R/results_R/metals1_h1_correlations.csv  <- H1 test (Pearson/Spearman)
#    R/results_R/metals1_unitroot_tests.csv   <- ADF / PP tests
#    R/results_R/R_objects/mc_cv_2021_2025.rds  <- cached MC critical values
#    R/graphs_R/Metal_Network_21_25_thresh_75p_5color1.{pdf,tiff}  <- Figure 1
#    R/graphs_R/bubbles_up_only1.{pdf,tiff}                        <- Figure 2
#
#  Figures 3-5 (DCC) come from m13; Table 4 (Johansen) comes from m12.
#  Both read R/results_R/series_and_bubble_up.csv produced here.
#
#  Packages (install once, outside this script):
#    tidyverse, here, exuber, moments, FinTS, urca,
#    knitr, kableExtra, igraph, ggraph
# =====================================================================

# igraph and ggraph are attached first on purpose: igraph masks base::union
# and a few dplyr/purrr verbs, so tidyverse is loaded afterwards and wins.
library(igraph)
library(ggraph)
library(tidyverse)
library(here)
library(exuber)
library(moments)
library(FinTS)
library(urca)
library(knitr)
library(kableExtra)


# ---------------------------------------------------------------------
#  1. CONFIGURATION
# ---------------------------------------------------------------------

DATA_FILE  <- here("data", "combined_metals_cleaned3.csv")
RESULT_DIR <- here("R", "results_R", "metals1")
GRAPH_DIR  <- here("R", "graphs_R",  "metals1")
OBJECT_DIR <- file.path(RESULT_DIR, "R_objects")

for (d in c(RESULT_DIR, OBJECT_DIR, GRAPH_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

SAMPLE_START <- as.Date("2021-07-19")
SAMPLE_END   <- as.Date("2025-07-21")

# Series expected in the input file (order-independent check).
EXPECTED_SERIES <- c(
  "NIDALY", "NILMEX", "NISHFE", "NIWUXI",
  "CUCOMX", "CUDALY", "CULMEX", "CUSHFE",
  "LIDALY", "LICOMX", "LILMEX", "LIANTH",
  "COCOMX", "CODALY", "COLMEX", "COWUXI"
)

# Monte Carlo critical values for the GSADF test.
MC_SEED    <- 123
MC_CV_FILE <- file.path(OBJECT_DIR, "mc_cv_2021_2025.rds")

# Correlation network.
#   NET_QUANTILE     - edges kept above this quantile of the upper-triangle
#                      correlation matrix (0.75 in the paper).
#   NET_LAYOUT       - "fr" reproduces the figure published in the paper.
#   NET_LAYOUT_WEIGHTED - TRUE = layout uses edge weights.
#
#   CAUTION: the caption of Figure 1 in the manuscript describes an
#   *unweighted Kamada-Kawai* layout, i.e. NET_LAYOUT = "kk" and
#   NET_LAYOUT_WEIGHTED = FALSE. The file actually included in the paper
#   (Metal_Network_21_25_thresh_75p_5color1.pdf) was produced with the
#   settings below. Either regenerate the figure with "kk" or correct the
#   caption -- do not leave the two inconsistent.
NET_QUANTILE        <- 0.75
NET_LAYOUT          <- "fr"
NET_LAYOUT_WEIGHTED <- TRUE
NET_SEED            <- 42

NET_COLOURS <- c(
  CU = "darkorange3",
  NI = "seagreen",
  LI = "gray",
  CO = "blue3"
)

# Output file stems, matching the names used by \includegraphics in the
# manuscript source. Change here if you rename the files in img/.
FIG_NETWORK <- "Metal_Network_21_25_thresh_75p_5color1"
FIG_BUBBLES <- "bubbles_up_only1"

# Save every figure as PDF (for LaTeX) and 300 dpi LZW TIFF (for the
# publisher's artwork requirements).
save_figure <- function(plot, stem, width, height) {
  ggsave(file.path(GRAPH_DIR, paste0(stem, ".pdf")), plot,
         width = width, height = height, bg = "white")
  ggsave(file.path(GRAPH_DIR, paste0(stem, ".tiff")), plot,
         width = width, height = height, dpi = 300,
         compression = "lzw", bg = "white")
  message("Saved figure: ", stem, " (.pdf, .tiff)")
}


# ---------------------------------------------------------------------
#  2. DATA
# ---------------------------------------------------------------------

df_prices <- read_csv(DATA_FILE, show_col_types = FALSE, guess_max = 10000) %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)

series_names <- base::setdiff(names(df_prices), "Date")

# --- validation: everything downstream indexes columns positionally ---
stopifnot(
  "Column `Date` is missing."          = "Date" %in% names(df_prices),
  "Expected exactly 16 price series."  = length(series_names) == 16,
  "Series names do not match EXPECTED_SERIES." =
    setequal(series_names, EXPECTED_SERIES),
  "Missing values present -- the input must be gap-filled." =
    !anyNA(df_prices),
  "Non-positive prices found -- log() would fail." =
    all(as.matrix(df_prices[series_names]) > 0)
)

if (min(df_prices$Date) != SAMPLE_START || max(df_prices$Date) != SAMPLE_END) {
  warning("Sample window is ", min(df_prices$Date), "..", max(df_prices$Date),
          ", expected ", SAMPLE_START, "..", SAMPLE_END, ".")
}

message("Loaded ", nrow(df_prices), " daily observations, ",
        length(series_names), " series, ",
        min(df_prices$Date), " to ", max(df_prices$Date))

# Log prices (levels used by GSADF and Johansen) and log returns.
df_log_prices <- df_prices %>% mutate(across(-Date, log))

df_returns <- df_prices %>%
  mutate(across(-Date, ~ log(.x / dplyr::lag(.x)))) %>%
  drop_na()

message("Returns: ", nrow(df_returns), " observations")


# ---------------------------------------------------------------------
#  3. DESCRIPTIVE STATISTICS  ->  Table 2
# ---------------------------------------------------------------------
# Q1/Q3 and ZeroPct are reported in the manuscript. Skewness, kurtosis,
# Jarque-Bera and ARCH-LM are computed here because the Data section
# refers to them, but are deliberately not tabulated.

df_desc <- df_returns %>%
  dplyr::select(-Date) %>%
  reframe(across(everything(), list(
    Mean      = ~ mean(.x, na.rm = TRUE),
    Std       = ~ sd(.x, na.rm = TRUE),
    Min       = ~ min(.x, na.rm = TRUE),
    Q1        = ~ unname(quantile(.x, 0.25, na.rm = TRUE)),
    Q3        = ~ unname(quantile(.x, 0.75, na.rm = TRUE)),
    Max       = ~ max(.x, na.rm = TRUE),
    Skew      = ~ skewness(.x, na.rm = TRUE),
    Kurt      = ~ kurtosis(.x, na.rm = TRUE),   # raw kurtosis: normal = 3
    ZeroPct   = ~ mean(.x == 0, na.rm = TRUE) * 100,
    JB_pval   = ~ moments::jarque.test(as.numeric(.x))$p.value,
    ARCH_pval = ~ ArchTest(.x, lags = 10, demean = TRUE)$p.value
  ))) %>%
  pivot_longer(
    cols          = everything(),
    names_to      = c("Ticker", ".value"),
    names_pattern = paste0("^(.+)_(Mean|Std|Min|Q1|Q3|Max|Skew|Kurt|",
                           "ZeroPct|JB_pval|ARCH_pval)$")
  ) %>%
  arrange(Ticker)

# Full set of statistics for the record.
write_csv2(df_desc %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
           file.path(RESULT_DIR, "metals1_tab2.csv"))

# Table 2 as printed in the manuscript.
tab2 <- df_desc %>%
  dplyr::select(Ticker, Mean, Std, Min, Q1, Q3, Max, ZeroPct) %>%
  mutate(across(where(is.numeric), ~ sprintf("%.3f", .x)))

latex_tab2 <- kable(
  tab2,
  format    = "latex",
  booktabs  = TRUE,
  linesep   = "",
  caption   = "Descriptive Statistics of Metal Price Returns",
  # knitr prepends "tab:" itself, so pass the label without it
  label     = "metals_returns_stats",
  col.names = c("Ticker", "Mean", "Std", "Min", "Q1", "Q3", "Max", "ZeroPct")
)
print(latex_tab2)
writeLines(as.character(latex_tab2), file.path(RESULT_DIR, "metals1_tab2.tex"))

message("\nJarque-Bera rejects normality at 1% for ",
        sum(df_desc$JB_pval < 0.01), "/16 series; ",
        "ARCH-LM rejects homoskedasticity at 1% for ",
        sum(df_desc$ARCH_pval < 0.01), "/16 series.")


# ---------------------------------------------------------------------
#  4. UNIT ROOT TESTS (ADF / PP)
# ---------------------------------------------------------------------
# Reported in the Data section: I(1) in levels, I(0) in first differences.

ur_stat <- function(x, test = c("adf", "pp")) {
  test <- match.arg(test)
  out <- if (test == "adf") {
    ur.df(x, type = "drift", selectlags = "AIC")
  } else {
    ur.pp(x, type = "Z-tau", model = "constant", lags = "short")
  }
  tibble(stat = as.numeric(out@teststat)[1],
         cv5  = out@cval[1, "5pct"],
         cv1  = out@cval[1, "1pct"])
}

unitroot_tests <- map_dfr(series_names, function(s) {
  lev <- df_log_prices[[s]]
  dif <- diff(lev)
  bind_rows(
    bind_cols(tibble(Ticker = s, Transform = "log level",      Test = "ADF"), ur_stat(lev, "adf")),
    bind_cols(tibble(Ticker = s, Transform = "log level",      Test = "PP"),  ur_stat(lev, "pp")),
    bind_cols(tibble(Ticker = s, Transform = "first difference", Test = "ADF"), ur_stat(dif, "adf")),
    bind_cols(tibble(Ticker = s, Transform = "first difference", Test = "PP"),  ur_stat(dif, "pp"))
  )
}) %>%
  mutate(Reject_1pct = stat < cv1)

write_csv2(unitroot_tests %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
           file.path(RESULT_DIR, "metals1_unitroot_tests.csv"))

unitroot_tests %>%
  count(Transform, Test, Reject_1pct) %>%
  print()


# ---------------------------------------------------------------------
#  5. CORRELATION NETWORK  ->  Figure 1
# ---------------------------------------------------------------------

cor_matrix <- cor(df_returns %>% dplyr::select(-Date), use = "pairwise.complete.obs")

# Correlation -> Euclidean distance -> edge weight.
# d(x,y) = sqrt(2 * (1 - rho)) maps rho in [-1, 1] to d in [0, 2];
# the weight is 2 - d, so a stronger correlation is a thicker edge.
dist_matrix   <- sqrt(2 * (1 - cor_matrix))
weight_matrix <- 2 - dist_matrix

# Keep only edges above the NET_QUANTILE of the upper-triangle correlations.
cor_threshold    <- quantile(cor_matrix[upper.tri(cor_matrix)], NET_QUANTILE, na.rm = TRUE)
weight_threshold <- 2 - sqrt(2 * (1 - cor_threshold))
weight_matrix    <- ifelse(weight_matrix >= weight_threshold, weight_matrix, 0)

message("Network: keeping edges with rho >= ", round(cor_threshold, 4),
        " (", NET_QUANTILE * 100, "th percentile)")

g <- graph_from_adjacency_matrix(weight_matrix, mode = "undirected",
                                 weighted = TRUE, diag = FALSE)

V(g)$strength <- strength(g, weights = E(g)$weight)  # node centrality
V(g)$metal    <- str_sub(V(g)$name, 1, 2)            # metal group prefix

set.seed(NET_SEED)
network_layout <- if (NET_LAYOUT_WEIGHTED) {
  create_layout(g, layout = NET_LAYOUT)
} else {
  create_layout(g, layout = NET_LAYOUT, weights = NA)
}

network_plot <- ggraph(network_layout) +
  geom_edge_link(aes(edge_width = weight), color = "red3", alpha = 0.7) +
  geom_node_point(aes(size = strength, fill = metal),
                  shape = 21, color = "black", stroke = 0.8) +
  geom_node_text(aes(label = name), repel = TRUE, size = 7.0,
                 fontface = "bold", color = "black",
                 force = 5, box.padding = 1.0, point.padding = 0.8,
                 max.overlaps = Inf, min.segment.length = Inf) +
  scale_edge_width_continuous(range = c(0.2, 4), guide = "none") +
  scale_size_continuous(range = c(6, 15), guide = "none") +
  scale_fill_manual(values = NET_COLOURS) +
  scale_x_continuous(expand = expansion(mult = 0.15)) +
  scale_y_continuous(expand = expansion(mult = 0.15)) +
  theme_void() +
  theme(legend.position = "none")

print(network_plot)
save_figure(network_plot, FIG_NETWORK, width = 9, height = 7)


# ---------------------------------------------------------------------
#  6. GSADF BUBBLE DETECTION
# ---------------------------------------------------------------------

data_log_matrix <- df_log_prices %>%
  dplyr::select(-Date) %>%
  as.matrix()
rownames(data_log_matrix) <- as.character(df_prices$Date)

n_obs      <- nrow(data_log_matrix)
min_window <- floor(n_obs * (0.01 + 1.8 / sqrt(n_obs)))
message("GSADF: T = ", n_obs, ", minimum window r0 = ", min_window, " days")

est_results <- radf(data_log_matrix)

# Monte Carlo critical values: expensive, so cached on disk.
if (file.exists(MC_CV_FILE)) {
  mc_cv <- readRDS(MC_CV_FILE)
  message("Loaded Monte Carlo critical values from cache.")
} else {
  mc_cv <- radf_mc_cv(n = n_obs, seed = MC_SEED)
  saveRDS(mc_cv, MC_CV_FILE)
  message("Computed and cached Monte Carlo critical values (seed = ", MC_SEED, ").")
}

summary(est_results, cv = mc_cv)


# ---------------------------------------------------------------------
#  7. BUBBLE EPISODES AND DUMMIES
# ---------------------------------------------------------------------
# The GSADF procedure flags explosive behaviour in either direction. Only
# episodes with a positive OLS trend in the price level are retained, so
# that "bubble" means exuberance rather than a collapse.

is_positive_trend <- function(start_idx, end_idx, prices) {
  p <- prices[start_idx:end_idx]
  if (length(p) > 2) coef(lm(p ~ seq_along(p)))[2] > 0 else tail(p, 1) > head(p, 1)
}

bubble_dates_up <- datestamp(est_results, cv = mc_cv) %>%
  imap(function(episodes, series) {
    if (is.null(episodes) || nrow(episodes) == 0) return(NULL)
    prices <- as.numeric(df_prices[[series]])
    keep   <- apply(episodes, 1, function(row)
      is_positive_trend(row["Start"], row["End"], prices))
    if (!any(keep)) return(NULL)
    episodes[keep, , drop = FALSE]
  }) %>%
  compact()

# One row per retained episode: used for both the figure and the table.
bubble_episodes <- if (length(bubble_dates_up) == 0) {
  tibble(Series = character(), Start = as.Date(character()), End = as.Date(character()))
} else {
  imap_dfr(bubble_dates_up, ~ tibble(
    Series = .y,
    Start  = df_prices$Date[.x[, "Start"]],
    End    = df_prices$Date[.x[, "End"]]
  ))
}

# 0/1 dummies for every series, including those with no episodes -- m12 and
# m13 expect 16 price columns followed by 16 dummy columns.
bubble_dummies <- map_dfc(series_names, function(series) {
  vec     <- rep(0L, nrow(df_prices))
  periods <- bubble_dates_up[[series]]
  if (!is.null(periods)) {
    for (i in seq_len(nrow(periods))) {
      vec[periods[i, "Start"]:periods[i, "End"]] <- 1L
    }
  }
  tibble(!!paste0(series, "_BD") := vec)
})

df_series_and_bubbles <- bind_cols(df_prices, bubble_dummies)

# m12 and m13 slice these columns positionally: [2:17] prices, [18:33] dummies.
stopifnot(
  ncol(df_series_and_bubbles) == 33,
  all(names(bubble_dummies) == paste0(series_names, "_BD"))
)

# Semicolon-separated with a decimal comma: m12 reads it with read.csv2()
# and m13 with read_csv2().
write_csv2(df_series_and_bubbles,
           file.path(RESULT_DIR, "series_and_bubble_up.csv"))
message("Wrote series_and_bubble_up.csv -- input for m12 and m13.")


# ---------------------------------------------------------------------
#  8. BUBBLE FIGURE  ->  Figure 2
# ---------------------------------------------------------------------

df_plot_long <- df_prices %>%
  pivot_longer(-Date, names_to = "Series", values_to = "Price")

bubble_plot <- ggplot() +
  {if (nrow(bubble_episodes) > 0)
    geom_rect(data = bubble_episodes,
              aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = 0.5)} +
  geom_line(data = df_plot_long, aes(x = Date, y = Price),
            color = "black", linewidth = 0.5) +
  facet_wrap(~ Series, scales = "free_y", ncol = 4) +
  theme_minimal() +
  theme(
    strip.text       = element_text(face = "plain", size = 14),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 9),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = NULL, y = NULL)

print(bubble_plot)
save_figure(bubble_plot, FIG_BUBBLES, width = 14, height = 10)


# ---------------------------------------------------------------------
#  9. BUBBLE SUMMARY  ->  Table 3
# ---------------------------------------------------------------------

bubble_summary <- tibble(
    Series         = series_names,
    Days_in_Bubble = as.integer(colSums(as.matrix(bubble_dummies)))
  ) %>%
  arrange(Series) %>%
  left_join(count(bubble_episodes, Series, name = "Number_of_Bubbles"), by = "Series") %>%
  mutate(Number_of_Bubbles = as.integer(replace_na(Number_of_Bubbles, 0L)))

write_csv2(bubble_summary, file.path(RESULT_DIR, "bubble_summary_up.csv"))
print(bubble_summary, n = 16)

# Publication layout: series stacked in blocks of four, one block per metal.
n_blocks <- ceiling(nrow(bubble_summary) / 4)

tab3 <- bubble_summary %>%
  mutate(Block   = ceiling(row_number() / 4),
         Col_Pos = (row_number() - 1) %% 4 + 1,
         across(everything(), as.character)) %>%
  pivot_longer(cols      = c(Series, Days_in_Bubble, Number_of_Bubbles),
               names_to  = "Metric",
               values_to = "Value") %>%
  mutate(Metric = factor(Metric,
                         levels = c("Series", "Days_in_Bubble", "Number_of_Bubbles"))) %>%
  arrange(Block, Metric, Col_Pos) %>%
  pivot_wider(id_cols      = c(Block, Metric),
              names_from   = Col_Pos,
              values_from  = Value,
              names_prefix = "Col_") %>%
  ungroup() %>%
  mutate(Metric = recode(as.character(Metric),
                         Series            = "",
                         Days_in_Bubble    = "Days in Bubble",
                         Number_of_Bubbles = "Number of Bubbles")) %>%
  dplyr::select(Metric, starts_with("Col_")) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))

latex_tab3 <- kable(
  tab3,
  format    = "latex",
  booktabs  = TRUE,
  col.names = NULL,
  linesep   = rep(c("", "", "\\addlinespace"), n_blocks),
  caption   = "Summary of speculative bubble episodes",
  # knitr prepends "tab:" itself, so pass the label without it
  label     = "bubble_summary_up"
)
print(latex_tab3)
writeLines(as.character(latex_tab3), file.path(RESULT_DIR, "metals1_tab3.tex"))


# ---------------------------------------------------------------------
#  10. H1: ILLIQUIDITY VERSUS BUBBLE INTENSITY
# ---------------------------------------------------------------------
# H1 predicts that bubble intensity is systematically related to relative
# illiquidity, proxied by the share of zero-return days (Lesmond et al.,
# 1999). Tested across the sixteen series with Pearson's r (linear) and
# Spearman's rho (rank, robust to the skew in the bubble counts).

h1_data <- df_desc %>%
  dplyr::select(Ticker, ZeroPct) %>%
  left_join(bubble_summary %>% rename(Ticker = Series), by = "Ticker")

stopifnot(nrow(h1_data) == 16, !anyNA(h1_data))

h1_correlations <- expand_grid(
  Measure = c("Days_in_Bubble", "Number_of_Bubbles"),
  Method  = c("pearson", "spearman")
) %>%
  mutate(
    test     = map2(Measure, Method,
                    ~ cor.test(h1_data$ZeroPct, h1_data[[.x]],
                               method = .y, exact = FALSE)),
    N        = nrow(h1_data),
    Estimate = map_dbl(test, ~ unname(.x$estimate)),
    P_Value  = map_dbl(test, ~ .x$p.value)
  ) %>%
  dplyr::select(Measure, Method, N, Estimate, P_Value) %>%
  mutate(across(c(Estimate, P_Value), ~ round(.x, 4)))

print(h1_correlations)
write_csv2(h1_correlations, file.path(RESULT_DIR, "metals1_h1_correlations.csv"))

message("\nH1: no coefficient significant at 5% -> ",
        all(h1_correlations$P_Value >= 0.05))

# =====================================================================
#  END
# =====================================================================
