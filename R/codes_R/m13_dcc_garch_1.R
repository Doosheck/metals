# ==============================================================================
# 1. SETUP & DATA PREPARATION (version unchecked corrected by Gemini)
# ==============================================================================
library(readr)
library(dplyr)
library(tidyr)
library(rugarch)
library(rmgarch)
library(ggplot2)
library(purrr)
library(here)

# Load data
df_metals <- read_csv2(here("R/results_R", "series_and_bubble_up.csv"), show_col_types = FALSE)

# Select only Date, Copper, and Nickel series, then calculate log returns
target_series <- c("NIDALY", "NILMEX", "NISHFE", "NIWUXI", "CUDALY", "CUCOMX", "CULMEX", "CUSHFE")

df_returns <- df_metals %>%
  dplyr::select(Date, dplyr::all_of(target_series)) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate(dplyr::across(-Date, ~ log(.x / dplyr::lag(.x)))) %>%
  tidyr::drop_na()

# ==============================================================================
# 2. MODEL SPECIFICATIONS
# ==============================================================================
# Base Univariate GARCH Specification
clean_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(5, 0), include.mean = TRUE),
  distribution.model = "std"
)

# Metal Groups for Intra-Commodity DCC
metal_groups <- list(
  Nickel = c("NIDALY", "NILMEX", "NISHFE", "NIWUXI"),
  Copper = c("CUDALY", "CUCOMX", "CULMEX", "CUSHFE")
)

# Minimalist Publication Theme for Plots
theme_article_fixed <- theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
    panel.grid.minor = element_line(color = "#f8f8f8", linewidth = 0.2),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10, color = "black"),
    axis.title = element_blank(),
    axis.text = element_text(size = 8, color = "gray30"),
    panel.spacing = unit(1.2, "lines"),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40")
  )

# ==============================================================================
# 3. INTRA-GROUP DCC ESTIMATION (COPPER & NICKEL)
# ==============================================================================
dcc_intra_results <- imap(metal_groups, function(series_names, group_name) {
  
  message("Estimating DCC model for: ", group_name)
  returns_matrix <- as.matrix(df_returns %>% dplyr::select(dplyr::all_of(series_names)))
  
  # Build multivariate spec
  n_series <- length(series_names)
  multi_spec <- multispec(replicate(n_series, clean_spec))
  
  dcc_spec <- dccspec(uspec = multi_spec, dccOrder = c(1, 1), distribution = "mvt")
  
  # Estimate with robust solver
  fit <- tryCatch({
    dccfit(spec = dcc_spec, data = returns_matrix, solver = "solnp", solver.control = list(trace = 0))
  }, error = function(e) {
    message("MVT failed. Switching to Normal distribution...")
    dcc_spec_norm <- dccspec(uspec = multi_spec, dccOrder = c(1, 1), distribution = "mvnorm")
    dccfit(spec = dcc_spec_norm, data = returns_matrix, solver = "nlminb")
  })
  
  # Extract correlations
  dcc_correlations <- rcor(fit)
  plot_data_list <- list()
  
  for (i in 1:(n_series - 1)) {
    for (j in (i + 1):n_series) {
      pair_name <- paste(series_names[i], "-", series_names[j])
      plot_data_list[[pair_name]] <- tibble::tibble(
        Date = df_returns$Date,
        Pair = pair_name,
        Correlation = dcc_correlations[i, j, ]
      )
    }
  }
  
  df_plot <- dplyr::bind_rows(plot_data_list)
  
  # Generate Plot
  p <- ggplot(df_plot, aes(x = Date, y = Correlation)) +
    geom_line(color = "black", linewidth = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.4) +
    facet_wrap(~ Pair, scales = "fixed", ncol = 2) + 
    scale_y_continuous(limits = c(-0.2, 1), breaks = seq(-0.2, 1, by = 0.2)) +
    labs(title = paste(group_name, "Group Dynamics"), subtitle = "Intra-commodity conditional correlations") +
    theme_article_fixed
  
  return(list(model = fit, data = df_plot, plot = p))
})

# Display and save Intra-Group Plots
print(dcc_intra_results$Nickel$plot)
print(dcc_intra_results$Copper$plot)

ggsave("R/graphs_R/correlation_nickel_final.pdf", plot = dcc_intra_results$Nickel$plot, width = 8, height = 6)
ggsave("R/graphs_R/correlation_copper_final.pdf", plot = dcc_intra_results$Copper$plot, width = 8, height = 6)


# ==============================================================================
# 4. CROSS-COMMODITY DCC ESTIMATION (COPPER vs NICKEL)
# ==============================================================================
message("Estimating Cross-Commodity DCC model for Copper vs Nickel...")

# Extract the leading benchmarks for the cross-commodity pair
cross_pair <- c("CUDALY", "NIDALY")
m_cross <- as.matrix(df_returns %>% dplyr::select(dplyr::all_of(cross_pair)))

# Specification for 2 variables
spec_cross <- dccspec(
  uspec = multispec(replicate(2, clean_spec)), 
  dccOrder = c(1, 1), 
  distribution = "mvt"
)

# Estimate Bivariate Model
fit_cross <- tryCatch({
  dccfit(spec = spec_cross, data = m_cross, solver = "solnp", solver.control = list(trace = 0))
}, error = function(e) {
  spec_cross_norm <- dccspec(uspec = multispec(replicate(2, clean_spec)), dccOrder = c(1, 1), distribution = "mvnorm")
  dccfit(spec = spec_cross_norm, data = m_cross, solver = "nlminb")
})

# Extract correlations for the single pair
cor_cross <- rcor(fit_cross)[1, 2, ]

df_cross_plot <- tibble::tibble(
  Date = df_returns$Date,
  Pair = "CUDALY - NIDALY",
  Correlation = cor_cross
)

# Generate Plot
plot_cross <- ggplot(df_cross_plot, aes(x = Date, y = Correlation)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.4) +
  scale_y_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.25)) +
  labs(
    title = "Cross-Commodity Dynamics: Copper vs Nickel", 
    subtitle = "Dynamic Conditional Correlation between LME benchmarks"
  ) +
  theme_article_fixed +
  theme(plot.title = element_text(face = "bold", size = 14)) # Slightly larger title for a single plot

# Display and save Cross-Commodity Plot
print(plot_cross)
ggsave("R/graphs_R/correlation_copper_vs_nickel.pdf", plot = plot_cross, width = 8, height = 5)

message("Pipeline completed successfully!")