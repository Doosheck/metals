# --- 1. Load Libraries ---
library(tidyverse)
library(rugarch)
library(here)

# --- 2. Load Processed Data ---
#write_csv2(df_final_dataset_up, here("R/results_R", "series_and_bubble_up.csv"))
df_metals  <- read_csv2(here("R/results_R", "series_and_bubble_up.csv"), show_col_types = FALSE)
df_prices <- df_metals[1:17]
df_dummies <- df_metals[c(1,18:33)]
#old files
  #df_dummies <- read_csv(here("data", "bubble_dummies.csv"), show_col_types = FALSE)
#df_dummies_sign <- read_csv(here("data", "bubble_dummies_separate.csv"), show_col_types = FALSE)

# --- 3. Data Transformation ---
# Log returns from log prices
df_returns <- df_prices %>%
  arrange(Date) %>%
  mutate(across(-Date, ~ .x - lag(.x))) %>%
  drop_na()

# Align dummies with returns (dropping the first row because returns lose 1 observation)
df_dummies_aligned <- df_dummies %>%
  arrange(Date) %>%
  slice(-1)


# --- 4. GARCH-X Estimation & Results Extraction ---

# Identify the base names of the metals (e.g., "NI", "CU")
metal_names <- names(df_prices)[names(df_prices) != "Date"]

# Initialize a list to store standardized residuals for DCC
std_resid_list <- list(Date = df_returns$Date)

# --- 2. The Estimation Loop ---
#garch_results <- map(metal_names, function(m_name) {
  
  # A. Define the Directional Dummies (Statistical Approach)
  y <- df_returns[[m_name]]
  dummy_active <- df_dummies_aligned[[m_name]]
  
  d_expanding  <- as.integer(dummy_active == 1 & y >= 0)
  d_collapsing <- as.integer(dummy_active == 1 & y < 0)
  
  # B. Only include regressors if bubbles exist for this series
  has_bubbles <- sum(dummy_active) > 0
  ext_reg <- if(has_bubbles) cbind(Exp = d_expanding, Col = d_collapsing) else NULL
  
  # C. Specification (using your preferred GARCH type and orders)
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(5, 0), include.mean = TRUE, external.regressors = ext_reg),
    distribution.model = "std"
  )
  
  # D. Estimation
  fit <- tryCatch({
    ugarchfit(spec = spec, data = y, solver = "hybrid")
  }, error = function(e) return(NULL))
  
  # E. Extract Standardized Residuals if converged
  if (!is.null(fit) && fit@fit$convergence == 0) {
    std_resid_list[[m_name]] <<- as.numeric(residuals(fit, standardize = TRUE))
    return(fit)
  } else {
    message("Model failed or did not converge for: ", m_name)
    return(NULL)
  }
}) %>% set_names(metal_names)

garch_results <- map(metal_names, function(m_name) {
  
  # 1. Prepare data
  y <- df_returns[[m_name]]
  
  # Construct the dummy column name (e.g., "COBALT_BD")
  dummy_col_name <- paste0(m_name, "_BD")
  
  # Fetch the dummy vector if it exists, otherwise use zeros
  if (dummy_col_name %in% names(df_dummies_aligned)) {
    dummy_active <- as.numeric(df_dummies_aligned[[dummy_col_name]])
  } else {
    dummy_active <- rep(0, length(y))
  }
  
  has_bubbles <- sum(dummy_active, na.rm = TRUE) > 0
  
  # 2. Base "Clean" Specification (No external regressors)
  spec_clean <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(5, 0), include.mean = TRUE),
    distribution.model = "std"
  )
  
  # 3. "Bubble" Specification (If bubbles exist)
  if (has_bubbles) {
    # It must be a matrix for rugarch!
    ext_reg <- as.matrix(dummy_active)
    colnames(ext_reg) <- c("Bubble")
    
    spec_bubble <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = ext_reg),
      mean.model     = list(armaOrder = c(5, 0), include.mean = TRUE, external.regressors = ext_reg),
      distribution.model = "std"
    )
  } else {
    spec_bubble <- NULL
  }
  
  # 4. Estimation (Using tryCatch to prevent loop crashing)
  fit_clean <- tryCatch({
    ugarchfit(spec = spec_clean, data = y, solver = "hybrid")
  }, error = function(e) return(NULL))
  
  fit_bubble <- if (has_bubbles) {
    tryCatch({
      ugarchfit(spec = spec_bubble, data = y, solver = "hybrid")
    }, error = function(e) return(NULL))
  } else {
    NULL
  }
  
  # 5. Check convergence and return results
  # We return a list containing both models and their status
  clean_converged <- !is.null(fit_clean) && fit_clean@fit$convergence == 0
  bubble_converged <- !is.null(fit_bubble) && fit_bubble@fit$convergence == 0
  
  if (!clean_converged) message("Clean model failed to converge for: ", m_name)
  if (has_bubbles && !bubble_converged) message("Bubble model failed to converge for: ", m_name)
  
  return(list(
    clean_model = fit_clean,
    bubble_model = fit_bubble,
    has_bubbles = has_bubbles,
    clean_converged = clean_converged,
    bubble_converged = bubble_converged
  ))
  
}) %>% set_names(metal_names)

# --- 3. Diagnostic Summary Table ---
diagnostic_summary <- map_df(names(garch_results), function(name) {
  
  # 1. Extract the list containing both models for the given metal
  res_list <- garch_results[[name]]
  
  # Helper function to extract diagnostics for a single GARCH fit
  get_diagnostics <- function(fit, model_type) {
    if (is.null(fit)) return(NULL)
    
    # Basic Stats
    log_likelihood <- fit@fit$LLH
    ic <- infocriteria(fit)
    
    # Extract Standardized Residuals for Testing
    res <- as.numeric(residuals(fit, standardize = TRUE))
    
    # Ljung-Box Test (on Residuals) - Testing for Mean Equation adequacy
    lb_test <- Box.test(res, lag = 10, type = "Ljung-Box")
    
    # ARCH-LM Test (on Squared Residuals) - Testing for Variance Equation adequacy
    arch_test <- Box.test(res^2, lag = 10, type = "Ljung-Box")
    
    # Create a row of results
    tibble(
      Series       = name,
      Model_Type   = model_type,
      Converged    = ifelse(fit@fit$convergence == 0, "Yes", "No"),
      AIC          = ic[1],
      BIC          = ic[2],
      LogLik       = log_likelihood,
      
      # Extracting Alpha and Beta estimates (column 1 in matcoef contains estimates)
      Alpha1   = if("alpha1" %in% rownames(fit@fit$matcoef)) fit@fit$matcoef["alpha1", 1] else NA,
      Beta1    = if("beta1" %in% rownames(fit@fit$matcoef)) fit@fit$matcoef["beta1", 1] else NA,
      
      AlphaBeta = Alpha1 + Beta1,
      
      LB_P_Value   = round(lb_test$p.value, 3),  
      
      # Rounding to 3 decimal places to avoid scientific notation (instead of forcing an integer)
      ARCH_P_Value = round(arch_test$p.value, 3),
      
      # Now checking for the correct regressor names assigned by rugarch (mxreg1, vxreg1)
      P_Bubble_Mean = if("mxreg1" %in% rownames(fit@fit$matcoef)) fit@fit$matcoef["mxreg1", 4] else NA,
      P_Bubble_Var  = if("vxreg1" %in% rownames(fit@fit$matcoef)) fit@fit$matcoef["vxreg1", 4] else NA
    )
  }
  
  # 2. Extract stats for both models and bind them together
  clean_stats  <- get_diagnostics(res_list$clean_model, "Clean")
  bubble_stats <- get_diagnostics(res_list$bubble_model, "Bubble")
  
  bind_rows(clean_stats, bubble_stats)
})

#diagnostic_summary <- map_df(names(garch_results), function(name) {
  fit <- garch_results[[name]]
  if (is.null(fit)) return(NULL)
  
  # 1. Basic Stats
  log_likelihood <- fit@fit$LLH
  ic <- infocriteria(fit)
  
  # 2. Extract Standardized Residuals for Testing
  res <- as.numeric(residuals(fit, standardize = TRUE))
  
  # 3. Ljung-Box Test (on Residuals) - Testing for Mean Equation adequacy
  # Null Hypothesis: No autocorrelation. We want p > 0.05
  lb_test <- Box.test(res, lag = 10, type = "Ljung-Box")
  
  # 4. ARCH-LM Test (on Squared Residuals) - Testing for Variance Equation adequacy
  # Null Hypothesis: No remaining ARCH effects. We want p > 0.05
  arch_test <- Box.test(res^2, lag = 10, type = "Ljung-Box")
  
  tibble(
    Series    = name,
    Converged = ifelse(fit@fit$convergence == 0, "Yes", "No"),
    AIC       = ic[1],
    BIC       = ic[2],
    LogLik    = log_likelihood,
    # P-values for Diagnostics
    LB_P_Value   = lb_test$p.value,    # Should be > 0.05
    ARCH_P_Value = arch_test$p.value,  # Should be > 0.05
    # Parameter significance
    P_Collapse_Mean = if("mxreg2" %in% rownames(fit@fit$matcoef)) fit@fit$matcoef["mxreg2", 4] else NA,
    P_Collapse_Var  = if("vxreg2" %in% rownames(fit@fit$matcoef)) fit@fit$matcoef["vxreg2", 4] else NA
  )
})

# View the result
print(diagnostic_summary, n = 30)

library(dplyr)
library(tidyr)

model_comparison <- diagnostic_summary %>%
  # 1. Select only the key columns needed for comparison
  dplyr::select(Series, Model_Type, AIC, BIC, P_Bubble_Mean, P_Bubble_Var) %>%
  
  # 2. Reshape the table to wider format so Clean and Bubble models are side-by-side
  pivot_wider(
    id_cols = Series,
    names_from = Model_Type,
    values_from = c(AIC, BIC, P_Bubble_Mean, P_Bubble_Var)
  ) %>%
  
  # 3. Drop empty parameter columns from the Clean model (since it had no exogenous variables) 
  # and assign cleaner names to the remaining Bubble parameter columns
  dplyr::select(
    Series,
    AIC_Clean = AIC_Clean,
    AIC_Bubble = AIC_Bubble,
    BIC_Clean = BIC_Clean,
    BIC_Bubble = BIC_Bubble,
    P_Bubble_Mean = P_Bubble_Mean_Bubble,
    P_Bubble_Var = P_Bubble_Var_Bubble
  ) %>%
  
  # 4. Add an automatic verdict based on Information Criteria (lower is better) 
  # and round the p-values to 3 decimal places for readability
  mutate(
    Better_AIC = ifelse(AIC_Bubble < AIC_Clean, "Bubble", "Clean"),
    Better_BIC = ifelse(BIC_Bubble < BIC_Clean, "Bubble", "Clean"),
    P_Bubble_Mean = round(P_Bubble_Mean, 3),
    P_Bubble_Var = round(P_Bubble_Var, 3)
  ) %>%
  
  # 5. Reorder columns for a logical flow in the final output
  dplyr::select(Series, AIC_Clean, AIC_Bubble, Better_AIC, BIC_Clean, BIC_Bubble, Better_BIC, P_Bubble_Mean, P_Bubble_Var)

# Print the resulting table
print(model_comparison, width = Inf)


# --- 4. Export for DCC GARCH ---



# Extract standardized residuals directly from the "Clean" models
df_std_residuals <- purrr::map_dfc(names(garch_results), function(m_name) {
  
  # Retrieve the clean model for the given metal
  fit_clean <- garch_results[[m_name]]$clean_model
  
  # If the model exists (converged), extract residuals and name the column after the metal
  if (!is.null(fit_clean)) {
    std_res <- as.numeric(residuals(fit_clean, standardize = TRUE))
    
    # Return as a single-column tibble (map_dfc will bind them column-wise into one wide table)
    tibble::tibble(!!m_name := std_res)
  }
})

write_csv(df_std_residuals, here("R", "results_R", "dcc_std_residual.csv"))

library(rmgarch)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# 1. Define the 4 groups of metals (modify if your grouping logic is different)
metal_groups <- list(
  Nickel  = c("NIDALY", "NILMEX", "NISHFE", "NIWUXI"),
  Copper  = c("CUDALY", "CUCOMX", "CULMEX", "CUSHFE"),
  Lithium = c("LIDALY", "LISAME", "LICOMX", "LILMEX"),
  Cobalt  = c("CODALY", "COLMEX", "COWUXI", "COCOMX")
)

# 2. Base Specification (Clean GARCH for margins)
clean_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(5, 0), include.mean = TRUE),
  distribution.model = "std"
)

# 3. Estimate DCC for each group and generate plots automatically
# imap allows us to iterate over the list and keep the name of the group (e.g., "Nickel")
#dcc_all_results <- imap(metal_groups, function(series_names, group_name) {
  
  message("--------------------------------------------------")
  message("Estimating STANDARD DCC model for group: ", group_name)
  
  # A. Prepare Data Matrix
  returns_matrix <- df_returns %>%
    dplyr::select(dplyr::all_of(series_names)) %>%
    as.matrix()
  
  # B. Build the standard DCC specification
  n_series <- length(series_names)
  multi_spec <- multispec(replicate(n_series, clean_spec))
  
  # This is the "Standard" Engle (2002) DCC spec
  dcc_spec <- dccspec(
    uspec = multi_spec, 
    dccOrder = c(1, 1), 
    distribution = "mvt" # Multivariate t-Student
  )
  
  # C. Estimation using dccfit (standard DCC)
  fit <- tryCatch({
    # Switching to 'hybrid' solver which is much more resilient
    dccfit(
      spec = dcc_spec, 
      data = returns_matrix, 
      solver = "hybrid", 
      solver.control = list(maxit = 5000)
    )
  }, error = function(e) {
    # Plan B: If hybrid fails, try switching distribution to "norm" inside the loop
    message("Hybrid solver failed for ", group_name, ". Attempting with Normal distribution...")
    
    dcc_spec_norm <- dccspec(
      uspec = multi_spec, 
      dccOrder = c(1, 1), 
      distribution = "norm" # Simpler likelihood function
    )
    
    tryCatch({
      dccfit(spec = dcc_spec_norm, data = returns_matrix, solver = "hybrid")
    }, error = function(e_final) {
      message("Critical failure for ", group_name, ": ", e_final$message)
      return(NULL)
    })
  })
  
  if (is.null(fit)) return(NULL)
  
  # D. Extract Correlations 
  # In standard dccfit, rcor() returns the 3D array [i, j, time] directly
  dcc_correlations <- rcor(fit)
  time_index <- df_returns$Date 
  
  plot_data_list <- list()
  
  # Loop to extract unique pairs
  for (i in 1:(n_series - 1)) {
    for (j in (i + 1):n_series) {
      pair_name <- paste(series_names[i], "-", series_names[j])
      
      # Now the 3D array indexing works perfectly
      cor_vector <- dcc_correlations[i, j, ]
      
      plot_data_list[[pair_name]] <- tibble::tibble(
        Date = time_index,
        Pair = pair_name,
        Correlation = cor_vector
      )
    }
  }
  
  df_dcc_plot <- dplyr::bind_rows(plot_data_list)
  
  # E. Create the Plot
  p <- ggplot(df_dcc_plot, aes(x = Date, y = Correlation)) +
    geom_line(color = "darkred", linewidth = 0.5) + # Changed color to distinguish
    facet_wrap(~ Pair, scales = "fixed", ncol = 2) + 
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
    labs(
      title = paste("Standard DCC-GARCH Correlations -", group_name),
      subtitle = "Based on Engle (2002) approach",
      x = "Date",
      y = "Conditional Correlation"
    ) +
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = "gray95", color = NA),
      strip.text = element_text(face = "bold")
    )
  
  return(list(
    model = fit,
    plot  = p,
    data  = df_dcc_plot
  ))
})

sum(is.na(df_returns))

dcc_all_results <- imap(metal_groups, function(series_names, group_name) {
  
  message("--------------------------------------------------")
  message("Estimating STANDARD DCC model for group: ", group_name)
  
  # A. Prepare Data Matrix
  # Ensure only selected columns are used and converted to matrix
  returns_matrix <- df_returns %>%
    dplyr::select(dplyr::all_of(series_names)) %>%
    as.matrix()
  
  # B. Build the standard DCC specification
  n_series <- length(series_names)
  multi_spec <- multispec(replicate(n_series, clean_spec))
  
  # Primary Specification: Multivariate t-Student (mvt)
  dcc_spec <- dccspec(
    uspec = multi_spec, 
    dccOrder = c(1, 1), 
    distribution = "mvt"
  )
  
  # C. Estimation using dccfit with fallback logic (SOLVER FIXED)
  fit <- tryCatch({
    # Attempt 1: Standard DCC with Multivariate t-Student (mvt) and "solnp"
    dccfit(
      spec = dcc_spec, 
      data = returns_matrix, 
      solver = "solnp", 
      solver.control = list(trace = 0) # Quiet output
    )
  }, error = function(e) {
    message("MVT failed for ", group_name, ". Trying Plan B (mvnorm + nlminb)...")
    
    # Attempt 2: Plan B - Multivariate Normal (mvnorm) and a different solver (nlminb)
    # nlminb is often more robust when solnp hits a singularity
    dcc_spec_norm <- dccspec(
      uspec = multi_spec, 
      dccOrder = c(1, 1), 
      distribution = "mvnorm" 
    )
    
    tryCatch({
      dccfit(
        spec = dcc_spec_norm, 
        data = returns_matrix, 
        solver = "nlminb"
      )
    }, error = function(e_final) {
      message("Critical failure for ", group_name, ": ", e_final$message)
      return(NULL)
    })
  })
  
  # D. Extract Correlations 
  # rcor() returns a 3D array [asset_i, asset_j, time]
  dcc_correlations <- rcor(fit)
  time_index <- df_returns$Date 
  
  plot_data_list <- list()
  
  # Loop to extract unique pairwise correlations
  for (i in 1:(n_series - 1)) {
    for (j in (i + 1):n_series) {
      pair_name <- paste(series_names[i], "-", series_names[j])
      
      # Extract the correlation vector across the time dimension
      cor_vector <- dcc_correlations[i, j, ]
      
      plot_data_list[[pair_name]] <- tibble::tibble(
        Date = time_index,
        Pair = pair_name,
        Correlation = cor_vector
      )
    }
  }
  
  # Combine pairwise data for plotting
  df_dcc_plot <- dplyr::bind_rows(plot_data_list)
  
  # E. Create the Plot
  p <- ggplot(df_dcc_plot, aes(x = Date, y = Correlation)) +
    geom_line(color = "darkred", linewidth = 0.5) +
    facet_wrap(~ Pair, scales = "fixed", ncol = 2) + 
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
    labs(
      title = paste("Standard DCC-GARCH Correlations -", group_name),
      subtitle = paste("Model Distribution:", fit@model$modeldesc$distribution),
      x = "Year",
      y = "Conditional Correlation"
    ) +
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = "gray95", color = NA),
      strip.text = element_text(face = "bold")
    )
  
  # Return final objects as a list
  return(list(
    model = fit,
    plot  = p,
    data  = df_dcc_plot
  ))
})

## plot final


theme_article_fixed <- theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
    panel.grid.minor = element_line(color = "#f8f8f8", linewidth = 0.2),
    strip.background = element_blank(),
    strip.text = element_text(face = "plain", size = 10, color = "black"),
    axis.title = element_blank(),
    axis.text = element_text(size = 8, color = "gray30"),
    panel.spacing = unit(1.2, "lines"),
    plot.title = element_blank(),
    plot.subtitle = element_blank()
  )
# --- WYKRES DLA NIKLU ---
p_nickel_final <- ggplot(dcc_all_results$Nickel$data, aes(x = Date, y = Correlation)) +
  geom_line(color = "black", linewidth = 0.4) + # Cienka czarna linia
  facet_wrap(~ Pair, ncol = 3, scales = "fixed") + 
  scale_y_continuous(limits = c(-0.2, 1), breaks = seq(-0.2, 1, 0.2)) +
  theme_article_fixed

# --- WYKRES DLA MIEDZI ---
p_copper_final <- ggplot(dcc_all_results$Copper$data, aes(x = Date, y = Correlation)) +
  geom_line(color = "black", linewidth = 0.4) +
  facet_wrap(~ Pair, ncol = 3, scales = "fixed") + 
  scale_y_continuous(limits = c(-0.2, 1), breaks = seq(-0.2, 1, 0.2)) +
  theme_article_fixed

# Wyœwietlenie (bêd¹ wygl¹daæ jak Twój PDF)
print(p_nickel_final)
print(p_copper_final)

ggsave("R/graphs_R/correlation_nickel_final.pdf", plot = p_nickel_final, width = 10, height = 6)
ggsave("R/graphs_R/correlation_copper_final.pdf", plot = p_copper_final, width = 10, height = 6)

#---- 4 metals DCC ----
library(rugarch)
library(rmgarch)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Select the 4 main benchmarks for cross-commodity analysis
cross_metals <- c("CUDALY", "NIDALY", "LIDALY", "CODALY")

message("Preparing data for the 4D model...")
df_cross <- df_returns %>%
  dplyr::select(Date, dplyr::all_of(cross_metals)) %>%
  na.omit()

# Convert to matrix format required by rmgarch
m_cross <- as.matrix(df_cross %>% dplyr::select(-Date))

# 2. Univariate specification (same as used previously)
dcc_margin_spec <- rugarch::ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(5, 0), include.mean = TRUE),
  distribution.model = "std"
)

# 3. Multivariate specification (4 dimensions)
# Primary attempt: Multivariate t-Student (MVT) distribution
spec_4d_mvt <- dccspec(
  uspec = multispec(replicate(4, dcc_margin_spec)), 
  dccOrder = c(1, 1), 
  distribution = "mvt"
)

# Fallback attempt: Multivariate Normal (MVNORM) distribution
spec_4d_norm <- dccspec(
  uspec = multispec(replicate(4, dcc_margin_spec)), 
  dccOrder = c(1, 1), 
  distribution = "mvnorm"
)

# 4. Estimation (with double safety fallback)
message("Starting 4D DCC model estimation. This may take a few minutes...")

fit_4d <- tryCatch({
  # eval.se = FALSE speeds up estimation by skipping standard error calculations
  dccfit(spec = spec_4d_mvt, data = m_cross, solver = "solnp", fit.control = list(eval.se = FALSE))
}, error = function(e) {
  message("MVT failed to converge for 4 dimensions. Trying multivariate normal distribution...")
  tryCatch({
    dccfit(spec = spec_4d_norm, data = m_cross, solver = "solnp", fit.control = list(eval.se = FALSE))
  }, error = function(e2) {
    message("Critical error: 4D model could not be estimated. Likely due to illiquidity (zeros) in LIDALY/CODALY.")
    return(NULL)
  })
})

# 5. Extract results (if the model converged successfully)
if (!is.null(fit_4d) && inherits(fit_4d, "DCCfit")) {
  message("Success! Model estimated. Extracting conditional correlations...")
  
  # rcor returns a 3D array: [asset i, asset j, time t]
  cor_array <- rcor(fit_4d)
  
  # Generate all unique pair combinations from the 4 metals (4 choose 2 = 6 pairs)
  pair_indices <- combn(1:4, 2)
  
  # Loop to extract the correlation vector for each pair across the time dimension
  list_cor_dfs <- lapply(1:ncol(pair_indices), function(i) {
    idx1 <- pair_indices[1, i]
    idx2 <- pair_indices[2, i]
    
    metal1 <- cross_metals[idx1]
    metal2 <- cross_metals[idx2]
    
    tibble::tibble(
      Date = df_cross$Date,
      Pair = paste(metal1, "-", metal2),
      Correlation = cor_array[idx1, idx2, ]
    )
  })
  
  # Combine all pairs into a single dataframe for ggplot
  df_cross_correlations <- dplyr::bind_rows(list_cor_dfs)
  
  # 6. Plot the results using the article's minimalist aesthetic
  theme_article_fixed <- theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
      panel.grid.minor = element_line(color = "#f8f8f8", linewidth = 0.2),
      strip.background = element_blank(),
      strip.text = element_text(face = "plain", size = 10, color = "black"),
      axis.title = element_blank(),
      axis.text = element_text(size = 8, color = "gray30"),
      panel.spacing = unit(1.2, "lines")
    )
  
  plot_4d <- ggplot(df_cross_correlations, aes(x = Date, y = Correlation)) +
    geom_line(color = "black", linewidth = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray80", linewidth = 0.3) +
    # Layout: 6 panels across 3 columns (2 rows)
    facet_wrap(~ Pair, ncol = 3, scales = "fixed") + 
    # Y-axis bounds slightly relaxed compared to intra-metal correlations
    scale_y_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, 0.25)) +
    theme_article_fixed
  
  print(plot_4d)
  
  # Optional: Save the plot as a high-quality PDF for publication
  # ggsave("Cross_Metals_DCC.pdf", plot = plot_4d, width = 10, height = 6)
  
} else {
  message("Estimation failed entirely. Consider excluding Lithium or Cobalt to run a 3D or bivariate model.")
}


##----plot 4 DALY series in DCC
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Define the minimalist article theme (ensuring it's loaded in the environment)
theme_article_fixed <- theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
    panel.grid.minor = element_line(color = "#f8f8f8", linewidth = 0.2),
    strip.background = element_blank(),
    strip.text = element_text(face = "plain", size = 10, color = "black"),
    axis.title = element_blank(),
    axis.text = element_text(size = 8, color = "gray30"),
    panel.spacing = unit(1.2, "lines"),
    plot.title = element_blank(),
    plot.subtitle = element_blank()
  )

# 2. Extract the conditional correlation matrix from the 4D model
# Assuming the estimated model is saved as 'fit_4d' and the metals are:
cross_metals <- c("CUDALY", "NIDALY", "LIDALY", "CODALY")
cor_array <- rcor(fit_4d)

# Generate all unique pair combinations (4 choose 2 = 6 pairs)
pair_indices <- combn(1:4, 2)

# Loop to extract the correlation vector for each pair and bind them into a dataframe
list_cor_dfs <- lapply(1:ncol(pair_indices), function(i) {
  idx1 <- pair_indices[1, i]
  idx2 <- pair_indices[2, i]
  
  metal1 <- cross_metals[idx1]
  metal2 <- cross_metals[idx2]
  
  tibble::tibble(
    Date = df_cross$Date, # Ensure df_cross represents the data used for estimation
    Pair = paste(metal1, "-", metal2),
    Correlation = cor_array[idx1, idx2, ]
  )
})

# Combine the list of dataframes into a single tidy dataframe
df_cross_correlations <- dplyr::bind_rows(list_cor_dfs)

# 3. Plot the results using the minimalist aesthetic
plot_cross_metals <- ggplot(df_cross_correlations, aes(x = Date, y = Correlation)) +
  # Thin, black line for correlation dynamics
  geom_line(color = "black", linewidth = 0.4) +
  # Reference line at zero correlation
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray80", linewidth = 0.3) +
  # Facet layout: 3 columns (creates 2 rows for the 6 pairs)
  facet_wrap(~ Pair, ncol = 3, scales = "fixed") + 
  # Y-axis bounds set from -0.5 to 1.0 (cross-metal correlations are typically lower)
  scale_y_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, 0.25)) +
  theme_article_fixed

# Display the final plot
print(plot_cross_metals)

# 4. Save the plot for publication (PDF and high-resolution PNG)
ggsave("R/graphs_R/DCC_4metals.pdf", plot = plot_cross_metals, width = 10, height = 6)
# ggsave("R/graphs_R/DCC_4metals.png", plot = plot_cross_metals, width = 10, height = 6, dpi = 300)

## ---- plot nikiel and copper - skip----
library(ggplot2)
library(dplyr)

# --- 1. Combined Plot for NICKEL ---
df_nickel <- dcc_all_results$Nickel$data

plot_nickel_combined <- ggplot(df_nickel, aes(x = Date, y = Correlation, color = Pair)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.2)) +
  theme_minimal() +
  scale_color_viridis_d(option = "plasma") + # Inne kolory dla odró¿nienia od Litu
  labs(
    title = "Dynamic Conditional Correlations: NICKEL Group",
    subtitle = "Higher liquidity markets show much more dynamic behavior",
    x = "Year",
    y = "Correlation",
    color = "Nickel Pairs"
  ) +
  theme(legend.position = "bottom", legend.text = element_text(size = 7))

# --- 2. Combined Plot for COPPER ---
df_copper <- dcc_all_results$Copper$data

plot_copper_combined <- ggplot(df_copper, aes(x = Date, y = Correlation, color = Pair)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.2)) +
  theme_minimal() +
  scale_color_viridis_d(option = "viridis") + 
  labs(
    title = "Dynamic Conditional Correlations: COPPER Group",
    subtitle = "Global benchmark for industrial activity",
    x = "Year",
    y = "Correlation",
    color = "Copper Pairs"
  ) +
  theme(legend.position = "bottom", legend.text = element_text(size = 7))

# Wyœwietlenie
print(plot_nickel_combined)
print(plot_copper_combined)

# library(ggplot2)
library(dplyr)

# --- generate 6 graphs ---
plot_market_grid <- function(df_data, market_name, line_color) {
  ggplot(df_data, aes(x = Date, y = Correlation)) +
    # add correlation
    geom_line(color = line_color, linewidth = 0.5) +
    # reference
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    # 6 graphs in 2 columns
    facet_wrap(~ Pair, ncol = 2, scales = "fixed") + 
    # aestetics
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
    theme_minimal() +
    labs(
      title = paste("Dynamic Correlations (DCC-GARCH) -", market_name),
      subtitle = "Six unique pairwise relationships",
      x = "Year",
      y = "Correlation"
    ) +
    theme(
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      strip.text = element_text(face = "bold", size = 9),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14)
    )
}

# --- generate plots ---
# Nickel 
p_nickel_grid <- plot_market_grid(dcc_all_results$Nickel$data, "NICKEL", "steelblue4")

# copper
p_copper_grid <- plot_market_grid(dcc_all_results$Copper$data, "COPPER", "chocolate4")

# Wyœwietlenie
print(p_nickel_grid)
print(p_copper_grid)

##---- solution for Lithium and cobalt:
# Bivariate DCC Loop for "Difficult" groups

library(purrr)
library(dplyr)
library(ggplot2)
library(rmgarch)

dcc_margin_spec <- rugarch::ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(5, 0), include.mean = TRUE),
  distribution.model = "std"
)
print(class(dcc_margin_spec))

target_groups <- list(
  Lithium = c("LIDALY", "LISAME", "LICOMX", "LILMEX"),
  Cobalt  = c("CODALY", "COLMEX", "COWUXI", "COCOMX")
)

dcc_lithium_cobalt_results <- imap(target_groups, function(series_names, group_name) {
  
  message("\n==================================================")
  message("GROUP: ", group_name)
  
  all_pairs <- combn(series_names, 2, simplify = FALSE)
  
  pair_results <- map(all_pairs, function(pair) {
    message("--> Checking pair: ", paste(pair, collapse=" vs "))
    
    # 1. Prepare data
    pair_data <- df_returns %>% 
      dplyr::select(Date, dplyr::all_of(pair)) %>% 
      na.omit()
    
    m_data <- as.matrix(pair_data %>% dplyr::select(-Date))
    
    # 2. PRE-FLIGHT CHECK: Use the NEW unique spec name
    # We use 'hybrid' solver here as it's more robust for univariate GARCH
    fit1 <- tryCatch(rugarch::ugarchfit(spec = dcc_margin_spec, data = m_data[,1], solver = "hybrid"), error = function(e) NULL)
    fit2 <- tryCatch(rugarch::ugarchfit(spec = dcc_margin_spec, data = m_data[,2], solver = "hybrid"), error = function(e) NULL)
    
    if (is.null(fit1) || is.null(fit2) || 
        rugarch::convergence(fit1) != 0 || rugarch::convergence(fit2) != 0) {
      message("    !! Skipping: Univariate GARCH failed to converge.")
      return(NULL)
    }
    
    # 3. DCC SPECIFICATION
    # Replicating the new unique spec
    spec_dcc_mvt <- dccspec(
      uspec = multispec(replicate(2, dcc_margin_spec)), 
      dccOrder = c(1, 1), 
      distribution = "mvt"
    )
    
    # 4. DCC ESTIMATION
    fit_dcc <- tryCatch({
      dccfit(spec = spec_dcc_mvt, data = m_data, solver = "solnp")
    }, error = function(e) {
      # Plan B: Normal distribution
      spec_dcc_norm <- dccspec(multispec(replicate(2, dcc_margin_spec)), distribution = "mvnorm")
      tryCatch(dccfit(spec = spec_dcc_norm, data = m_data, solver = "solnp"), error = function(e2) NULL)
    })
    
    # 5. VALIDATION & EXTRACTION
    if (!inherits(fit_dcc, "DCCfit")) return(NULL)
    
    cor_data <- tryCatch({
      rc <- rcor(fit_dcc)
      if (is.null(rc) || length(dim(rc)) < 3) stop("Empty")
      rc[1, 2, ]
    }, error = function(e) NULL)
    
    if (is.null(cor_data)) return(NULL)
    
    message("    SUCCESS: Pair converged and data extracted.")
    
    tibble::tibble(
      Date = pair_data$Date,
      Pair = paste(pair, collapse=" - "),
      Correlation = cor_data
    )
  })
  
  # Final steps: bind and plot (same as before)
  df_group_results <- dplyr::bind_rows(compact(pair_results))
  
  if (nrow(df_group_results) > 0) {
    p <- ggplot(df_group_results, aes(x = Date, y = Correlation)) +
      geom_line(color = "darkred", linewidth = 0.5) +
      facet_wrap(~ Pair, scales = "fixed", ncol = 2) +
      scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
      theme_minimal() +
      labs(title = paste("DCC Correlations:", group_name))
  } else {
    p <- NULL
  }
  
  return(list(data = df_group_results, plot = p))
})

# plot of conditional correlations
library(ggplot2)

# --- 1. Combined Plot for Lithium ---
# We use the extracted data and filter out any potential NAs 
plot_lithium_combined <- ggplot(dcc_lithium_cobalt_results$Lithium$data, 
                                aes(x = Date, y = Correlation, color = Pair)) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  # Adding a horizontal line at 0 for better reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.2)) +
  theme_minimal() +
  labs(
    title = "Dynamic Conditional Correlations: Lithium Group",
    subtitle = "Combined pairwise correlations (DCC-GARCH)",
    x = "Year",
    y = "Correlation",
    color = "Metal Pairs"
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    plot.title = element_text(face = "bold")
  )

# --- 2. Combined Plot for Cobalt ---
plot_cobalt_combined <- ggplot(dcc_lithium_cobalt_results$Cobalt$data, 
                               aes(x = Date, y = Correlation, color = Pair)) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.2)) +
  theme_minimal() +
  labs(
    title = "Dynamic Conditional Correlations: Cobalt Group",
    subtitle = "Combined pairwise correlations (DCC-GARCH)",
    x = "Year",
    y = "Correlation",
    color = "Metal Pairs"
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    plot.title = element_text(face = "bold")
  )

# Display plots
print(plot_lithium_combined)
print(plot_cobalt_combined)

# --- 3. Save for Publication ---
# High quality PNGs
ggsave("DCC_Lithium_Combined.png", plot = plot_lithium_combined, width = 10, height = 6, dpi = 300)
ggsave("DCC_Cobalt_Combined.png", plot = plot_cobalt_combined, width = 10, height = 6, dpi = 300)


##############################################################################
#---- OLD PART ----



# --- 5. Estimation Loop with Conditional Dummy Logic ---

# garch_results <- map(metal_names, function(m_name) {
  
  # 1. Check if the series actually has any bubbles
  current_dummy <- df_dummies_aligned[[m_name]]
  has_bubbles <- sum(current_dummy) > 0
  
  if (has_bubbles) {
    message("Estimating GARCH-X (Bubbles detected) for: ", m_name)
    v_reg <- as.matrix(current_dummy)
    
    current_spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = v_reg),
      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
      distribution.model = "std"
    )
  } else {
    message("Estimating Standard GARCH (No bubbles) for: ", m_name)
    current_spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
      distribution.model = "std"
    )
  }
  
  # 2. Fit Model with error handling for convergence
  fit <- tryCatch({
    ugarchfit(spec = current_spec, data = df_returns[[m_name]])
  }, error = function(e) {
    message("Estimation failed for ", m_name, ": ", e$message)
    return(NULL)
  })
  
  return(fit)
}) %>% #set_names(metal_names)

# --- 6. "Drill Down" Into a Specific Series ---

# Change "NIDALY" to whatever series name you want to inspect closely
# target_series <- "NIDALY" 
# 
# if (!is.null(garch_results[[target_series]])) {
#   cat("\n--- Detailed Inspection for:", target_series, "---\n")
#   
#   # Print the full coefficient table
#   print(round(as.data.frame(garch_results[[target_series]]@fit$matcoef), 4))
#   
#   # Check Convergence Status (0 means successful convergence)
#   conv_status <- garch_results[[target_series]]@fit$convergence
#   cat("\nConvergence Status:", ifelse(conv_status == 0, "Successful", "Failed"), "\n")
#   
#   # Optional: Plot the News Impact Curve or Volatility for this specific metal
#   # plot(garch_results[[target_series]], which = 2)
# }

# --- 1. Manual Configuration ---
# Change these values to manually adjust the model structure
# garch_type  <- "sGARCH"     # Options: "sGARCH", "eGARCH", "gjrGARCH"
# ar_order    <- 2            # AR order for mean equation
# ma_order    <- 0            # MA order for mean equation
# dist_type   <- "std"        # Options: "norm", "std" (Student-t), "ged"
# target_ser  <- "NIDALY"     # The specific series you want to inspect
# 
# # --- 2. Estimation Loop ---
# garch_results <- map(metal_names, function(m_name) {
#   
#   current_dummy <- df_dummies_aligned[[m_name]]
#   has_bubbles   <- sum(current_dummy) > 0
#   
#   # Define external regressor matrix
#   ext_reg <- if(has_bubbles) as.matrix(current_dummy) else NULL
#   
#   # Build specification with dual external regressors
#   spec <- ugarchspec(
#     variance.model = list(
#       model = garch_type, 
#       garchOrder = c(1, 1),
#       external.regressors = ext_reg # IMPACT ON VOLATILITY
#     ),
#     mean.model = list(
#       armaOrder = c(ar_order, ma_order), 
#       include.mean = TRUE,
#       external.regressors = ext_reg # IMPACT ON RETURNS
#     ),
#     distribution.model = dist_type
#   )
#   
#   # Estimation
#   fit <- tryCatch({
#     ugarchfit(spec = spec, data = df_returns[[m_name]], solver = "hybrid")
#   }, error = function(e) NULL)
#     
#   return(fit)
# }) %>% set_names(metal_names)
# 
# # --- 3. Detailed Inspection ---
# if (!is.null(garch_results[[target_ser]])) {
#   fit_obj <- garch_results[[target_ser]]
#   
#   cat("\n--- Detailed Results for:", target_ser, "---\n")
#   
#   # 1. Coefficients
#   # 'mxreg1' = Effect of bubble on the MEAN (Returns)
#   # 'vxreg1' = Effect of bubble on the VARIANCE (Volatility)
#   cat("\n[1] PARAMETER ESTIMATES:\n")
#   coefs <- as.data.frame(fit_obj@fit$matcoef)
#   print(round(coefs, 5))
#   
#   # 2. Information Criteria
#   cat("\n[2] MODEL FIT (Lower is better):\n")
#   print(infocriteria(fit_obj))
#   
#   # 3. Standardized Residual Diagnostics (Summary Output)
#   cat("\n[3] DIAGNOSTIC TESTS (Ljung-Box & ARCH-LM):\n")
#   # Printing the summary directly provides the full diagnostic suite
#   # When prompted for 'Selection', use 0 to exit if running interactively
#   show(fit_obj)
# }
# 
# #---- Single bench ----
# # --- 1. Manual Selection & Configuration ---
# target_ser  <- "NILMEX"     # Change this to your instrument of choice
# garch_type  <- "fiGARCH"     # "sGARCH", "eGARCH", or "gjrGARCH"
# dist_type   <- "sstd"        # Student-t is recommended for commodities
# 
# # --- 2. Prepare Data for the Single Series ---
# # Get returns for the target series
# y <- df_returns[[target_ser]]
# 
# # Create the Two Sub-categories of Bubbles
# # Expanding = Bubble is active AND return is positive (or price is rising)
# # Collapsing = Bubble is active AND return is negative (or price is falling)
# dummy_active <- df_dummies_aligned[[target_ser]]
# 
# d_expanding  <- as.integer(dummy_active == 1 & y >= 0)
# d_collapsing <- as.integer(dummy_active == 1 & y < 0)
# 
# # Combine into a matrix for the model
# #ext_reg_matrix <- cbind(Expanding = d_expanding, Collapsing = d_collapsing)
# ext_reg_matrix <-  cbind(Collapsing = d_collapsing)
# 
# # --- 3. GARCH-X Specification ---
# # We add both dummies to Mean and Variance equations
# spec <- ugarchspec(
#   variance.model = list(
#     model = garch_type, 
#     garchOrder = c(1, 1)#,
#     #external.regressors = ext_reg_matrix
#   ),
#   mean.model = list(
#     armaOrder = c(2, 0), 
#     include.mean = TRUE#,
#     #external.regressors = ext_reg_matrix
#   ),
#   distribution.model = dist_type
# )

# --- 4. Estimation ---
#fit <- ugarchfit(spec = spec, data = y)
fit <- ugarchfit(spec = spec, data = y, solver = "hybrid")

# --- 5. Comprehensive Results Output ---

if (fit@fit$convergence == 0) {
  cat("\n========================================================\n")
  cat("  GARCH-X RESULTS FOR:", target_ser, "\n")
  cat("  Bubble Directionality Analysis\n")
  cat("========================================================\n")
  
  # 5.1. Coefficients Table
  # mxreg1/vxreg1 = Expanding | mxreg2/vxreg2 = Collapsing
  coef_table <- as.data.frame(fit@fit$matcoef)
  rownames(coef_table) <- rownames(fit@fit$matcoef)
  
  cat("\n[1] COEFFICIENTS (Mean & Variance):\n")
  print(round(coef_table, 5))
  
  # 5.2. Standardized Residual Diagnostics
  cat("\n[2] RESIDUAL DIAGNOSTICS:\n")
  # This shows the full summary including Ljung-Box and ARCH-LM tests
  show(fit)
  
  # 5.3. Volatility Visualization
  # Plotting conditional volatility to see the spikes
  plot(fit, which = 3) 
  
} else {
  cat("\n!!! MODEL DID NOT CONVERGE !!!\n")
  cat("Try changing the GARCH type or simplifying the mean equation.\n")
}


#---- comparison of bubble detection methods ----
# --- 2. Load External Dummies ---
# Assuming your file has a 'Date' column and columns like 'NIDALY_up', 'NIDALY_down'
df_ext_dummies <- df_dummies_sign %>%
  slice(-1) # Align with returns (drop first row)

# --- 3. Prepare Data for Both Approaches ---
y <- df_returns[[target_ser]]

# Approach A: Return-Based (Statistical)
dummy_active <- df_dummies_aligned[[target_ser]]
d_exp_stat   <- as.integer(dummy_active == 1 & y >= 0)
d_col_stat   <- as.integer(dummy_active == 1 & y < 0)
reg_stat     <- cbind(Exp_Stat = d_exp_stat, Col_Stat = d_col_stat)

# Approach B: External File (Pre-defined - from GSADF approach)
# We dynamically pull the columns using the target name and your prefixes
d_up_ext   <- df_ext_dummies[[paste0(target_ser, "_up")]]
d_down_ext <- df_ext_dummies[[paste0(target_ser, "_down")]]
reg_ext    <- cbind(Up_Ext = d_up_ext, Down_Ext = d_down_ext)

# --- 4. GARCH-X Specification Function ---
# We create a function to avoid repeating the spec code
run_garch_x <- function(y_data, x_regs, label) {
  spec <- ugarchspec(
    variance.model = list(model = garch_type, garchOrder = c(1,1)),
    mean.model     = list(armaOrder = c(5,0), include.mean = TRUE, external.regressors = x_regs),
    distribution.model = dist_type
  )
  
  fit <- tryCatch({
    ugarchfit(spec = spec, data = y_data)
  }, error = function(e) return(NULL))
  
  return(fit)
}

# --- 5. Run and Compare ---
fit_stat <- run_garch_x(y, reg_stat, "Statistical")
fit_ext  <- run_garch_x(y, reg_ext, "External")

# --- 6. Results Comparison ---
cat("\n--- COMPARISON FOR:", target_ser, "---\n")

if (!is.null(fit_stat)) {
  cat("\n[APPROACH A] RETURN-BASED DUMMIES:\n")
  print(round(as.data.frame(fit_stat@fit$matcoef), 5))
}

if (!is.null(fit_ext)) {
  cat("\n[APPROACH B] EXTERNAL FILE DUMMIES:\n")
  print(round(as.data.frame(fit_ext@fit$matcoef), 5))
}
infocriteria(fit_stat)
infocriteria(fit_ext)
