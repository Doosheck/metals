# --- 1. Load Libraries ---
library(tidyverse)
library(rugarch)
library(here)

# --- 2. Load Processed Data ---
df_prices  <- read_csv(here("data", "combined_metals_cleaned.csv"), show_col_types = FALSE)
df_dummies <- read_csv(here("data", "bubble_dummies.csv"), show_col_types = FALSE)
df_dummies_sign <- read_csv(here("data", "bubble_dummies_separate.csv"), show_col_types = FALSE)

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
garch_results <- map(metal_names, function(m_name) {
  
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

# --- 3. Diagnostic Summary Table ---
diagnostic_summary <- map_df(names(garch_results), function(name) {
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
print(diagnostic_summary)

# --- 4. Export for DCC GARCH ---
df_std_residuals <- as_tibble(std_resid_list)
write_csv(df_std_residuals, here("data", "std_residuals_for_dcc.csv"))



# --- 5. Estimation Loop with Conditional Dummy Logic ---

garch_results <- map(metal_names, function(m_name) {
  
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
}) %>% set_names(metal_names)

# --- 6. "Drill Down" Into a Specific Series ---

# Change "NIDALY" to whatever series name you want to inspect closely
target_series <- "NIDALY" 

if (!is.null(garch_results[[target_series]])) {
  cat("\n--- Detailed Inspection for:", target_series, "---\n")
  
  # Print the full coefficient table
  print(round(as.data.frame(garch_results[[target_series]]@fit$matcoef), 4))
  
  # Check Convergence Status (0 means successful convergence)
  conv_status <- garch_results[[target_series]]@fit$convergence
  cat("\nConvergence Status:", ifelse(conv_status == 0, "Successful", "Failed"), "\n")
  
  # Optional: Plot the News Impact Curve or Volatility for this specific metal
  # plot(garch_results[[target_series]], which = 2)
}

# --- 1. Manual Configuration ---
# Change these values to manually adjust the model structure
garch_type  <- "sGARCH"     # Options: "sGARCH", "eGARCH", "gjrGARCH"
ar_order    <- 2            # AR order for mean equation
ma_order    <- 0            # MA order for mean equation
dist_type   <- "std"        # Options: "norm", "std" (Student-t), "ged"
target_ser  <- "NIDALY"     # The specific series you want to inspect

# --- 2. Estimation Loop ---
garch_results <- map(metal_names, function(m_name) {
  
  current_dummy <- df_dummies_aligned[[m_name]]
  has_bubbles   <- sum(current_dummy) > 0
  
  # Define external regressor matrix
  ext_reg <- if(has_bubbles) as.matrix(current_dummy) else NULL
  
  # Build specification with dual external regressors
  spec <- ugarchspec(
    variance.model = list(
      model = garch_type, 
      garchOrder = c(1, 1),
      external.regressors = ext_reg # IMPACT ON VOLATILITY
    ),
    mean.model = list(
      armaOrder = c(ar_order, ma_order), 
      include.mean = TRUE,
      external.regressors = ext_reg # IMPACT ON RETURNS
    ),
    distribution.model = dist_type
  )
  
  # Estimation
  fit <- tryCatch({
    ugarchfit(spec = spec, data = df_returns[[m_name]], solver = "hybrid")
  }, error = function(e) NULL)
    
  return(fit)
}) %>% set_names(metal_names)

# --- 3. Detailed Inspection ---
if (!is.null(garch_results[[target_ser]])) {
  fit_obj <- garch_results[[target_ser]]
  
  cat("\n--- Detailed Results for:", target_ser, "---\n")
  
  # 1. Coefficients
  # 'mxreg1' = Effect of bubble on the MEAN (Returns)
  # 'vxreg1' = Effect of bubble on the VARIANCE (Volatility)
  cat("\n[1] PARAMETER ESTIMATES:\n")
  coefs <- as.data.frame(fit_obj@fit$matcoef)
  print(round(coefs, 5))
  
  # 2. Information Criteria
  cat("\n[2] MODEL FIT (Lower is better):\n")
  print(infocriteria(fit_obj))
  
  # 3. Standardized Residual Diagnostics (Summary Output)
  cat("\n[3] DIAGNOSTIC TESTS (Ljung-Box & ARCH-LM):\n")
  # Printing the summary directly provides the full diagnostic suite
  # When prompted for 'Selection', use 0 to exit if running interactively
  show(fit_obj)
}

#---- Single bench ----
# --- 1. Manual Selection & Configuration ---
target_ser  <- "NILMEX"     # Change this to your instrument of choice
garch_type  <- "fiGARCH"     # "sGARCH", "eGARCH", or "gjrGARCH"
dist_type   <- "sstd"        # Student-t is recommended for commodities

# --- 2. Prepare Data for the Single Series ---
# Get returns for the target series
y <- df_returns[[target_ser]]

# Create the Two Sub-categories of Bubbles
# Expanding = Bubble is active AND return is positive (or price is rising)
# Collapsing = Bubble is active AND return is negative (or price is falling)
dummy_active <- df_dummies_aligned[[target_ser]]

d_expanding  <- as.integer(dummy_active == 1 & y >= 0)
d_collapsing <- as.integer(dummy_active == 1 & y < 0)

# Combine into a matrix for the model
#ext_reg_matrix <- cbind(Expanding = d_expanding, Collapsing = d_collapsing)
ext_reg_matrix <-  cbind(Collapsing = d_collapsing)

# --- 3. GARCH-X Specification ---
# We add both dummies to Mean and Variance equations
spec <- ugarchspec(
  variance.model = list(
    model = garch_type, 
    garchOrder = c(1, 1)#,
    #external.regressors = ext_reg_matrix
  ),
  mean.model = list(
    armaOrder = c(2, 0), 
    include.mean = TRUE#,
    #external.regressors = ext_reg_matrix
  ),
  distribution.model = dist_type
)

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
