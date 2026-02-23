# ==============================================================================
# SCRIPT: 03_LASSO.R
# PURPOSE: Linear Benchmark (Logistic Regression with L1 Penalty)
# ==============================================================================

library(tidyverse)
library(glmnet)   # The standard package for LASSO
library(xtable)

# ==============================================================================
# 1. DATA PREPARATION (Same as before)
# ==============================================================================
prepare_ml_data <- function(target_metal, target_col_name, df_master) {
  
  target_dummy <- paste0(target_col_name, "_BD")
  
  if(!target_dummy %in% names(df_master)) stop("Target not found")
  
  # A. Identify Predictors
  predictors_numeric <- df_master %>%
    select(where(is.numeric)) %>%
    select(-ends_with("BD")) %>%
    select(-any_of(c("Date", "date", "DATE"))) %>% 
    names()
  
  # Define the specific variables that are allowed to keep their levels
  # (VIX, 10Y Yield, Oil, Gas, and the 4 metals)
  vars_for_levels <- c("VIX", "YIELD_10Y", "OIL_WTI", "GAS_HH", 
                       "CODALY", "LIDALY", "NIDALY", "CUDALY")
  # Intersect ensures we only select columns that actually exist in the dataframe
  predictors_for_levels <- intersect(predictors_numeric, vars_for_levels)
  
  peer_dummies <- df_master %>%
    select(ends_with("BD")) %>%
    select(-all_of(target_dummy)) %>%
    names()
  
  # B. Helper Functions
  calc_safe_ret <- function(x) {
    x_num <- as.numeric(x)
    bad_idx <- which(x_num <= 0)
    if(length(bad_idx) > 0) x_num[bad_idx] <- 0.001 
    c(NA, diff(log(x_num)))
  }
  
  calc_safe_vol <- function(x) {
    x_num <- as.numeric(x)
    bad_idx <- which(x_num <= 0)
    if(length(bad_idx) > 0) x_num[bad_idx] <- 0.001
    TTR::runSD(c(NA, diff(log(x_num))), n = 10)
  }
  
  # C. Processing
  df_features <- df_master %>%
    arrange(Date) %>%
    mutate(
      # Calculate Returns and Volatility for ALL numeric predictors
      across(all_of(predictors_numeric), 
             list(Ret = calc_safe_ret, Vol = calc_safe_vol),
             .names = "{.col}_{.fn}")
    ) %>%
    # REMOVE VIX VOLATILITY: VIX is already a volatility index, 
    # so we drop the newly created VIX_Vol column to reduce noise
    select(-any_of("VIX_Vol"))
  
  # D. Lagging
  df_model <- df_features %>%
    mutate(
      Target_Bubble = .data[[target_dummy]], 
      across(ends_with("_Ret"), ~ lag(.), .names = "{.col}_Lag1"),
      across(ends_with("_Vol"), ~ lag(.), .names = "{.col}_Lag1"),
      across(all_of(predictors_for_levels), ~ lag(.), .names = "{.col}_Level_Lag1"),
      #across(all_of(predictors_numeric), ~ lag(.), .names = "{.col}_Level_Lag1"),
      across(all_of(peer_dummies), ~ replace_na(lag(.), 0), .names = "{.col}_Lag1")
    ) %>%
    select(Date, Target_Bubble, ends_with("Lag1"))
  
  # E. Cleanup
  df_final <- drop_na(df_model)
  return(df_final)
}

# ==============================================================================
# 2. LASSO PIPELINE
# ==============================================================================

# metal_configs <- list(
#   "Cobalt"  = "CODALY", 
#   "Lithium" = "LIDALY", 
#   "Nickel"  = "NIDALY", 
#   "Copper"  = "CUDALY"
# )
# 
# # Initialize the results list and performance table (Make sure AUC is here)
# lasso_results <- list()
# 
# force_single <- function(x, default = NA) {
#   if (is.null(x) || length(x) == 0) return(default)
#   return(x[1])
# }
# 
# perf_table_lasso <- data.frame(
#   Metal = character(),
#   Balanced_Accuracy = numeric(),
#   Sensitivity = numeric(),
#   Specificity = numeric(),
#   Precision = numeric(),
#   AUC = numeric(),         # <--- AUC Column
#   Bubbles_Detected = character(),
#   #Non_Zero_Coeffs = numeric(),
#   stringsAsFactors = FALSE
# )
# ## pipeline
# for (m_name in names(metal_configs)) {
#   
#   col_code <- metal_configs[[m_name]]
#   
#   # 1. Prepare Data
#   df_ml <- tryCatch({
#     prepare_ml_data(m_name, col_code, df_master)
#   }, error = function(e) {
#     cat("Error in data preparation for", m_name, ":", conditionMessage(e), "\n")
#     return(NULL)
#   })
#   
#   if (!is.null(df_ml) && sum(df_ml$Target_Bubble) >= 10) {
#     
#     # 2. Convert to Matrix (Required for glmnet)
#     X <- as.matrix(select(df_ml, -Date, -Target_Bubble))
#     y <- as.numeric(df_ml$Target_Bubble)
#     
#     # 3. Train LASSO with Cross-Validation (cv.glmnet)
#     # alpha = 1 means LASSO (0 would be Ridge)
#     # family = "binomial" means Logistic Regression (0/1)
#     # type.measure = "class" optimizes for classification error
#     
#     cv_lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial", type.measure = "class")
#     
#     # 4. Get Predictions using the Best Lambda (lambda.min or lambda.1se)
#     # lambda.1se is more conservative (simpler model), lambda.min is more accurate.
#     # We use lambda.min to be competitive.
#     probs <- predict(cv_lasso, newx = X, s = "lambda.min", type = "response")
#     
#     probs_vec <- as.numeric(probs)
#     
#     auc_val <- NA
#     try({
#       roc_obj <- pROC::roc(y, probs_vec, quiet = TRUE)
#       auc_val <- as.numeric(pROC::auc(roc_obj))
#     }, silent = TRUE)
#     
#     # 5. Calculate Metrics
#     pred_class <- ifelse(probs > 0.5, 1, 0)
#     
#     TP <- sum(pred_class == 1 & y == 1)
#     TN <- sum(pred_class == 0 & y == 0)
#     FP <- sum(pred_class == 1 & y == 0)
#     FN <- sum(pred_class == 0 & y == 1)
#     
#     sens <- TP / (TP + FN)
#     spec <- TN / (TN + FP)
#     bal_acc <- (sens + spec) / 2
#     prec <- TP / (TP + FP)
#     if(is.nan(prec)) prec <- 0
#     
#     bubbles_str <- paste0(TP, "/", (TP + FN))
#     
#     # Check how many variables were selected (Coefficients != 0)
#     coefs <- coef(cv_lasso, s = "lambda.min")
#     n_vars <- sum(coefs != 0) - 1 # Subtract Intercept
#     
#     # 6. Save Results
#     lasso_results[[m_name]] <- list(model = cv_lasso, coefs = coefs)
#     
#     perf_table_lasso <- rbind(perf_table_lasso, data.frame(
#       Metal = force_single(m_name), 
#       Balanced_Accuracy = force_single(bal_acc, 0), 
#       Sensitivity = force_single(sens, 0),
#       Specificity = force_single(spec, 0), 
#       Precision = force_single(prec, 0), 
#       AUC = force_single(auc_val, NA),
#       Bubbles_Detected = force_single(bubbles_str, "0/0") 
#       #Non_Zero_Coeffs = force_single(n_vars, 0)
#     ))
#     
#     cat(paste("Finished LASSO for:", m_name, "| Variables kept:", n_vars, "\n"))
#   }
# }
# saveRDS(lasso_results, "R/lasso_results_complete.rds")
# # Print the final dataframe to console
# print(perf_table_lasso)
# # ==============================================================================
# # 3. OUTPUT TABLE
# # ==============================================================================
# 
# # Rename for LaTeX
# colnames(perf_table_lasso) <- c("Metal", "Bal. Accuracy", "Sensitivity", "Specificity", "Precision", "AUC", "Bubbles")
# 
# latex_obj <- xtable(perf_table_lasso, 
#                     caption = "LASSO (Linear Benchmark) Performance Metrics", 
#                     label = "tab:lasso_performance",
#                     digits = 3, 
#                     align = "lccccccc") # Added one column for Vars Kept
# 
# 
# 
# print(latex_obj, 
#       include.rownames = FALSE, 
#       booktabs = TRUE,      # Makes the table look professional (requires \usepackage{booktabs} in LaTeX)
#       comment = FALSE)  
# 
# #print(latex_obj, include.rownames = FALSE, booktabs = TRUE, comment = FALSE, file = "Table_Results_LASSO.tex")
# 
# # Storage for results
# lasso_results <- list()
# perf_table_lasso <- data.frame(
#   Metal = character(),
#   Balanced_Accuracy = numeric(),
#   Sensitivity = numeric(),
#   Specificity = numeric(),
#   Precision = numeric(),
#   AUC = numeric(),         # <--- NEW AUC COLUMN
#   Bubbles_Detected = character(),
#   Non_Zero_Coeffs = integer(),
#   stringsAsFactors = FALSE
# )
# 
# cat("\nStarting LASSO Benchmark...\n")
# 
# 
# 
# 
# for (m_name in names(lasso_results)) {
#   
#   res <- lasso_results[[m_name]]
#   
#   if (!is.null(res)) {
#     cv_lasso <- res$model
#     X <- res$X
#     y <- res$y
#     
#     # 1. Get Probabilities from LASSO
#     # type = "response" gives us probabilities between 0 and 1
#     probs_matrix <- predict(cv_lasso, newx = X, s = "lambda.min", type = "response")
#     
#     # TINY DIFFERENCE: Convert the matrix to a simple vector for the ROC function
#     probs_vec <- as.numeric(probs_matrix) 
#     
#     # 2. Calculate AUC
#     roc_obj <- roc(y, probs_vec, quiet = TRUE)
#     auc_val <- as.numeric(auc(roc_obj))
#     
#     # 3. Standard Classification (Strict 0.5 Threshold)
#     pred_class <- ifelse(probs_vec > 0.5, 1, 0)
#     
#     # 4. Standard Metrics Calculation
#     TP <- sum(pred_class == 1 & y == 1)
#     TN <- sum(pred_class == 0 & y == 0)
#     FP <- sum(pred_class == 1 & y == 0)
#     FN <- sum(pred_class == 0 & y == 1)
#     
#     sens <- TP / (TP + FN)
#     spec <- TN / (TN + FP)
#     bal_acc <- (sens + spec) / 2
#     prec <- TP / (TP + FP)
#     if(is.nan(prec)) prec <- 0
#     
#     bubbles_str <- paste0(TP, "/", (TP + FN))
#     
#     # Extract number of non-zero coefficients (LASSO's feature selection)
#     coef_vals <- coef(cv_lasso, s = "lambda.min")
#     n_vars <- sum(coef_vals[-1] != 0) 
#     
#     # 5. Add to Table
#     perf_table_lasso <- rbind(perf_table_lasso, data.frame(
#       Metal = m_name,
#       Balanced_Accuracy = bal_acc,
#       Sensitivity = sens,
#       Specificity = spec,
#       Precision = prec,
#       AUC = auc_val,          # <--- AUC added here
#       Bubbles_Detected = bubbles_str,
#       Non_Zero_Coeffs = n_vars
#     ))
#   }
# }
# 
# print(perf_table_lasso)
# 
# # ==============================================================================
# # GENERATE LATEX TABLE FOR LASSO
# # ==============================================================================
# 
# # 8 Columns of Data = 9 Alignment Characters
# colnames(perf_table_lasso) <- c("Metal", "Bal. Accuracy", "Sensitivity", "Specificity", "Precision", "AUC", "Bubbles", "Features Used")
# 
# latex_obj_lasso <- xtable(perf_table_lasso, 
#                           caption = "LASSO Model Performance Metrics", 
#                           label = "tab:lasso_performance",
#                           digits = 3, 
#                           align = "llccccccc") # 9 characters: 1 index + 8 columns
# 
# print(latex_obj_lasso, 
#       include.rownames = FALSE, 
#       booktabs = TRUE, 
#       comment = FALSE,
#       file = "Table_Results_LASSO.tex")
# print(latex_obj_lasso)


#---- corrected version----

metal_configs <- list(
  "Cobalt"  = "CODALY", 
  "Lithium" = "LIDALY", 
  "Nickel"  = "NIDALY", 
  "Copper"  = "CUDALY"
)

# 1. Safety function (prevents '1, 0 rows' error if a metric is empty)
force_single <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) return(default)
  return(x[1])
}

# 2. Initialize an empty results table
perf_table_lasso <- data.frame(
  Metal = character(), Balanced_Accuracy = numeric(), Sensitivity = numeric(),
  Specificity = numeric(), Precision = numeric(), AUC = numeric(),
  Bubbles_Detected = character(), Non_Zero_Coeffs = numeric(),
  stringsAsFactors = FALSE
)

lasso_results <- list()

cat("\nStarting Fair (Out-of-Sample) LASSO Benchmark...\n")

for (m_name in names(metal_configs)) {
  
  col_code <- metal_configs[[m_name]]
  
  # Prepare data
  df_ml <- tryCatch({
    prepare_ml_data(m_name, col_code, df_master)
  }, error = function(e) {
    cat("Error in data preparation for", m_name, ":", conditionMessage(e), "\n")
    return(NULL)
  })
  
  # Proceed only if we have data AND at least 10 bubbles to train on
  if (!is.null(df_ml) && sum(df_ml$Target_Bubble == 1, na.rm = TRUE) >= 10) {
    
    # Safe conversion to matrix (X) and numeric vector (y)
    X <- as.matrix(select(df_ml, -Date, -Target_Bubble))
    y <- as.numeric(as.character(df_ml$Target_Bubble)) 
    
    # Train LASSO with 'keep = TRUE' to retain fair Out-Of-Fold predictions
    cv_lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial", 
                          type.measure = "class", standardize = TRUE, 
                          maxit = 1000000, keep = TRUE)
    
    # EXTRACT FAIR PREDICTIONS (Out-Of-Sample)
    # Find which column index corresponds to the optimal lambda (lambda.min)
    idx_min <- which(cv_lasso$lambda == cv_lasso$lambda.min)
    
    # fit.preval returns log-odds. We must convert them to probabilities (0 to 1)
    log_odds <- cv_lasso$fit.preval[, idx_min]
    probs_vec <- exp(log_odds) / (1 + exp(log_odds))
    
    # Calculate AUC safely
    auc_val <- NA
    try({
      roc_obj <- pROC::roc(y, probs_vec, quiet = TRUE)
      auc_val <- as.numeric(pROC::auc(roc_obj))
    }, silent = TRUE)
    
    # Standard Classification (0.5 Threshold)
    pred_class <- ifelse(probs_vec > 0.5, 1, 0)
    
    TP <- sum(pred_class == 1 & y == 1, na.rm = TRUE)
    TN <- sum(pred_class == 0 & y == 0, na.rm = TRUE)
    FP <- sum(pred_class == 1 & y == 0, na.rm = TRUE)
    FN <- sum(pred_class == 0 & y == 1, na.rm = TRUE)
    
    # Safe division to prevent NaN errors
    sens <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
    spec <- ifelse((TN + FP) == 0, 0, TN / (TN + FP))
    bal_acc <- (sens + spec) / 2
    prec <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
    
    bubbles_str <- paste0(TP, "/", (TP + FN))
    
    # Check how many variables LASSO selected (excluding the intercept)
    coefs <- coef(cv_lasso, s = "lambda.min")
    n_vars <- sum(coefs != 0) - 1
    
    # Save the model and coefficients to our list
    lasso_results[[m_name]] <- list(model = cv_lasso, coefs = coefs)
    
    # Append the metrics to the results table safely
    perf_table_lasso <- rbind(perf_table_lasso, data.frame(
      Metal = force_single(m_name), 
      Balanced_Accuracy = force_single(bal_acc, 0), 
      Sensitivity = force_single(sens, 0),
      Specificity = force_single(spec, 0), 
      Precision = force_single(prec, 0), 
      AUC = force_single(auc_val, NA),
      Bubbles_Detected = force_single(bubbles_str, "0/0"), 
      Non_Zero_Coeffs = force_single(n_vars, 0)
    ))
    
    cat(paste("Finished LASSO for:", m_name, "| Variables kept:", force_single(n_vars, 0), "| AUC:", round(force_single(auc_val, NA), 3), "\n"))
  }
}

# Save results to disk
if(!dir.exists("R")) dir.create("R")
saveRDS(lasso_results, "R/lasso_results_complete.rds")

# Display the true, cross-validated table
print(perf_table_lasso)

# ==============================================================================
# 3. OUTPUT TABLE (LaTeX)
# ==============================================================================

colnames(perf_table_lasso) <- c("Metal", "Bal. Accuracy", "Sensitivity", "Specificity", "Precision", "AUC", "Bubbles", "Vars Kept")

latex_obj <- xtable(perf_table_lasso, 
                    caption = "LASSO (Linear Benchmark) Performance Metrics", 
                    label = "tab:lasso_performance",
                    digits = 3, 
                    align = "llccccccc") # 9 alignment characters (1 for row names + 8 columns)

# print(latex_obj, 
#       include.rownames = FALSE, 
#       booktabs = TRUE, 
#       comment = FALSE,
#       file = "Table_Results_LASSO.tex")
print(latex_obj)
cat("\nDone! Check the true, cross-validated LASSO results.\n")

#---- third code with coeffcients----
library(pROC)
library(glmnet)
library(dplyr)
library(ggplot2) # Required for plotting
library(xtable)

metal_configs <- list(
  "Cobalt"  = "CODALY", 
  "Lithium" = "LIDALY", 
  "Nickel"  = "NIDALY", 
  "Copper"  = "CUDALY"
)

# Create a folder to save LASSO importance plots
if(!dir.exists("plots_lasso")) dir.create("plots_lasso")

# 1. Safety function (prevents '1, 0 rows' error if a metric is empty)
force_single <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) return(default)
  return(x[1])
}

# 2. Initialize an empty results table
perf_table_lasso <- data.frame(
  Metal = character(), Balanced_Accuracy = numeric(), Sensitivity = numeric(),
  Specificity = numeric(), Precision = numeric(), AUC = numeric(),
  Bubbles_Detected = character(), Non_Zero_Coeffs = numeric(),
  stringsAsFactors = FALSE
)

lasso_results <- list()

cat("\nStarting Fair (Out-of-Sample) LASSO Benchmark...\n")

for (m_name in names(metal_configs)) {
  
  col_code <- metal_configs[[m_name]]
  
  # Prepare data
  df_ml <- tryCatch({
    prepare_ml_data(m_name, col_code, df_master)
  }, error = function(e) {
    cat("Error in data preparation for", m_name, ":", conditionMessage(e), "\n")
    return(NULL)
  })
  
  # Proceed only if we have data AND at least 10 bubbles to train on
  if (!is.null(df_ml) && sum(df_ml$Target_Bubble == 1, na.rm = TRUE) >= 10) {
    
    # Safe conversion to matrix (X) and numeric vector (y)
    X <- as.matrix(select(df_ml, -Date, -Target_Bubble))
    y <- as.numeric(as.character(df_ml$Target_Bubble)) 
    
    set.seed(123)
    # Train LASSO with 'keep = TRUE' to retain fair Out-Of-Fold predictions
    cv_lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial", 
                          type.measure = "class", standardize = TRUE, 
                          maxit = 1000000, keep = TRUE)
    
    # EXTRACT FAIR PREDICTIONS (Out-Of-Sample)
    idx_min <- which(cv_lasso$lambda == cv_lasso$lambda.min)
    log_odds <- cv_lasso$fit.preval[, idx_min]
    probs_vec <- exp(log_odds) / (1 + exp(log_odds))
    
    # Calculate AUC safely
    auc_val <- NA
    try({
      roc_obj <- pROC::roc(y, probs_vec, quiet = TRUE)
      auc_val <- as.numeric(pROC::auc(roc_obj))
    }, silent = TRUE)
    
    # Standard Classification (0.5 Threshold)
    pred_class <- ifelse(probs_vec > 0.5, 1, 0)
    
    TP <- sum(pred_class == 1 & y == 1, na.rm = TRUE)
    TN <- sum(pred_class == 0 & y == 0, na.rm = TRUE)
    FP <- sum(pred_class == 1 & y == 0, na.rm = TRUE)
    FN <- sum(pred_class == 0 & y == 1, na.rm = TRUE)
    
    # Safe division to prevent NaN errors
    sens <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
    spec <- ifelse((TN + FP) == 0, 0, TN / (TN + FP))
    bal_acc <- (sens + spec) / 2
    prec <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
    
    bubbles_str <- paste0(TP, "/", (TP + FN))
    
    # Extract coefficients
    coefs <- coef(cv_lasso, s = "lambda.min")
    n_vars <- sum(coefs != 0) - 1
    
    # ==========================================================================
    # GENERATE AND SAVE FEATURE IMPORTANCE PLOT (Coefficients)
    # ==========================================================================
    
    # Convert sparse matrix of coefficients to a standard dataframe
    coef_df <- as.data.frame(as.matrix(coefs))
    coef_df$Feature <- rownames(coef_df)
    colnames(coef_df)[1] <- "Coefficient"
    
    # Filter out Intercept and zero coefficients, select top 20 by magnitude
    coef_df <- coef_df %>%
      filter(Feature != "(Intercept)", Coefficient != 0) %>%
      mutate(
        Feature = gsub("_Lag1", "", Feature),      # removes "_Lag1"
        Feature = gsub("_Level", "", Feature)      # removes "_Level" 
      ) %>%
      arrange(desc(abs(Coefficient))) %>%
      head(15)
    
    if(nrow(coef_df) > 0) {
      p_imp <- ggplot(coef_df, aes(x = reorder(Feature, abs(Coefficient)), y = Coefficient, fill = Coefficient > 0)) +
        geom_col() +
        coord_flip() +
        scale_fill_manual(values = c("TRUE" = "orange", "FALSE" = "purple3"), guide = "none") +
        labs(
          title = paste("LASSO Feature Importance -", m_name),
          subtitle = "Top Non-Zero Coefficients",
          x = "Features", 
          y = "Coefficient Value (Impact on Bubble Probability)"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14))
      
      # Save the plot
      ggsave(filename = paste0("plots_lasso/Importance_", m_name, ".png"), 
             plot = p_imp, width = 10, height = 8, bg = "white", dpi = 300)
    }
    # ==========================================================================
    
    # Save the model and coefficients to our list
    lasso_results[[m_name]] <- list(model = cv_lasso, coefs = coefs)
    
    # Append the metrics to the results table safely
    perf_table_lasso <- rbind(perf_table_lasso, data.frame(
      Metal = force_single(m_name), 
      Balanced_Accuracy = force_single(bal_acc, 0), 
      Sensitivity = force_single(sens, 0),
      Specificity = force_single(spec, 0), 
      Precision = force_single(prec, 0), 
      AUC = force_single(auc_val, NA),
      Bubbles_Detected = force_single(bubbles_str, "0/0"), 
      Non_Zero_Coeffs = force_single(n_vars, 0)
    ))
    
    cat(paste("Finished LASSO for:", m_name, "| Variables kept:", force_single(n_vars, 0), "| AUC:", round(force_single(auc_val, NA), 3), "\n"))
  }
}

# Save results to disk
if(!dir.exists("R")) dir.create("R")
saveRDS(lasso_results, "R/lasso_results_complete.rds")

# Display the true, cross-validated table
print(perf_table_lasso)

colnames(perf_table_lasso) <- c("Metal", "Bal. Accuracy", "Sensitivity", "Specificity", "Precision", "AUC", "Bubbles", "Vars Kept")

latex_obj <- xtable(perf_table_lasso, 
                    caption = "LASSO (Linear Benchmark) Performance Metrics", 
                    label = "tab:lasso_performance",
                    digits = 3, 
                    align = "llccccccc") # 9 alignment characters (1 for row names + 8 columns)

# print(latex_obj, 
#       include.rownames = FALSE, 
#       booktabs = TRUE, 
#       comment = FALSE,
#       file = "Table_Results_LASSO.tex")
print(latex_obj)
cat("\nDone! Check the true, cross-validated LASSO results.\n")