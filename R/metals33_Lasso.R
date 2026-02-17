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
  
  target_dummy <- paste0(target_col_name, "_dummy")
  
  if(!target_dummy %in% names(df_master)) stop("Target not found")
  
  # A. Identify Predictors
  predictors_numeric <- df_master %>%
    select(where(is.numeric)) %>%
    select(-ends_with("dummy")) %>%
    select(-any_of(c("Date", "date", "DATE"))) %>% 
    names()
  
  peer_dummies <- df_master %>%
    select(ends_with("dummy")) %>%
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
      across(all_of(predictors_numeric), 
             list(Ret = calc_safe_ret, Vol = calc_safe_vol),
             .names = "{.col}_{.fn}")
    )
  
  # D. Lagging
  df_model <- df_features %>%
    mutate(
      Target_Bubble = .data[[target_dummy]], 
      across(ends_with("_Ret"), ~ lag(.), .names = "{.col}_Lag1"),
      across(ends_with("_Vol"), ~ lag(.), .names = "{.col}_Lag1"),
      across(all_of(predictors_numeric), ~ lag(.), .names = "{.col}_Level_Lag1"),
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

metal_configs <- list(
  "Cobalt"  = "CODALY", 
  "Lithium" = "LIDALY", 
  "Nickel"  = "NIDALY", 
  "Copper"  = "CUDALY"
)

# Storage for results
lasso_results <- list()
perf_table_lasso <- data.frame(
  Metal = character(),
  Balanced_Accuracy = numeric(),
  Sensitivity = numeric(), 
  Specificity = numeric(), 
  Precision = numeric(),
  Bubbles_Detected = character(),
  Non_Zero_Coeffs = integer(), # New metric: How many variables did LASSO keep?
  stringsAsFactors = FALSE
)

cat("\nStarting LASSO Benchmark...\n")

for (m_name in names(metal_configs)) {
  
  col_code <- metal_configs[[m_name]]
  
  # 1. Prepare Data
  df_ml <- tryCatch({
    prepare_ml_data(m_name, col_code, df_master)
  }, error = function(e) return(NULL))
  
  if (!is.null(df_ml) && sum(df_ml$Target_Bubble) >= 10) {
    
    # 2. Convert to Matrix (Required for glmnet)
    X <- as.matrix(select(df_ml, -Date, -Target_Bubble))
    y <- as.numeric(df_ml$Target_Bubble)
    
    # 3. Train LASSO with Cross-Validation (cv.glmnet)
    # alpha = 1 means LASSO (0 would be Ridge)
    # family = "binomial" means Logistic Regression (0/1)
    # type.measure = "class" optimizes for classification error
    
    cv_lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial", type.measure = "class")
    
    # 4. Get Predictions using the Best Lambda (lambda.min or lambda.1se)
    # lambda.1se is more conservative (simpler model), lambda.min is more accurate.
    # We use lambda.min to be competitive.
    probs <- predict(cv_lasso, newx = X, s = "lambda.min", type = "response")
    
    # 5. Calculate Metrics
    pred_class <- ifelse(probs > 0.5, 1, 0)
    
    TP <- sum(pred_class == 1 & y == 1)
    TN <- sum(pred_class == 0 & y == 0)
    FP <- sum(pred_class == 1 & y == 0)
    FN <- sum(pred_class == 0 & y == 1)
    
    sens <- TP / (TP + FN)
    spec <- TN / (TN + FP)
    bal_acc <- (sens + spec) / 2
    prec <- TP / (TP + FP)
    if(is.nan(prec)) prec <- 0
    
    bubbles_str <- paste0(TP, "/", (TP + FN))
    
    # Check how many variables were selected (Coefficients != 0)
    coefs <- coef(cv_lasso, s = "lambda.min")
    n_vars <- sum(coefs != 0) - 1 # Subtract Intercept
    
    # 6. Save Results
    lasso_results[[m_name]] <- list(model = cv_lasso, coefs = coefs)
    
    perf_table_lasso <- rbind(perf_table_lasso, data.frame(
      Metal = m_name,
      Balanced_Accuracy = bal_acc,
      Sensitivity = sens,
      Specificity = spec,
      Precision = prec,
      Bubbles_Detected = bubbles_str,
      Non_Zero_Coeffs = n_vars
    ))
    
    cat(paste("Finished LASSO for:", m_name, "| Variables kept:", n_vars, "\n"))
  }
}
saveRDS(lasso_results, "R/lasso_results_complete.rds")
# ==============================================================================
# 3. OUTPUT TABLE
# ==============================================================================

# Rename for LaTeX
colnames(perf_table_lasso) <- c("Metal", "Bal. Accuracy", "Sensitivity", "Specificity", "Precision", "Bubbles (Found/Total)", "Vars Kept")

latex_obj <- xtable(perf_table_lasso, 
                    caption = "LASSO (Linear Benchmark) Performance Metrics", 
                    label = "tab:lasso_performance",
                    digits = 3, 
                    align = "lccccccc") # Added one column for Vars Kept

print(perf_table_lasso)

print(latex_obj, 
      include.rownames = FALSE, 
      booktabs = TRUE,      # Makes the table look professional (requires \usepackage{booktabs} in LaTeX)
      comment = FALSE)  

#print(latex_obj, include.rownames = FALSE, booktabs = TRUE, comment = FALSE, file = "Table_Results_LASSO.tex")