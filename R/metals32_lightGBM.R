# ==============================================================================
# SCRIPT: 02_LightGBM.R
# PURPOSE: Benchmark Model (Gradient Boosting)
# ==============================================================================

library(tidyverse)
library(lightgbm) # install.packages("lightgbm")
library(shapviz)
library(xtable)
library(pROC)
# ==============================================================================
# 1. DATA PREPARATION (Identical to Random Forest for fair comparison)
# ==============================================================================
prepare_ml_data <- function(target_metal, target_col_name, df_master) {
  
  target_dummy <- paste0(target_col_name, "_BD")
  
  if(!target_dummy %in% names(df_master)) {
    stop(paste("Error: Column", target_dummy, "not found in df_master"))
  }
  
  # A. Identify Predictors
  predictors_numeric <- df_master %>%
    select(where(is.numeric)) %>%
    select(-ends_with("BD")) %>%
    select(-any_of(c("Date", "date", "DATE"))) %>% 
    names()
  
  peer_dummies <- df_master %>%
    select(ends_with("BD")) %>%
    select(-all_of(target_dummy)) %>%
    names()
  
  # B. Helper Functions (Fix Negative Values)
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
# 2. LIGHTGBM PIPELINE FUNCTION
# ==============================================================================
run_lgbm_pipeline <- function(metal_name, col_name, df_data) {
  
  cat(paste("\n=== Processing (LightGBM):", metal_name, "===\n"))
  
  # A. Prepare Data
  df_ml <- tryCatch({
    prepare_ml_data(metal_name, col_name, df_data)
  }, error = function(e) {
    message(paste("Error preparing data:", e$message))
    return(NULL)
  })
  
  if (is.null(df_ml)) return(NULL)
  
  n_bubbles <- sum(df_ml$Target_Bubble)
  
  if (n_bubbles < 10) {
    message("Skipping - Not enough bubbles (<10).")
    return(NULL)
  }
  
  # B. Prepare LightGBM Dataset
  # LightGBM requires numeric Matrix, not DataFrame
  X_matrix <- as.matrix(select(df_ml, -Date, -Target_Bubble))
  y_vector <- as.numeric(df_ml$Target_Bubble)
  
  dtrain <- lgb.Dataset(data = X_matrix, label = y_vector)
  
  # C. Train Model
  # is_unbalance = TRUE is the secret weapon for bubbles!
  params <- list(
    objective = "binary",
    metric = "auc",             
    learning_rate = 0.05,       
    num_leaves = 31,            
    is_unbalance = TRUE,        
    feature_fraction = 0.8,     
    bagging_fraction = 0.8,     
    bagging_freq = 5,
    verbosity = -1
  )
  
  fit_lgbm <- lgb.train(
    params = params,
    data = dtrain,
    nrounds = 200,              
    verbose = 0
  )
  
  # D. SHAP Calculation (Native & Fast)
  shap_values <- shapviz(fit_lgbm, X_pred = X_matrix)
  
  # E. Plot (Using clean labels)
  clean_labels_short <- function(x) {
    x <- gsub("_Lag1", "", x)
    x <- gsub("_Level", "", x)
    x <- gsub("_dummy", " Bubble", x)
    x <- gsub("_Vol", " Vol", x)
    x <- gsub("Volatility", " Vol", x)
    x <- gsub("_Ret", " Ret", x)
    x <- gsub("Returns", " Ret", x)
    return(x)
  }
  
  plot_obj <- sv_importance(shap_values, kind = "beeswarm") +
    theme_minimal() +
    labs(title = NULL) + 
    scale_y_discrete(labels = clean_labels_short) +
    theme(
      axis.text.y  = element_text(size = 14, color = "black"), 
      axis.text.x  = element_text(size = 12, color = "black"), 
      axis.title.x = element_text(size = 14, face = "bold"),   
      legend.text  = element_text(size = 10),
      legend.title = element_text(size = 12)
    )
  
  # Return everything (saving X_matrix for later predictions)
  return(list(model = fit_lgbm, plot = plot_obj, shap_obj = shap_values, X = X_matrix, y = y_vector))
}

# ==============================================================================
# 3. EXECUTION LOOP
# ==============================================================================

metal_configs <- list(
  "Cobalt"  = "CODALY", 
  "Lithium" = "LIDALY", 
  "Nickel"  = "NIDALY", 
  "Copper"  = "CUDALY"
)

# New folders for LGBM results to keep things organized
if(!dir.exists("results_lgbm")) dir.create("results_lgbm")
if(!dir.exists("plots_lgbm")) dir.create("plots_lgbm")

lgbm_results <- list()

It looks like you just need the initialization of the perf_table_lgbm dataframe added right before the second loop, along with the LaTeX table generation at the end so you get your final output.

Here is the complete, seamlessly joined code block. You can copy and paste this entire section directly into your script.

R
library(pROC)
library(xtable)
library(dplyr)

# ==============================================================================
# 3. EXECUTION LOOP (Training LightGBM models)
# ==============================================================================

metal_configs <- list(
  "Cobalt"  = "CODALY", 
  "Lithium" = "LIDALY", 
  "Nickel"  = "NIDALY", 
  "Copper"  = "CUDALY"
)

# New folders for LGBM results to keep things organized
if(!dir.exists("results_lgbm")) dir.create("results_lgbm")
if(!dir.exists("plots_lgbm")) dir.create("plots_lgbm")

lgbm_results <- list()

# Standard loop (LightGBM is fast enough without parallel usually)
for (m_name in names(metal_configs)) {
  
  col_code <- metal_configs[[m_name]]
  
  res <- run_lgbm_pipeline(m_name, col_code, df_master)
  
  if (!is.null(res)) {
    lgbm_results[[m_name]] <- res
    saveRDS(res, paste0("results_lgbm/result_", m_name, ".rds"))
    
    # Save Plot
    ggsave(
      filename = paste0("plots_lgbm/Shap_", m_name, "_LGBM.png"), 
      plot = res$plot, 
      width = 10, height = 8, bg = "white", dpi = 300
    )
    print(res$plot)
  }
}

saveRDS(lgbm_results, "R/lgbm_results_complete.rds")
message("LightGBM Pipeline Complete!")

# ==============================================================================
# 4. CROSS-VALIDATION & METRICS CALCULATION
# ==============================================================================

cat("\nCalculating LightGBM Metrics using 5-Fold Cross-Validation...\n")

# --- INITIALIZE THE TABLE HERE ---
perf_table_lgbm <- data.frame(
  Metal = character(),
  Balanced_Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  Precision = numeric(),
  AUC = numeric(),
  Bubbles_Detected = character(),
  stringsAsFactors = FALSE
)


for (m_name in names(lgbm_results)) {
  
  res <- lgbm_results[[m_name]]
  
  if (!is.null(res)) {
    # 1. Extract data from the saved result object
    X <- res$X
    y <- res$y
    
    # 2. Define parameters (same as in the training phase)
    params <- list(
      objective = "binary",
      learning_rate = 0.05,
      num_leaves = 31,
      is_unbalance = TRUE,
      feature_fraction = 0.8,
      bagging_fraction = 0.8,
      bagging_freq = 5,
      verbosity = -1
    )
    
    # 3. Run Cross-Validation (5-Fold)
    n_folds <- 5
    folds <- sample(rep(1:n_folds, length.out = nrow(X)))
    cv_preds <- numeric(nrow(X)) # Empty vector to store predictions
    
    for(k in 1:n_folds) {
      # Split into Train and Validation sets for the current fold
      idx_val <- which(folds == k)
      idx_train <- which(folds != k)
      
      d_train_fold <- lgb.Dataset(data = X[idx_train, , drop=FALSE], label = y[idx_train])
      
      # Train on 80% of the data
      bst_fold <- lgb.train(
        params = params, 
        data = d_train_fold, 
        nrounds = 200, 
        verbose = 0
      )
      
      # Test on the hidden 20% and store probabilities
      cv_preds[idx_val] <- predict(bst_fold, X[idx_val, , drop=FALSE])
    }
    
    # 4. AUC & Optimal Threshold Calculation
    roc_obj <- roc(y, cv_preds, quiet = TRUE)
    auc_val <- as.numeric(auc(roc_obj))
    
    # Find the Optimal Threshold (balances Sensitivity & Specificity)
    best_coords <- coords(roc_obj, "best", best.method = "closest.topleft", 
                          ret = c("threshold", "sensitivity", "specificity"))
    
    # Safety check: if coords returns multiple rows, take the first one
    if(is.matrix(best_coords) || is.data.frame(best_coords)) {
      best_thresh <- best_coords$threshold[1]
    } else {
      best_thresh <- best_coords["threshold"]
    }
    
    cat(paste("Optimal Threshold for", m_name, ":", round(best_thresh, 3), "\n"))
    
    # 5. Classification using the OPTIMAL Threshold (first one) or 0.5 (second)
    #pred_class <- ifelse(cv_preds > best_thresh, 1, 0)
    pred_class <- ifelse(cv_preds > 0.5, 1, 0)
    # 6. Metrics Calculation
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
    
    # 7. Add results to the table
    perf_table_lgbm <- rbind(perf_table_lgbm, data.frame(
      Metal = m_name,
      Balanced_Accuracy = bal_acc,
      Sensitivity = sens,
      Specificity = spec,
      Precision = prec,
      AUC = auc_val,          
      Bubbles_Detected = bubbles_str
    ))
    
    cat(paste("Finished CV for:", m_name, "- AUC:", round(auc_val, 3), "\n\n"))
  }
}

# Print the final dataframe to console
print(perf_table_lgbm)

# ==============================================================================
# 5. GENERATE LATEX TABLE
# ==============================================================================

# Clean up column names for the LaTeX output
colnames(perf_table_lgbm) <- c("Metal", "Bal. Accuracy", "Sensitivity", "Specificity", "Precision", "AUC", "Bubbles (Found/Total)")

# Ensure alignment is 8 characters long (1 index + 7 columns)
latex_obj_lgbm <- xtable(perf_table_lgbm, 
                         caption = "LightGBM Model Performance Metrics", 
                         label = "tab:lgbm_performance",
                         digits = 3, 
                         align = "llcccccc") 

#print(latex_obj_lgbm, 
      #include.rownames = FALSE, 
      #booktabs = TRUE, 
      #comment = FALSE,
      #file = "Table_Results_LGBM.tex")

print(latex_obj_lgbm)

# cat("\nLaTeX table saved as 'Table_Results_LGBM.tex'.\n")
# 
# # Standard loop (LightGBM is fast enough without parallel usually)
# for (m_name in names(metal_configs)) {
#   
#   col_code <- metal_configs[[m_name]]
#   
#   res <- run_lgbm_pipeline(m_name, col_code, df_master)
#   
#   if (!is.null(res)) {
#     lgbm_results[[m_name]] <- res
#     saveRDS(res, paste0("results_lgbm/result_", m_name, ".rds"))
#     
#     # Save Plot
#     ggsave(
#       filename = paste0("plots_lgbm/Shap_", m_name, "_LGBM.png"), 
#       plot = res$plot, 
#       width = 10, height = 8, bg = "white", dpi = 300
#     )
#     print(res$plot)
#   }
# }
# 
# saveRDS(lgbm_results, "R/lgbm_results_complete.rds")
# message("LightGBM Pipeline Complete!")
# 
# cat("\nCalculating LightGBM Metrics using 5-Fold Cross-Validation...\n")
# 
# for (m_name in names(lgbm_results)) {
#   
#   res <- lgbm_results[[m_name]]
#   
#   if (!is.null(res)) {
#     # 1. Extract data from the saved result object
#     X <- res$X
#     y <- res$y
#     
#     # 2. Define parameters (same as in the training phase)
#     params <- list(
#       objective = "binary",
#       learning_rate = 0.05,
#       num_leaves = 31,
#       is_unbalance = TRUE,
#       feature_fraction = 0.8,
#       bagging_fraction = 0.8,
#       bagging_freq = 5,
#       verbosity = -1
#     )
#     
#     # 3. Run Cross-Validation (5-Fold)
#     # This simulates the Out-Of-Bag (OOB) behavior seen in Random Forest
#     # We use a manual loop to easily extract the out-of-fold predictions.
#     
#     n_folds <- 5
#     folds <- sample(rep(1:n_folds, length.out = nrow(X)))
#     cv_preds <- numeric(nrow(X)) # Empty vector to store predictions
#     
#     for(k in 1:n_folds) {
#       # Split into Train and Validation sets for the current fold
#       idx_val <- which(folds == k)
#       idx_train <- which(folds != k)
#       
#       d_train_fold <- lgb.Dataset(data = X[idx_train, , drop=FALSE], label = y[idx_train])
#       
#       # Train on 80% of the data
#       bst_fold <- lgb.train(
#         params = params, 
#         data = d_train_fold, 
#         nrounds = 200, 
#         verbose = 0
#       )
#       
#       # Test on the hidden 20% and store probabilities
#       cv_preds[idx_val] <- predict(bst_fold, X[idx_val, , drop=FALSE])
#     }
#     
#     # 4. Now we have 'fair' (out-of-fold) probabilities stored in cv_preds
#     
#     # --- NEW: AUC & Optimal Threshold Calculation ---
#     
#     # Generate ROC Object
#     roc_obj <- roc(y, cv_preds, quiet = TRUE)
#     auc_val <- as.numeric(auc(roc_obj))
#     
#     # Find the Optimal Threshold (balances Sensitivity & Specificity)
#     best_coords <- coords(roc_obj, "best", best.method = "closest.topleft", 
#                           ret = c("threshold", "sensitivity", "specificity"))
#     best_thresh <- best_coords$threshold
#     
#     # Print the threshold so you can see how much it differs from 0.5
#     cat(paste("Optimal Threshold for", m_name, ":", round(best_thresh, 3), "\n"))
#     
#     # --- END NEW ---
#     
#     # 5. Classification using the OPTIMAL Threshold (not 0.5)
#     pred_class <- ifelse(cv_preds > best_thresh, 1, 0)
#     
#     # 6. Metrics Calculation
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
#     # 7. Add results to the table (Make sure AUC is included here!)
#     perf_table_lgbm <- rbind(perf_table_lgbm, data.frame(
#       Metal = m_name,
#       Balanced_Accuracy = bal_acc,
#       Sensitivity = sens,
#       Specificity = spec,
#       Precision = prec,
#       AUC = auc_val,          # <--- Added AUC
#       Bubbles_Detected = bubbles_str
#     ))
#     
#     cat(paste("Finished CV for:", m_name, "- AUC:", round(auc_val, 3), "\n"))
#   }
# }
# #nowy ale z³y CALCULATE METRICS (With AUC & Optimal Threshold)
# # ==============================================================================
# 
# # 1. Generate ROC Object
# # We compare the True Target (y) with the Predicted Probabilities (cv_preds)
# roc_obj <- roc(y, cv_preds, quiet = TRUE)
# auc_val <- as.numeric(auc(roc_obj))
# 
# # 2. Find Optimal Threshold (Optional but recommended)
# # Instead of 0.5, we find the cutoff that balances Sensitivity & Specificity
# best_coords <- coords(roc_obj, "best", best.method = "closest.topleft", 
#                       ret = c("threshold", "sensitivity", "specificity"))
# best_thresh <- best_coords$threshold
# 
# # 3. Make Binary Classifications (0 or 1)
# # You can use '0.5' here if you want standard behavior, 
# # or 'best_thresh' if you want to fix the low Balanced Accuracy.
# pred_class <- ifelse(cv_preds > best_thresh, 1, 0) 
# 
# # 4. Calculate Standard Metrics
# TP <- sum(pred_class == 1 & y == 1)
# TN <- sum(pred_class == 0 & y == 0)
# FP <- sum(pred_class == 1 & y == 0)
# FN <- sum(pred_class == 0 & y == 1)
# 
# sens <- TP / (TP + FN)
# spec <- TN / (TN + FP)
# bal_acc <- (sens + spec) / 2
# prec <- TP / (TP + FP)
# if(is.nan(prec)) prec <- 0
# 
# bubbles_str <- paste0(TP, "/", (TP + FN))
# 
# # 5. Add to Results Table
# # Ensure your 'perf_table_lgbm' has the AUC column initialized!
# perf_table_lgbm <- rbind(perf_table_lgbm, data.frame(
#   Metal = m_name,
#   Balanced_Accuracy = bal_acc,
#   Sensitivity = sens,
#   Specificity = spec,
#   Precision = prec,
#   AUC = auc_val,          # <--- The new AUC value
#   Bubbles_Detected = bubbles_str
# ))
# 
# # ==============================================================================
# # 4. PERFORMANCE METRICS (with Cross-Validation)
# # ==============================================================================
# perf_table_lgbm <- data.frame(
#   Metal = character(),
#   Balanced_Accuracy = numeric(),
#   Sensitivity = numeric(), 
#   Specificity = numeric(), 
#   Precision = numeric(),
#   Bubbles_Detected = character(),
#   stringsAsFactors = FALSE
# )
# 
# cat("\nCalculating LightGBM Metrics using 5-Fold Cross-Validation...\n")
# 
# for (m_name in names(lgbm_results)) {
#   
#   res <- lgbm_results[[m_name]]
#   
#   if (!is.null(res)) {
#     # 1. Pobieramy dane z zapisanego wyniku
#     X <- res$X
#     y <- res$y
#     
#     # 2. Definiujemy parametry (takie same jak w treningu)
#     params <- list(
#       objective = "binary",
#       learning_rate = 0.05,
#       num_leaves = 31,
#       is_unbalance = TRUE,
#       feature_fraction = 0.8,
#       bagging_fraction = 0.8,
#       bagging_freq = 5,
#       verbosity = -1
#     )
#     
#     # 3. Uruchamiamy Cross-Validation (5-Fold)
#     # To symuluje zachowanie OOB z Random Forest
#     dtrain <- lgb.Dataset(data = X, label = y)
#     
#     # lgb.cv automatycznie dzieli dane i zwraca wyniki walidacji
#     # Musimy jednak wyci¹gn¹æ predykcje rêcznie lub u¿yæ pêtli, 
#     # bo R-pakiet lightgbm jest specyficzny. Zrobimy to rêczn¹ pêtl¹ dla pewnoœci.
#     
#     n_folds <- 5
#     folds <- sample(rep(1:n_folds, length.out = nrow(X)))
#     cv_preds <- numeric(nrow(X)) # Puste pude³ko na wyniki
#     
#     for(k in 1:n_folds) {
#       # Dzielimy na Trening i Test
#       idx_val <- which(folds == k)
#       idx_train <- which(folds != k)
#       
#       d_train_fold <- lgb.Dataset(data = X[idx_train, , drop=FALSE], label = y[idx_train])
#       
#       # Trenujemy na 80%
#       bst_fold <- lgb.train(
#         params = params, 
#         data = d_train_fold, 
#         nrounds = 200, 
#         verbose = 0
#       )
#       
#       # Testujemy na ukrytych 20%
#       cv_preds[idx_val] <- predict(bst_fold, X[idx_val, , drop=FALSE])
#     }
#     
#     # 4. Teraz mamy 'uczciwe' prawdopodobieñstwa (cv_preds)
#     
#     # Classification Threshold 0.5
#     pred_class <- ifelse(cv_preds > 0.5, 1, 0)
#     
#     # Metrics Calculation
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
#     perf_table_lgbm <- rbind(perf_table_lgbm, data.frame(
#       Metal = m_name,
#       Balanced_Accuracy = bal_acc,
#       Sensitivity = sens,
#       Specificity = spec,
#       Precision = prec,
#       Bubbles_Detected = bubbles_str
#     ))
#     
#     cat(paste("Finished CV for:", m_name, "\n"))
#   }
# }
# 
# # --- Generate LaTeX ---
# colnames(perf_table_lgbm) <- c("Metal", "Bal. Accuracy", "Sensitivity", "Specificity", "Precision", "Bubbles (Found/Total)")
# 
# latex_obj <- xtable(perf_table_lgbm, 
#                     caption = "LightGBM Model Performance Metrics (5-Fold CV)", 
#                     label = "tab:lgbm_performance",
#                     digits = 3, 
#                     align = "lcccccc")
# 
# # Print to console
# print(perf_table_lgbm)
# 
# print(latex_obj, 
#       include.rownames = FALSE, 
#       booktabs = TRUE,      # Makes the table look professional (requires \usepackage{booktabs} in LaTeX)
#       comment = FALSE)  

# Save to file
#print(latex_obj, include.rownames = FALSE, booktabs = TRUE, comment = FALSE, file = "Table_Results_LGBM.tex")

