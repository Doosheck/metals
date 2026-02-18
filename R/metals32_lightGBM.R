# ==============================================================================
# SCRIPT: 02_LightGBM.R
# PURPOSE: Benchmark Model (Gradient Boosting)
# ==============================================================================

library(tidyverse)
library(lightgbm) # install.packages("lightgbm")
library(shapviz)
library(xtable)

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
# 4. PERFORMANCE METRICS (FIXED with Cross-Validation)
# ==============================================================================
perf_table_lgbm <- data.frame(
  Metal = character(),
  Balanced_Accuracy = numeric(),
  Sensitivity = numeric(), 
  Specificity = numeric(), 
  Precision = numeric(),
  Bubbles_Detected = character(),
  stringsAsFactors = FALSE
)

cat("\nCalculating LightGBM Metrics using 5-Fold Cross-Validation...\n")

for (m_name in names(lgbm_results)) {
  
  res <- lgbm_results[[m_name]]
  
  if (!is.null(res)) {
    # 1. Pobieramy dane z zapisanego wyniku
    X <- res$X
    y <- res$y
    
    # 2. Definiujemy parametry (takie same jak w treningu)
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
    
    # 3. Uruchamiamy Cross-Validation (5-Fold)
    # To symuluje zachowanie OOB z Random Forest
    dtrain <- lgb.Dataset(data = X, label = y)
    
    # lgb.cv automatycznie dzieli dane i zwraca wyniki walidacji
    # Musimy jednak wyci¹gn¹æ predykcje rêcznie lub u¿yæ pêtli, 
    # bo R-pakiet lightgbm jest specyficzny. Zrobimy to rêczn¹ pêtl¹ dla pewnoœci.
    
    n_folds <- 5
    folds <- sample(rep(1:n_folds, length.out = nrow(X)))
    cv_preds <- numeric(nrow(X)) # Puste pude³ko na wyniki
    
    for(k in 1:n_folds) {
      # Dzielimy na Trening i Test
      idx_val <- which(folds == k)
      idx_train <- which(folds != k)
      
      d_train_fold <- lgb.Dataset(data = X[idx_train, , drop=FALSE], label = y[idx_train])
      
      # Trenujemy na 80%
      bst_fold <- lgb.train(
        params = params, 
        data = d_train_fold, 
        nrounds = 200, 
        verbose = 0
      )
      
      # Testujemy na ukrytych 20%
      cv_preds[idx_val] <- predict(bst_fold, X[idx_val, , drop=FALSE])
    }
    
    # 4. Teraz mamy 'uczciwe' prawdopodobieñstwa (cv_preds)
    
    # Classification Threshold 0.5
    pred_class <- ifelse(cv_preds > 0.5, 1, 0)
    
    # Metrics Calculation
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
    
    perf_table_lgbm <- rbind(perf_table_lgbm, data.frame(
      Metal = m_name,
      Balanced_Accuracy = bal_acc,
      Sensitivity = sens,
      Specificity = spec,
      Precision = prec,
      Bubbles_Detected = bubbles_str
    ))
    
    cat(paste("Finished CV for:", m_name, "\n"))
  }
}

# --- Generate LaTeX ---
colnames(perf_table_lgbm) <- c("Metal", "Bal. Accuracy", "Sensitivity", "Specificity", "Precision", "Bubbles (Found/Total)")

latex_obj <- xtable(perf_table_lgbm, 
                    caption = "LightGBM Model Performance Metrics (5-Fold CV)", 
                    label = "tab:lgbm_performance",
                    digits = 3, 
                    align = "lcccccc")

# Print to console
print(perf_table_lgbm)

print(latex_obj, 
      include.rownames = FALSE, 
      booktabs = TRUE,      # Makes the table look professional (requires \usepackage{booktabs} in LaTeX)
      comment = FALSE)  

# Save to file
#print(latex_obj, include.rownames = FALSE, booktabs = TRUE, comment = FALSE, file = "Table_Results_LGBM.tex")

