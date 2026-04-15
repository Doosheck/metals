# --- Function to Install and Load Packages ---
install_and_load <- function(packages) {
  
  # 1. Identify missing packages
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  # 2. Install missing packages (if any)
  if(length(new_packages) > 0) {
    message("Installing missing packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages, dependencies = TRUE)
  } else {
    message("All packages are already installed.")
  }
  
  # 3. Load all packages
  # We use invisible() to suppress the "Attaching package..." startup messages
  invisible(lapply(packages, library, character.only = TRUE))
}

# --- Define Your Required Libraries ---
required_libs <- c(
  "tidyverse",   # Data manipulation & plotting
  "tidyquant",   # Financial data & functions
  "here",        # File paths
  "ranger",      # Random Forest
  "shapviz",     # SHAP plots
  "kernelshap",  # SHAP calculation
  "xtable",      # latex table
  "TTR"          # Volatility/Technical indicators
)

library(future)
library(doFuture)
library(xtable)
library(pROC)

# --- Execute ---
install_and_load(required_libs)
library(tidyverse)   # Loads ggplot2, dplyr, tidyr, readr, purrr all at once
library(tidyquant)   # For downloading Macro/Market data (FRED/Yahoo) + financial functions
library(here)        # For robust file paths (works across different OS)
library(ranger)      # Fast, multi-threaded implementation of Random Forest (replaces 'randomForest')
library(shapviz)     # Best tool for SHAP visualizations (Beeswarm plots)
library(kernelshap)  # Calculates SHAP values (model-agnostic, works seamlessly with ranger)
library(TTR)         # Technical Trading Rules (needed for rolling volatility/RSI calculations)
library(exuber)



#----ML----

##----STEP 1 Feature Engineering (The "Composite Market Dynamics" Logic)----
###----new function inlcuding DV of other metals to be predictors ----

prepare_ml_data <- function(target_metal, target_col_name, df_master) {
  
  target_dummy <- paste0(target_col_name, "_BD")
  
  if(!target_dummy %in% names(df_master)) {
    stop(paste("Error: Column", target_dummy, "not found in df_master"))
  }
  
  # A. Identyfikacja zmiennych
  predictors_numeric <- df_master %>%
    select(where(is.numeric)) %>%
    select(-ends_with("BD")) %>%
    select(-any_of(c("Date", "date", "DATE"))) %>% 
    names()
  
  peer_dummies <- df_master %>%
    select(ends_with("BD")) %>%
    select(-all_of(target_dummy)) %>%
    names()
  
  # B. Funkcja pomocnicza: NAPRAWA UJEMNYCH WARTOŒCI
  # Zamiast NA, wstawiamy 0.001. To pozwala policzyæ logarytm, 
  # a jednoczeœnie generuje bardzo du¿y ujemny zwrot (sygna³ krachu dla modelu).
  calc_safe_ret <- function(x) {
    x_num <- as.numeric(x)
    
    # --- TU JEST ZMIANA ---
    # Jeœli wartoœæ <= 0, zamieñ j¹ na 0.001 (nie wyrzucaj!)
    # which() jest potrzebne, ¿eby nie wywali³o siê na istniej¹cych NA
    bad_idx <- which(x_num <= 0)
    if(length(bad_idx) > 0) {
      x_num[bad_idx] <- 0.001 
    }
    
    c(NA, diff(log(x_num)))
  }
  
  calc_safe_vol <- function(x) {
    x_num <- as.numeric(x)
    
    bad_idx <- which(x_num <= 0)
    if(length(bad_idx) > 0) {
      x_num[bad_idx] <- 0.001
    }
    
    TTR::runSD(c(NA, diff(log(x_num))), n = 10)
  }
  
  # C. Przetwarzanie
  df_features <- df_master %>%
    arrange(Date) %>%
    mutate(
      across(all_of(predictors_numeric), 
             list(
               Ret = calc_safe_ret, 
               Vol = calc_safe_vol
             ),
             .names = "{.col}_{.fn}")
    )
  
  # D. Lagowanie
  df_model <- df_features %>%
    mutate(
      Target_Bubble = .data[[target_dummy]], 
      
      across(ends_with("_Ret"), ~ lag(.), .names = "{.col}_Lag1"),
      across(ends_with("_Vol"), ~ lag(.), .names = "{.col}_Lag1"),
      across(all_of(predictors_numeric), ~ lag(.), .names = "{.col}_Level_Lag1"),
      
      # Peer lags (0 zamiast NA dla zmiennych binarnych)
      across(all_of(peer_dummies), ~ replace_na(lag(.), 0), .names = "{.col}_Lag1")
    ) %>%
    select(Date, Target_Bubble, ends_with("Lag1"))
  
  # E. Finalne czyszczenie
  # Teraz drop_na usunie tylko ok. 10 pierwszych dni (przez lagi), 
  # a nie œrodek kryzysu COVID.
  df_final <- drop_na(df_model)
  
  return(df_final)
}

##---- STEP 2: The ML Pipeline (Random Forest + SHAP)----
run_ml_pipeline_robust <- function(metal_name, col_name, df_data) {
  
  cat(paste("\n=== Processing:", metal_name, "===\n"))
  
  # A. Prepare Data
  df_ml <- tryCatch({
    prepare_ml_data(metal_name, col_name, df_data)
  }, error = function(e) {
    message(paste("Error preparing data:", e$message))
    return(NULL)
  })
  
  if (is.null(df_ml)) return(NULL)
  
  # Check bubbles count
  n_bubbles <- sum(df_ml$Target_Bubble)
  cat(paste("Bubbles found:", n_bubbles, "\n"))
  
  if (n_bubbles < 10) {
    message("Skipping - Not enough bubbles (<10) for training.")
    return(NULL)
  }
  
  # B. Train Model
  fit_rf <- ranger(
    as.factor(Target_Bubble) ~ ., 
    data = select(df_ml, -Date), 
    importance = "permutation",
    probability = TRUE, 
    num.trees = 500
  )
  
  # C. SHAP Calculation
  X_explain <- select(df_ml, -Date, -Target_Bubble)
  bg_X <- X_explain[sample(nrow(X_explain), min(25, nrow(X_explain))), ] 
  rows_to_explain <- sample(nrow(X_explain), min(100, nrow(X_explain)))
  
  shap_values <- kernelshap(fit_rf, X_explain[rows_to_explain, ], bg_X = bg_X)
  viz <- shapviz(shap_values)
  
  # Extract Bubble Class
  if (inherits(viz, "mshapviz")) {
    viz_bubble <- tryCatch({ viz[["1"]] }, error = function(e) viz[[2]])
  } else {
    viz_bubble <- viz
  }
  
  # D. Plot (Clean Labels)
  clean_labels <- function(x) {
    x <- gsub("_Lag1", "", x)
    x <- gsub("_Level", "", x)
    x <- gsub("_Ret", " Returns", x)
    x <- gsub("_Vol", " Volatility", x)
    x <- gsub("_dummy", " Bubble", x)
    return(x)
  }
  
  plot_obj <- sv_importance(viz_bubble, kind = "beeswarm") +
    theme_minimal() +
    labs(title = NULL) + 
    scale_y_discrete(labels = clean_labels) +
    theme(
      axis.text.y  = element_text(size = 14, color = "black"), 
      axis.text.x  = element_text(size = 12, color = "black"), 
      axis.title.x = element_text(size = 14, face = "bold"),   
      legend.text  = element_text(size = 10),
      legend.title = element_text(size = 12)
    )
  
  return(list(model = fit_rf, plot = plot_obj, shap_obj = viz_bubble))
}

##----# STEP 3: Execution Loop----

# This line tells R to use 4 background workers.
# "multisession" works on both Windows and Mac.
registerDoFuture()
plan(multisession, workers = 4)

message("Parallel processing enabled: Using 4 cores.")

# Define your metal configurations.
# KEY: The name in the vector (e.g., "CODALY") must match the prefix of your dummy column.
# If your dummy is "CODALY_dummy", put "CODALY" here.

# --- 3. Execution Loop with Incremental Saving ---
metal_configs <- list(
  "Cobalt"  = "CODALY", 
  "Lithium" = "LIDALY", 
  "Nickel"  = "NIDALY", 
  "Copper"  = "CUDALY"
)

# Create a folder for results if it doesn't exist
if(!dir.exists("results_rds")) dir.create("results_rds")
ml_results <- list()

for (m_name in names(metal_configs)) {
  
  col_code <- metal_configs[[m_name]]
  
  # Run pipeline
  res <- run_ml_pipeline_robust(m_name, col_code, df_master)
  
  if (!is.null(res)) {
    ml_results[[m_name]] <- res
    saveRDS(res, paste0("results_rds/result_", m_name, ".rds"))
    print(res$plot) # Display plot
  }
}


# ---Cleanup ---
plan(sequential)
saveRDS(ml_results, "R/RF_results_complete_new.rds")
message("All done! Results saved to 'ml_results_complete.rds'.")

# --- 4. Accessing Results Later ---
# You can now pull up specific plots like this:
ml_results[["Cobalt"]]$plot
ml_results[["Lithium"]]$plot
ml_results[["Nickel"]]$plot
ml_results[["Copper"]]$plot

ml_results$Copper$plot
print(ml_results$Cobalt$model)
sv_importance(ml_results$Nickel$shap_obj, kind = "beeswarm")

# --- 1. Define the Short Label Function ---
# This function shortens variable names for better readability on the Y-axis.
clean_labels_short <- function(x) {
  # Remove technical suffixes
  x <- gsub("_Lag1", "", x)           # Remove "_Lag1"
  x <- gsub("_Level", "", x)          # Remove "_Level"
  x <- gsub("_dummy", " Bubble", x)   # Change "_dummy" to " Bubble" (for peers)
  
  # Shorten main terms (The requested change)
  x <- gsub("_Vol", " Vol", x)        # Change "_Vol" or "Volatility" to " Vol"
  x <- gsub("Volatility", " Vol", x)  # Catch explicit "Volatility" if present
  x <- gsub("_Ret", " Ret", x)        # Change "_Ret" or "Returns" to " Ret"
  x <- gsub("Returns", " Ret", x)     # Catch explicit "Returns" if present
  
  return(x)
}

# --- 2. Create Output Directory ---
if(!dir.exists("plots")) dir.create("plots")

# --- 3. Loop Through Results and Update ---
cat("Updating plots with shorter labels...\n")

for (m_name in names(ml_results)) {
  
  # Retrieve the existing plot object
  old_plot <- ml_results[[m_name]]$plot
  
  if (!is.null(old_plot)) {
    
    # Update the plot layers
    # We add a new scale_y_discrete to overwrite the previous labels
    new_plot <- old_plot +
      scale_y_discrete(labels = clean_labels_short) +
      theme(
        # Ensure fonts remain large and readable
        axis.text.y  = element_text(size = 14, color = "black"), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.x  = element_text(size = 12, color = "black"),
        legend.position = "right",
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 12)
      )
    
    # Save the updated plot
    filename <- paste0("plots/Shap_", m_name, "_Short.png")
    
    ggsave(
      filename = filename, 
      plot = new_plot, 
      width = 10, 
      height = 8, 
      bg = "white",
      dpi = 300
    )
    
    cat(paste("Saved:", filename, "\n"))
  }
}

cat("Done! Check the 'plots' folder.")





###---- Run Update for Lithium ----

# # 1. Define configuration for Lithium
# metal_name <- "Lithium"
# col_code   <- "LIDALY" # Ensure this matches your price column name in df_master
# 
# # 2. Run the pipeline
# # This will overwrite the previous entry for Lithium in your results list
# lit_result <- run_ml_pipeline(metal_name, col_code, df_master)
# 
# # 3. Store and View
# if (!is.null(lit_result)) {
#   ml_results[[metal_name]] <- lit_result
#   print(lit_result$plot) # This should now show ONLY the Bubble drivers (Single panel)
# }

##---- Balanced Accuracy and AUC-ROC----
install.packages("pROC")
library(pROC)

# --- 1. Initialize Storage ---
performance_table <- data.frame(
  Metal = character(),
  Balanced_Accuracy = numeric(),
  Sensitivity = numeric(), 
  Specificity = numeric(), 
  Precision = numeric(),
  AUC = numeric(),         
  Bubbles_Detected = character(), # Changed to character for "150/200" format
  stringsAsFactors = FALSE
)

# --- 2. Calculation Loop ---
for (m_name in names(ml_results)) {
  
  res_obj <- ml_results[[m_name]]
  
  if (!is.null(res_obj)) {
    model_rf <- res_obj$model
    col_code <- metal_configs[[m_name]]
    
    # Re-create data to get True Labels
    df_check <- tryCatch({
      prepare_ml_data(m_name, col_code, df_master)
    }, error = function(e) return(NULL))
    
    if (!is.null(df_check)) {
      true_y <- df_check$Target_Bubble
      prob_bubble <- model_rf$predictions[, 2] 
      roc_obj <- roc(true_y, prob_bubble, quiet = TRUE)
      auc_val <- as.numeric(auc(roc_obj))
      pred_class <- ifelse(prob_bubble > 0.5, 1, 0)
      
      # Confusion Matrix Counts
      TP <- sum(pred_class == 1 & true_y == 1)
      TN <- sum(pred_class == 0 & true_y == 0)
      FP <- sum(pred_class == 1 & true_y == 0)
      FN <- sum(pred_class == 0 & true_y == 1)
      
      # Calculate Metrics
      sens <- TP / (TP + FN)
      spec <- TN / (TN + FP)
      bal_acc <- (sens + spec) / 2
      prec <- TP / (TP + FP)
      if(is.nan(prec)) prec <- 0
      
      # Format "Bubbles Found / Total" string
      bubbles_str <- paste0(TP, "/", (TP + FN))
      
      # Add to table
      performance_table <- rbind(performance_table, data.frame(
        Metal = m_name,
        Balanced_Accuracy = bal_acc,
        Sensitivity = sens,
        Specificity = spec,
        Precision = prec,
        AUC = auc_val,
        Bubbles_Detected = bubbles_str
      ))
    }
  }
}

# --- 3. Convert to LaTeX using xtable ---

# Rename columns for the paper (cleaner headers)
colnames(performance_table) <- c("Metal", "Bal_Accuracy", "Sensitivity", 
                                 "Specificity", "Precision", "AUC", "Bubbles")

# performance_table <- performance_table %>%
#   mutate(
#     Balanced_Accuracy = ifelse(is.nan(Bal_Accuracy), 0, Bal_Accuracy),
#     Sensitivity       = ifelse(is.nan(Sensitivity), 0, Sensitivity),
#     Specificity       = ifelse(is.nan(Specificity), 0, Specificity),
#     Precision         = ifelse(is.nan(Precision), 0, Precision),
#     AUC               = ifelse(is.nan(AUC), 0, AUC) 
#   )

# Create the xtable object
latex_obj <- xtable(performance_table, 
                    caption = "Random Forest Model Performance Metrics", 
                    label = "tab:rf_performance",
                    digits = 3, 
                    align = "llcccccc")

# Option A: Print to Console (Clean copy-paste)
print(latex_obj, 
      include.rownames = FALSE, 
      booktabs = TRUE,      # Makes the table look professional (requires \usepackage{booktabs} in LaTeX)
      comment = FALSE)      # Removes the version comment

# Option B: Save to file (Best method)
# print(latex_obj, 
      #include.rownames = FALSE, 
      #booktabs = TRUE, 
      #comment = FALSE,
      #file = "Table_Results.tex") # <--- Checks your folder for this file!

#---- 2nd approach - a division into training and testing -----
Przeanalizowa³em kod pod k¹tem potencjalnych b³êdów, stabilnoœci ("edge cases") oraz zgodnoœci z Twoimi wytycznymi dotycz¹cymi pracy na OneDrive i czystoœci kodu.

Znalaz³em kilka drobnych, ale wa¿nych miejsc, które wymaga³y ostatecznego szlifu, aby upewniæ siê, ¿e pêtla nie "wywali siê" w trakcie obliczeñ (zw³aszcza na zbiorze testowym).

Lista finalnych poprawek:
  Bezpieczne wyci¹ganie prawdopodobieñstw: Zamiast wybieraæ drug¹ kolumnê predykcji w ciemno (predictions[, 2]), u¿y³em jawnego odwo³ania do nazwy klasy (predictions[, "1"]). Chroni to przed sytuacj¹, w której ranger zamieni kolejnoœæ kolumn.

Zabezpieczenie AUC-ROC (Safe Guard): Jeœli w zbiorze testowym (stanowi¹cym 20% danych) nie bêdzie ani jednej bañki, funkcja pROC::roc() zwróci³aby b³¹d i zatrzyma³a ca³¹ pêtlê. Doda³em warunek sprawdzaj¹cy, czy w true_y s¹ obecne obie klasy.

Tidyverse w Ewaluacji: Zamieni³em powolne rbind() w pêtli na czyste podejœcie z wykorzystaniem list i bind_rows(), co jest standardem w tidyverse.

Oto ostateczna, w pe³ni kuloodporna wersja kodu:
  
  R
##---- LIBRARIES ----
library(tidyverse)
library(ranger)
library(kernelshap)
library(shapviz)
library(future.apply)
library(doFuture)
library(pROC)
library(xtable)

##---- STEP 1: Feature Engineering ----
prepare_ml_data <- function(target_col_prefix, df_master) {
  
  target_dummy <- paste0(target_col_prefix, "_BD")
  
  if (!target_dummy %in% names(df_master)) {
    stop(paste("Error: Column", target_dummy, "not found in df_master"))
  }
  
  # A. Identify variables
  predictors_numeric <- df_master %>%
    select(where(is.numeric)) %>%
    select(-ends_with("BD"), -any_of(c("Date", "date", "DATE"))) %>%
    names()
  
  peer_dummies <- df_master %>%
    select(ends_with("BD")) %>%
    select(-all_of(target_dummy)) %>%
    names()
  
  # B. Helper functions for safe returns and volatility
  calc_safe_ret <- function(x) {
    x_num <- as.numeric(x)
    x_num[x_num <= 0] <- 0.001 # Prevent log(0)
    c(NA, diff(log(x_num)))
  }
  
  calc_safe_vol <- function(x) {
    x_num <- as.numeric(x)
    x_num[x_num <= 0] <- 0.001
    TTR::runSD(c(NA, diff(log(x_num))), n = 10)
  }
  
  # C. Processing and Lagging
  df_model <- df_master %>%
    arrange(Date) %>%
    mutate(
      across(all_of(predictors_numeric), 
             list(Ret = calc_safe_ret, Vol = calc_safe_vol), 
             .names = "{.col}_{.fn}")
    ) %>%
    mutate(
      Target_Bubble = factor(.data[[target_dummy]], levels = c("0", "1")),
      across(ends_with("_Ret"), ~ lag(.), .names = "{.col}_Lag1"),
      across(ends_with("_Vol"), ~ lag(.), .names = "{.col}_Lag1"),
      across(all_of(predictors_numeric), ~ lag(.), .names = "{.col}_Level_Lag1"),
      across(all_of(peer_dummies), ~ replace_na(lag(.), 0), .names = "{.col}_Lag1")
    ) %>%
    select(Date, Target_Bubble, ends_with("Lag1")) %>%
    drop_na()
  
  return(df_model)
}

##---- STEP 2: The ML Pipeline (with Train/Test Split) ----
run_ml_pipeline_robust <- function(metal_label, col_prefix, df_data, train_prop = 0.8) {
  
  message(paste("\n=== Processing:", metal_label, "==="))
  
  df_ml <- tryCatch({
    prepare_ml_data(col_prefix, df_data)
  }, error = function(e) {
    message("Error preparing data: ", e$message)
    return(NULL)
  })
  
  if (is.null(df_ml)) return(NULL)
  
  # Check bubble prevalence
  n_bubbles <- sum(df_ml$Target_Bubble == "1")
  if (n_bubbles < 10) {
    message("Skipping - Not enough bubbles (<10).")
    return(NULL)
  }
  
  # --- Chronological Split ---
  n_obs <- nrow(df_ml)
  split_idx <- floor(n_obs * train_prop)
  
  df_train <- df_ml[1:split_idx, ]
  df_test  <- df_ml[(split_idx + 1):n_obs, ]
  
  # Train Model (on Train Set)
  fit_rf <- ranger(
    Target_Bubble ~ ., 
    data        = select(df_train, -Date), 
    importance  = "permutation",
    probability = TRUE, 
    num.trees   = 500
  )
  
  # SHAP Calculation (on Test Set)
  X_test <- select(df_test, -Date, -Target_Bubble)
  bg_X   <- X_test[sample(nrow(X_test), min(25, nrow(X_test))), ]
  
  shap_values <- kernelshap(fit_rf, X_test, bg_X = bg_X)
  viz <- shapviz(shap_values, focus = "1") 
  
  # Label Cleaning Function
  clean_labels_short <- function(x) {
    x %>% 
      str_remove_all("_Lag1|_Level") %>% 
      str_replace("_Ret", " Ret") %>% 
      str_replace("_Vol", " Vol") %>%
      str_replace("_BD", " Bubble")
  }
  
  # Generate Plot
  plot_obj <- sv_importance(viz, kind = "beeswarm") +
    scale_y_discrete(labels = clean_labels_short) +
    theme_minimal() +
    labs(title = paste(metal_label, "Feature Importance (Out-of-Sample)"))
  
  return(list(
    model = fit_rf, 
    plot = plot_obj, 
    shap_obj = viz, 
    test_data = df_test
  ))
}

##---- STEP 3: Execution Loop ----
registerDoFuture()
plan(multisession, workers = 4)

metal_configs <- list(
  "Cobalt"  = "CODALY", 
  "Lithium" = "LIDALY", 
  "Nickel"  = "NIDALY", 
  "Copper"  = "CUDALY"
)

# Flat folder structure (OneDrive safe)
if(!dir.exists("results_rds")) dir.create("results_rds")
ml_results <- list()

for (m_name in names(metal_configs)) {
  res <- run_ml_pipeline_robust(m_name, metal_configs[[m_name]], df_master)
  if (!is.null(res)) {
    ml_results[[m_name]] <- res
    saveRDS(res, paste0("results_rds/result_", m_name, ".rds"))
  }
}
plan(sequential)

##---- STEP 4: Performance Evaluation (Out-of-Sample) ----
eval_results <- list()

for (m_name in names(ml_results)) {
  res_obj <- ml_results[[m_name]]
  test_df <- res_obj$test_data
  
  # Safely predict and extract probabilities for class "1"
  preds_prob <- predict(res_obj$model, data = select(test_df, -Date, -Target_Bubble))$predictions[, "1"]
  preds_class <- ifelse(preds_prob > 0.5, 1, 0)
  true_y <- as.numeric(as.character(test_df$Target_Bubble))
  
  # Safely calculate AUC (requires both 0 and 1 in true labels)
  if (length(unique(true_y)) > 1) {
    roc_obj <- roc(true_y, preds_prob, quiet = TRUE)
    auc_val <- as.numeric(auc(roc_obj))
  } else {
    auc_val <- NA # Not enough classes to calculate AUC
    message(paste("Warning:", m_name, "test set does not contain both classes for AUC."))
  }
  
  # Metrics
  TP <- sum(preds_class == 1 & true_y == 1)
  TN <- sum(preds_class == 0 & true_y == 0)
  FP <- sum(preds_class == 1 & true_y == 0)
  FN <- sum(preds_class == 0 & true_y == 1)
  
  # Handle division by zero
  sens <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  spec <- ifelse((TN + FP) == 0, 0, TN / (TN + FP))
  
  eval_results[[m_name]] <- data.frame(
    Metal = m_name,
    Bal_Acc = (sens + spec) / 2,
    Sens = sens,
    Spec = spec,
    AUC = auc_val,
    Bubbles = paste0(TP, "/", (TP + FN))
  )
}

# Combine all results into one table using tidyverse
performance_table <- bind_rows(eval_results)

# Print LaTeX code to console
print(xtable(performance_table, caption = "Out-of-Sample Performance", digits = 3), 
      include.rownames = FALSE)
             