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

# ----Construct dataset----
## ---- Metals ----

# --- Define File Paths ---
# Adjust filenames here to match your exact folder structure
metal_files <- list(
  "Cobalt"  = "data/ALL_cobalt_prices_cubic_spline.csv",
  "Copper"  = "data/ALL_copper_prices_cubic_spline.csv",
  "Lithium" = "data/ALL_lithium_prices_cubic_spline.csv",
  "Nickel"  = "data/ALL_nickel_prices_cubic_spline.csv"
)

# ---- 1 st approach: Define Selection Function based on the length of series---
get_longest_series_from_file <- function(file_path, metal_name) {
  
  # Read Data
  df <- read_csv(here(file_path), show_col_types = FALSE, guess_max = 10000)
  
  # Identify potential price columns (numeric, excluding Date)
  price_cols <- df %>%
    select(where(is.numeric)) %>%
    names()
  
  # Count valid (non-NA) observations for each column
  counts <- df %>%
    summarise(across(all_of(price_cols), ~ sum(!is.na(.)))) %>%
    pivot_longer(everything(), names_to = "Series", values_to = "Count") %>%
    arrange(desc(Count))
  
  # Pick the winner
  best_series <- counts$Series[1]
  best_count  <- counts$Count[1]
  
  # Print Info for the user
  cat(sprintf("Metal: %-10s | Best Series: %-10s | Observations: %d\n", 
              metal_name, best_series, best_count))
  
  # Return just the Date and the Winner
  df %>%
    select(Date, all_of(best_series))
}

# --- 3. Execute and Merge ---

cat("\n=== Selection Report ===\n")

# Apply function to all files and store in a list
list_of_frames <- imap(metal_files, ~ get_longest_series_from_file(.x, .y))

# Merge all frames by Date
# full_join keeps all dates; inner_join would keep only overlapping dates immediately
df_merged_all <- list_of_frames %>%
  reduce(full_join, by = "Date") %>%
  arrange(Date)

# --- 4. Create "Strict Overlap" Version ---
# This version removes rows where ANY metal is missing (useful for correlations/VAR)
df_overlap <- df_merged_all %>%
  drop_na()

# --- 5. Summary ---
cat("\n=== Final Dataset Dimensions ===\n")
cat("Merged (All Dates):", dim(df_merged_all), "\n")
cat("Strict Overlap:    ", dim(df_overlap), "\n")

head(df_overlap)

### ---- 2. approach: Select Series by Source Pattern ----
get_series_by_source <- function(file_path, metal_name, source_pattern) {
  
  # Read Data
  df <- read_csv(here(file_path), show_col_types = FALSE, guess_max = 10000)
  
  # Find columns that match the source pattern (case-insensitive)
  # We exclude "Date" and ensure they are numeric
  matching_cols <- df %>%
    select(matches(source_pattern, ignore.case = TRUE)) %>%
    select(where(is.numeric)) %>%
    names()
  
  # Check if we found anything
  if (length(matching_cols) == 0) {
    warning(paste("No series found for", metal_name, "with source:", source_pattern))
    return(NULL)
  }
  
  # If multiple columns match (e.g., LME_Cash vs LME_3Mo), pick the longest one
  best_col <- df %>%
    summarise(across(all_of(matching_cols), ~ sum(!is.na(.)))) %>%
    pivot_longer(everything(), names_to = "Series", values_to = "Count") %>%
    arrange(desc(Count)) %>%
    pull(Series) %>%
    .[1] # Take the first one
  
  cat(sprintf("Found %-10s for %-10s: %s\n", source_pattern, metal_name, best_col))
  
  # Return Date and the selected column
  df %>%
    select(Date, all_of(best_col))
}

# --- 3. Wrapper to Build the Full Dataset ---
build_robustness_dataset <- function(source_name) {
  
  cat(paste0("\n=== Building Dataset for Source: ", source_name, " ===\n"))
  
  # Map through files, but remove NULLs if a metal doesn't have that source
  list_of_frames <- imap(metal_files, ~ get_series_by_source(.x, .y, source_name)) %>%
    keep(~ !is.null(.x))
  
  if(length(list_of_frames) == 0) {
    stop("No data found for this source pattern.")
  }
  
  # Merge
  df_merged <- list_of_frames %>%
    reduce(full_join, by = "Date") %>%
    arrange(Date) %>%
    drop_na() # Strict overlap for modeling
  
  return(df_merged)
}
# --- 4. Execution Examples ---

# Scenario A: Get all "DALY" series
df_daly <- build_robustness_dataset("DALY")
cat("DALY Dataset Dimensions:", dim(df_daly), "\n")

# Scenario B: Get all "LME" series
# Note: Lithium might not have LME data in your file, the code will warn and skip it
df_lme <- build_robustness_dataset("LME")
cat("LME Dataset Dimensions:", dim(df_lme), "\n")

# Scenario C: Get "WUXI" (Chinese exchange) series
df_wuxi <- build_robustness_dataset("WUXI")
cat("WUXI Dataset Dimensions:", dim(df_wuxi), "\n")

# Check the column names to see what you got
print(names(df_daly))
str(df_daly)

###---- Bubble detection:----
data_matrix <- df_daly %>%
  select(-Date) %>%
  as.data.frame()
rownames(data_matrix) <- as.character(df_daly$Date)

est_results <- radf(data_matrix)
n_obs <- nrow(data_matrix)
mc_cv <- radf_mc_cv(n = n_obs, seed = 123) # Seed ensures reproducibility
mc_cv
saveRDS(mc_cv, "mc_cv_bubble_df_daly.rds")
# for the next time:
# mc_cv_bubble_df_daly <- readRDS("mc_cv.rds")

summary(est_results, cv = mc_cv)
bubble_dates <- datestamp(est_results, cv = mc_cv)
series_names <- names(data_matrix)  # Exclude Date column
dummy_matrix <- matrix(0L, nrow = nrow(df_daly), ncol = length(series_names))
colnames(dummy_matrix) <- paste0(series_names, "_dummy") # e.g., CODALY_dummy

# Fill in 1s where bubbles are detected
for (series in series_names) {
  
  # Get the start/end periods for this specific series
  periods <- bubble_dates[[series]]
  
  # Check if any bubbles exist
  if (!is.null(periods) && nrow(periods) > 0) {
    
    # Loop through each detected bubble episode
    for (i in 1:nrow(periods)) {
      # exuber returns row INDICES (integers), so we can use them directly
      start_idx <- periods[i, "Start"]
      end_idx   <- periods[i, "End"]
      
      # Find the column index in our dummy matrix that matches this series
      col_idx <- which(colnames(dummy_matrix) == paste0(series, "_dummy"))
      
      # Set rows to 1
      dummy_matrix[start_idx:end_idx, col_idx] <- 1L
    }
  }
}
df_final_dataset <- bind_cols(df_daly, as_tibble(dummy_matrix))
head(df_final_dataset)

saveRDS(df_final_dataset, here("data", "bubble_dummies_df_daly.rds"))
# Load it later (it will be exactly the same tibble)
readRDS("data/bubble_dummies_df_daly.rds")

## ----Macrovariables----
# # Tickers map:
# DXY: US Dollar Index US Dollar Index (DX-Y.NYB)
# VIX: Volatility/Fear Index
# TNX: 10-Year Treasury Yield (Opportunity cost)
# ^GSPC: SP500 Global Demand Proxy
# MSCI: Global Stock Market Proxy
# MME=F: MSCI Emerging Markets Index Fut 
# KEUA = Carbon Credits (KraneShares European Carbon Allowance ETF) - Best free proxy for EUA
# BZT=F = Brent Crude Oil Futures
# URTH  = MSCI World ETF (Proxy for MSCI World Index, which is often restricted)
# CL=F: Brent Crude Oil Last Day Future 
# HH=F: Natural Gas (Henry Hub) - na razie dajê spokoj, dziwnie wyglada
# QCLN: First Trust NASDAQ Clean Edge Green Energy Index Fund ()
# Gold Apr 26 (GC=F) yahoo
# DE000A1EY8J4.SG (yahoo) Solactive Global Lithium Index Stuttgart EUR
# Solactive Solar Index GTR (DE000SL0EBG1.SG) Stuttgart EUR -little changes over time

# China Coal Energy Company Limited (1898.HK)





macro_tickers <- c("DCOILWTICO", "DHHNGSP", "DTWEXBGS", "VIXCLS", "DGS10")
# Download data from FRED

df_macro <- tq_get(macro_tickers, get = "economic.data", 
                   from = min(df_daly$Date, to = max(df_daly$Date))) %>%
  pivot_wider(names_from = symbol, values_from = price) %>%
  rename(
    OIL_WTI = DCOILWTICO,
    GAS_HHUB = DHHNGSP,
    USD_INDEX = DTWEXBGS,
    VIX = VIXCLS,
    YIELD_10Y = DGS10
  ) %>%
  # Fill missing weekends/holidays (Last Observation Carried Forward)
  fill(everything(), .direction = "down")

head(df_macro)
# Note: For Carbon Credits (EUA), Yahoo Finance "KEUA" or "EUA.L" is best

# Yahoo Finance Data Download (Carbon, Brent, MSCI, ...) 
yahoo_tickers <- c("KEUA", "GC=F", "URTH", "MME=F", "^GSPC", "QCLN", "1898.HK")  #"single stock MSCI", "CL=F", 
master_dates <- df_daly %>%
  select(Date) %>%
  rename(date = Date) %>%
  distinct() # Safety check to remove duplicates if any exist


df_yahoo <- tq_get(yahoo_tickers, get = "stock.prices", 
                       from = min(df_daly$Date, to = max(df_daly$Date)))

df_yahoo <- df_yahoo %>%
  select(date, symbol, adjusted) %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  
  # CRITICAL STEP: Enforce all dates exist. 
  # This adds rows for weekends, holidays, and early years where Yahoo might be empty.
  right_join(master_dates, by = "date") %>% 
  
  # Rename columns (matches your snippet)
  rename(
    C_EUA = KEUA,
    #OIL_BRENT = `CL=F`, #instead of that we have WTI 
    GOLD_FUT = `GC=F`,
    MSCI_ETF = URTH, 
    MSCI_EM = `MME=F`,
    SP500 = `^GSPC`,
    QCLN = QCLN, 
    COAL = "1898.HK"
  ) %>%
  
  # Sort to be safe
  arrange(date)

head(df_yahoo)

# check the exact number of rows 
cat("Rows in df_daly:", nrow(df_daly), "\n")
cat("Rows in df_yahoo:", nrow(df_yahoo), "\n")
sum(is.na(df_yahoo$C_EUA))

cols_to_exclude <- c("C_EUA")

df_merged_raw <- df_daly %>%
  # Join Macro (FRED)
  left_join(df_macro, by = c("Date" = "date")) %>%
  # Join Yahoo (Market)
  left_join(df_yahoo, by = c("Date" = "date")) %>%
  # Remove the problematic columns immediately
  select(-any_of(cols_to_exclude)) %>%
  arrange(Date)

summary(df_merged_raw)

vars_to_interpolate <- df_merged_raw %>%
  select(where(is.numeric)) %>%
  names()

cat("Interpolating the following variables:\n")
print(vars_to_interpolate)

df_final <- df_merged_raw %>%
  # Interpolate all numeric columns
  mutate(across(all_of(vars_to_interpolate), 
                ~ na.spline(., x = Date, method = "natural", na.rm = FALSE))) %>%
  # Safety floor for prices (prevent negatives from spline overshooting)
  mutate(across(any_of(c("Oil_WTI", "Oil_Brent", "VIX", "Gas_HenryHub")), 
                ~ if_else(. < 0.01, 0.01, .)))
summary(df_final)

#----ML----
##---- join datasets ----
df_master <- df_final %>%
  left_join(
    df_final_dataset %>% select(Date, ends_with("_dummy")), 
    by = "Date"
  ) %>%
  drop_na() # Remove rows where we might miss dummies or macro data

print(colnames(df_master))
summary(df_master)
saveRDS(df_master, "R/df_master.rds")

##---- Feature Engineering (The "Composite Market Dynamics" Logic)----
#prepare_ml_data_old <- function(target_metal, target_col_name, df_master) {
  
  # 1. Identify the Target Column (Bubble Flag)
  # We assume the bubble dummy column is named like "CODALY_dummy"
  target_dummy <- paste0(target_col_name, "_dummy")
  
  if(!target_dummy %in% names(df_master)) {
    stop(paste("Target dummy column not found:", target_dummy))
  }
  
  # 2. Identify Predictor Columns (Price Levels -> Returns)
  # We exclude Date and Dummies from the transformation features
  # This logic automatically picks up ALL your new macro variables
  predictors_raw <- df_master %>%
    select(where(is.numeric)) %>%
    select(-ends_with("dummy")) %>%
    names()
  
  # 3. Generate Features (Returns, Volatility, Lags)
  # We calculate Log-Returns and 10-day Volatility for every numeric series
  df_features <- df_master %>%
    arrange(Date) %>%
    mutate(
      across(all_of(predictors_raw), 
             list(
               Ret = ~ c(NA, diff(log(.))),                # Log-Return
               Vol = ~ runSD(c(NA, diff(log(.))), n = 10)  # 10-day Rolling Volatility
             ),
             .names = "{.col}_{.fn}") # e.g., Oil_WTI_Ret, Oil_WTI_Vol
    )
  
  # 4. Construct the Final Model Dataset (X -> y)
  df_model <- df_features %>%
    mutate(
      # Define Target (y)
      Target_Bubble = .data[[target_dummy]],
      
      # --- PREDICTORS (X) ---
      # CRITICAL: We lag ALL features by 1 day to avoid look-ahead bias.
      # We are predicting "Tomorrow's Bubble" using "Today's Market Data".
      across(ends_with("_Ret"), ~ lag(.), .names = "{.col}_Lag1"),
      across(ends_with("_Vol"), ~ lag(.), .names = "{.col}_Lag1"),
      
      # We also include lagged LEVELS of macro variables (sometimes levels matter, e.g., VIX > 30)
      across(all_of(predictors_raw), ~ lag(.), .names = "{.col}_Level_Lag1")
    ) %>%
    # Select only the Target, Date, and the new Lagged Features
    select(Date, Target_Bubble, ends_with("Lag1")) %>%
    # Remove initial rows with NAs caused by lags/rolling windows
    drop_na()
  
  return(df_model)
}

###----new function inlcuding DV of other metals to be predictors ----

prepare_ml_data <- function(target_metal, target_col_name, df_master) {
  
  target_dummy <- paste0(target_col_name, "_dummy")
  
  if(!target_dummy %in% names(df_master)) {
    stop(paste("Error: Column", target_dummy, "not found in df_master"))
  }
  
  # A. Identyfikacja zmiennych
  predictors_numeric <- df_master %>%
    select(where(is.numeric)) %>%
    select(-ends_with("dummy")) %>%
    select(-any_of(c("Date", "date", "DATE"))) %>% 
    names()
  
  peer_dummies <- df_master %>%
    select(ends_with("dummy")) %>%
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
    
    # --- TU JEST ZMIANA ---
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
saveRDS(ml_results, "R/RF_results_complete.rds")
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

##---- Balanced Accuracy----


# --- 1. Initialize Storage ---
performance_table <- data.frame(
  Metal = character(),
  Balanced_Accuracy = numeric(),
  Sensitivity = numeric(), 
  Specificity = numeric(), 
  Precision = numeric(),
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
        Bubbles_Detected = bubbles_str
      ))
    }
  }
}

# --- 3. Convert to LaTeX using xtable ---

# Rename columns for the paper (cleaner headers)
colnames(performance_table) <- c("Metal", "Bal. Accuracy", "Sensitivity", "Specificity", "Precision", "Bubbles (Found/Total)")

# Create the xtable object
latex_obj <- xtable(performance_table, 
                    caption = "Random Forest Model Performance Metrics", 
                    label = "tab:rf_performance",
                    digits = 3,      # Number of decimal places
                    align = "lcccccc") # Alignment: Left, Center, Center...

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