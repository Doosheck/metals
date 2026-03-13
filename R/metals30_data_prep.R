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

### ---- Define Selection Function based on the length of series ----

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

### ---- Alternative approach: Select Series by Source Pattern ----
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
### ---- Execution Examples ----

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
saveRDS(df_daly, "R/df_daly_raw.rds")
write.csv2(df_daly, "R/df_daly_raw.csv")

##---- Bubble detection:----
###---- Procedure for 4 series----
# 1. Filter and Log-Transform
df_filtered <- df_daly %>% 
  filter(Date >= as.Date("2017-11-01")) %>%
  mutate(across(-Date, log))

data_matrix <- as.data.frame(df_filtered[,-1])
rownames(data_matrix) <- as.character(df_filtered$Date)

# 2. GSADF Calculation
est_results <- radf(data_matrix) #, minw = 105
mc_cv <- radf_mc_cv(n = nrow(data_matrix), seed = 123) #minw = 105, 
summary(est_results, cv = mc_cv) #show critical values:
saveRDS(mc_cv, "mc_cv_bubble_df_daly_20171101.rds")
#mc_cv <- readRDS("mc_cv_bubble_df_daly.rds")

# 3. Extract Dates (Significance 5%)
bubble_dates <- datestamp(est_results, cv = mc_cv, p = 0.05)

# 4. Create Dummies (Shortened logic)
# We initialize a matrix of 0s matching the filtered data size
series_names <- names(data_matrix)
dummy_matrix <- matrix(0L, nrow = nrow(df_filtered), ncol = length(series_names))
colnames(dummy_matrix) <- paste0(series_names, "_BD")

for (series in series_names) {
  periods <- bubble_dates[[series]]
  if (!is.null(periods) && nrow(periods) > 0) {
    for (i in 1:nrow(periods)) {
      # exuber returns indices relative to the input data_matrix
      dummy_matrix[periods[i, "Start"]:periods[i, "End"], paste0(series, "_BD")] <- 1L
    }
  }
}

# 5. Combine and SAVE ONLY THE NEW DATA
df_final_dataset <- bind_cols(df_filtered, as_tibble(dummy_matrix))
# Use a clear name to avoid loading old files
saveRDS(df_final_dataset, "R/data_R/bubble_results_new_baseline.rds")

#df_final_dataset <- readRDS("R/data_R/bubble_dummies_df_daly.rds")
#count bubbles in LIDALY:
sum(df_final_dataset$LIDALY_BD)/nrow(df_final_dataset)


### ----Plot series with bubbles----

metal_map <- list(
  "Cobalt"  = c(price = "CODALY", dummy = "CODALY_BD"),
  "Copper"  = c(price = "CUDALY", dummy = "CUDALY_BD"),
  "Lithium" = c(price = "LIDALY", dummy = "LIDALY_BD"),
  "Nickel"  = c(price = "NIDALY", dummy = "NIDALY_BD")
)

# Create a folder to save plots (if it doesn't exist)
# if(!dir.exists("R/plots_timeline")) dir.create("R/plots_timeline")

get_bubble_rects <- function(dates, dummy_col) {
  
  if(is.null(dummy_col)) return(NULL)
  dummy_col_clean <- as.numeric(as.character(unlist(dummy_col)))
  
  # Identify changes (0->1 or 1->0) to find blocks
  # We use rle (Run Length Encoding) to find consecutive runs of 1s
  runs <- rle(dummy_col)
  
  # Calculate end positions of each run
  end_pos <- cumsum(runs$lengths)
  # Calculate start positions
  start_pos <- c(1, head(end_pos, -1) + 1)
  
  # Filter only the runs where value is 1 (Bubble)
  bubble_indices <- which(runs$values == 1)
  
  if(length(bubble_indices) == 0) return(NULL)
  
  # Create a dataframe of start and end dates for rectangles
  rects <- data.frame(
    xmin = dates[start_pos[bubble_indices]],
    xmax = dates[end_pos[bubble_indices]]
  )
  
  # Tiny fix: If a bubble is 1 day long, xmin equals xmax. 
  # To make it visible, we can add 1 day to xmax.
  rects$xmax <- rects$xmax + days(1)
  
  return(rects)
}

plot_list <- list()

for (metal_name in names(metal_map)) {
  
  # A. Setup Variables
  cols <- metal_map[[metal_name]]
  col_price <- cols["price"]
  col_dummy <- cols["dummy"]
  
  # B. Get Rectangles for Shading
  # We extract the date and dummy column to find the intervals
  rect_data <- get_bubble_rects(df_final_dataset$Date, df_final_dataset[[col_dummy]])
  
  # C. Create Plot
  p <- ggplot() +
    
    # Layer 1: Pink Shaded Regions (Must be first to be in background)
    # We use -Inf and Inf for ymin/ymax so the band covers the whole height
    {if(!is.null(rect_data)) 
      geom_rect(data = rect_data, 
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "purple3", alpha = 0.2) # Alpha 0.2 makes it light pink
    } +
    
    # Layer 2: The Price Line (Black)
    geom_line(data = df_final_dataset, aes(x = Date, y = .data[[col_price]]), 
              color = "black", linewidth = 0.6) +
    
    # Layer 3: Styling
    labs(
      title = metal_name, # Simple title like in your image
      y = NULL,           # Removing Y label to match your image style
      x = NULL            # Removing X label to match your image style
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5), # Centered Bold Title
      panel.grid.minor = element_blank(), # Cleaner grid
      panel.grid.major.x = element_line(color = "grey90"), # Vertical grid lines
      axis.text = element_text(size = 12, color = "grey30")
    )
  
  plot_list[[metal_name]] <- p
  # D. Print and Save
  print(p)
  
  ggsave(
    filename = paste0("R/plots_timeline/Bubble_Shaded_", metal_name, ".png"), 
    plot = p, 
    width = 10, 
    height = 6, 
    dpi = 300,
    bg = "white"
  )
  
  cat(paste("Saved shaded plot for:", metal_name, "\n"))
}

library(patchwork) # Optional: Great for combining plots side-by-side
final_plot <- (plot_list[["Cobalt"]] + plot_list[["Lithium"]]) / 
  (plot_list[["Nickel"]] + plot_list[["Copper"]])
print(final_plot)
ggsave("R/plots_timeline/All_Metals_Bubbles201711.png", final_plot, width = 12, height = 8)



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
# HH=F: Natural Gas (Henry Hub) - strange
# QCLN: First Trust NASDAQ Clean Edge Green Energy Index Fund ()
# Gold Apr 26 (GC=F) yahoo
# DE000A1EY8J4.SG (yahoo) Solactive Global Lithium Index Stuttgart EUR
# Solactive Solar Index GTR (DE000SL0EBG1.SG) Stuttgart EUR -little changes over time

# China Coal Energy Company Limited (1898.HK)
# Peabody Energy Corporation (BTU) NYSE starts in 2017 May




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
yahoo_tickers <- c("KEUA", "GC=F", "URTH", "MME=F", "^GSPC", "QCLN", "1898.HK", "BTU")  #"single stock MSCI", "CL=F", 
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
    C_CHR = "1898.HK",
    C_US = "BTU"
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


##---- Join datasets ----

df_master <- df_final %>%
  left_join(
    df_final_dataset %>% select(Date, ends_with("_dummy")), 
    by = "Date"
  ) %>%
  drop_na() # Remove rows where we might miss dummies or macro data

print(colnames(df_master))
df_master <- df_master %>%
  rename_with(~ gsub("_dummy", "_BD", .x), ends_with("_dummy"))

# Verify new names
print(names(df_master))
summary(df_master)
saveRDS(df_master, "R/df_master.rds")
df_master <- readRDS("R/df_master.rds")
write.csv2(df_master, "R/df_master.csv")


df_master_plot <- df_master %>%
  mutate(across(
    .cols = where(is.numeric) & !ends_with("dummy") & !ends_with("DB"), 
    .fns = ~ ifelse(. <= 0, 0.001, .)
  ))

# Verification: Check if any zeros/negatives remain in the price columns
summary(select(df_master_plot, where(is.numeric), -ends_with("BD")))
cat("Data cleaning complete. All non-positive values replaced with 0.001.\n")

###---- Plots of series ----

vars_to_plot <- df_master_plot %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("Date", "date"))) %>%
  select(-ends_with("dummy")) %>%  # Standard exclusion
  select(-ends_with("BD")) %>%     # Just in case you renamed them
  names()

cat(paste("Plotting", length(vars_to_plot), "series in a grid...\n"))

# 2. Reshape to Long Format
df_long <- df_master_plot %>%
  select(Date, all_of(vars_to_plot)) %>%
  pivot_longer(
    cols = -Date, 
    names_to = "Series", 
    values_to = "Value"
  ) %>%
  mutate(Series = factor(Series, levels = vars_to_plot))

# 3. Generate Plot
p_grid <- ggplot(df_long, aes(x = Date, y = Value)) +
  # Thin dark line (standard academic style)
  geom_line(color = "black", linewidth = 0.3) +
  
  # Log Scale handling (Safety for 0 or negative values like Oil)
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10)) +
  
  # 4x4 Grid
  facet_wrap(~ Series, ncol = 4, scales = "free_y") +
  
  # Styling
  labs(x = NULL, y = NULL) + # Clean look
  theme_minimal() + 
  theme(
    # REMOVE GREY BOXES (The "Strip")
    strip.background = element_blank(),
    
    # Make titles bold and left-aligned (or centered)
    strip.text = element_text(face = "bold", size = 10, hjust = 0.5, color = "black"),
    
    # Axis text small but readable
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7, color = "grey40"),
    axis.text.y = element_text(size = 7, color = "grey40"),
    
    # Remove minor grid lines for clarity
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.2),
    
    # Add a border around the panels (optional, but often looks tidy)
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
  )
# 4. Save (Square dimensions work best for 4x4)
ggsave("R/All_Series_Grid_4x4.png", p_grid, width = 12, height = 10, dpi = 300, bg="white")

print(p_grid)