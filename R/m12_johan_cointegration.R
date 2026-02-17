install.packages("vars")
# Load libraries
library(vars)
library(urca)
library(here)
library(tidyverse)
library(kableExtra)



#---- Single metal example ----


# --- 1. Filter Cobalt Series ---
df_final <- read_csv(here("data", "combined_metals_cleaned.csv"), 
  show_col_types = FALSE, 
  guess_max = 10000)

# Adjust the pattern "CO" to match your specific Cobalt column names
cobalt_data <- df_final %>%
  dplyr::select(CODALY, COLMEX, COLMEA, COWUXI) %>% 
  mutate(across(everything(), ~ log(.x))) %>%
  as.matrix()

head(cobalt_data)
dim(cobalt_data)

df_dummies <- read_csv(here("data", "bubble_dummies.csv"), 
                     show_col_types = FALSE, 
                     guess_max = 10000) 
# Prepare the Dummy Matrix 
cobalt_dummies <- df_dummies %>%
  dplyr::select(CODALY, COLMEX, COLMEA, COWUXI) %>%
  rename_with(~ paste0(.x, "_DV")) %>%
  # Ensure no rows have NA values
  drop_na() %>%
  as.matrix()

head(cobalt_dummies)
dim(cobalt_dummies)

# --- 2. Lag Selection ---
# Cointegration tests are highly sensitive to the lag structure
# We include the dummies in the lag selection to ensure the optimal lag 
# isn't being skewed by the bubble variance.

cobalt_lags <- VARselect(cobalt_data, lag.max = 10, type = "const")
best_lag <- cobalt_lags$selection["AIC(n)"]

cobalt_lags_ex <- VARselect(cobalt_data, 
                         lag.max = 10, 
                         type = "const", 
                         exogen = cobalt_dummies)

best_lag_ex <- cobalt_lags$selection["AIC(n)"]
# no exogenous variables

best_lag

# --- 3. Johansen Test (Trace) ---
# K is the number of lags. ecdet="const" allows for a drift in the price levels

jo_cobalt <- ca.jo(cobalt_data, 
                   type = "trace", 
                   ecdet = "const", 
                   K = best_lag)

jo_cobalt_ex <- ca.jo(cobalt_data, 
                   type = "trace", 
                   ecdet = "const", 
                   K = best_lag,
                   dumvar = cobalt_dummies)
# --- 4. Interpretation ---
summary(jo_cobalt)
summary(jo_cobalt_ex)

#---- Four metals and function Sonet 4.5----

library(vars)
library(urca)
library(here)
library(tidyverse)
library(kableExtra)

# --- Read Data Once ---
df_final <- read_csv(here("data", "combined_metals_cleaned.csv"), 
                     show_col_types = FALSE, 
                     guess_max = 10000)

df_dummies <- read_csv(here("data", "bubble_dummies.csv"), 
                       show_col_types = FALSE, 
                       guess_max = 10000)

# --- Function to Run Analysis ---
run_metal_analysis <- function(metal_name, columns) {
  
  message("\n=== Analyzing: ", metal_name, " ===")
  
  tryCatch({
    # Prepare Price Matrix (Logged)
    metal_data <- df_final %>%
      dplyr::select(all_of(columns)) %>% 
      mutate(across(everything(), ~ log(.x))) %>%
      drop_na() %>%
      as.matrix()
    
    # Prepare Dummy Matrix
    metal_dummies <- df_dummies %>%
      dplyr::select(all_of(columns)) %>%
      rename_with(~ paste0(.x, "_DV")) %>%
      drop_na() %>%
      as.matrix()
    
    # Ensure same number of rows
    min_rows <- min(nrow(metal_data), nrow(metal_dummies))
    metal_data <- metal_data[1:min_rows, ]
    metal_dummies <- metal_dummies[1:min_rows, ]
    
    message("  Data dimensions: ", nrow(metal_data), " x ", ncol(metal_data))
    
    # --- CRITICAL: Remove constant dummy columns (all zeros = no bubbles) ---
    dummy_vars <- apply(metal_dummies, 2, var, na.rm = TRUE)
    constant_cols <- which(dummy_vars < 1e-10)
    
    if (length(constant_cols) > 0) {
      message("  Removing ", length(constant_cols), " constant dummy column(s): ", 
              paste(colnames(metal_dummies)[constant_cols], collapse = ", "))
      metal_dummies <- metal_dummies[, -constant_cols, drop = FALSE]
    }
    
    # Check if we have any dummies left
    has_dummies <- ncol(metal_dummies) > 0
    
    if (!has_dummies) {
      message("  WARNING: No bubbles detected in any series - skipping exogenous model")
    } else {
      message("  Using ", ncol(metal_dummies), " bubble dummy variable(s)")
    }
    
    # --- Lag Selection (Without Exogenous) ---
    message("  Running lag selection (basic)...")
    lags_basic <- VARselect(metal_data, lag.max = 10, type = "const")
    best_lag_basic <- lags_basic$selection["AIC(n)"]
    
    # --- Lag Selection (With Exogenous) - only if we have dummies ---
    if (has_dummies) {
      message("  Running lag selection (with exogenous)...")
      lags_ex <- VARselect(metal_data, 
                           lag.max = 10, 
                           type = "const", 
                           exogen = metal_dummies)
      best_lag_ex <- lags_ex$selection["AIC(n)"]
    } else {
      best_lag_ex <- best_lag_basic
    }
    
    # --- Johansen Test (Basic - No Exogenous) ---
    message("  Running Johansen test (basic)...")
    jo_basic <- ca.jo(metal_data, 
                      type = "trace", 
                      ecdet = "const", 
                      K = best_lag_basic)
    
    # --- Johansen Test (With Exogenous) - only if we have dummies ---
    if (has_dummies) {
      message("  Running Johansen test (with exogenous)...")
      jo_ex <- ca.jo(metal_data, 
                     type = "trace", 
                     ecdet = "const", 
                     K = best_lag_ex,
                     dumvar = metal_dummies)
    } else {
      jo_ex <- NULL
    }
    
    # --- Extract Results ---
    # Basic model
    trace_stats_basic <- jo_basic@teststat
    crit_vals_basic <- jo_basic@cval[, 2]  # 5% critical values
    r_basic <- sum(trace_stats_basic > crit_vals_basic)
    
    # Model with exogenous (if we have bubbles)
    if (!is.null(jo_ex)) {
      trace_stats_ex <- jo_ex@teststat
      crit_vals_ex <- jo_ex@cval[, 2]
      r_ex <- sum(trace_stats_ex > crit_vals_ex)
    } else {
      trace_stats_ex <- rep(NA, length(trace_stats_basic))
      crit_vals_ex <- rep(NA, length(crit_vals_basic))
      r_ex <- NA
    }
    
    # Create results tibble
    results <- tibble(
      Metal = metal_name,
      N_Series = ncol(metal_data),
      N_Obs = nrow(metal_data),
      N_Bubble_Vars = if(has_dummies) ncol(metal_dummies) else 0,
      
      # Basic Model
      Lag_Basic = best_lag_basic,
      Rank_Basic = r_basic,
      Trace_r0_Basic = trace_stats_basic[length(trace_stats_basic)],
      Crit_5pct_r0_Basic = crit_vals_basic[length(crit_vals_basic)],
      Trace_r1_Basic = if(length(trace_stats_basic) > 1) trace_stats_basic[length(trace_stats_basic) - 1] else NA,
      Crit_5pct_r1_Basic = if(length(crit_vals_basic) > 1) crit_vals_basic[length(crit_vals_basic) - 1] else NA,
      
      # Model with Exogenous
      Lag_Ex = best_lag_ex,
      Rank_Ex = r_ex,
      Trace_r0_Ex = if(!is.null(jo_ex)) trace_stats_ex[length(trace_stats_ex)] else NA,
      Crit_5pct_r0_Ex = if(!is.null(jo_ex)) crit_vals_ex[length(crit_vals_ex)] else NA,
      Trace_r1_Ex = if(!is.null(jo_ex) && length(trace_stats_ex) > 1) trace_stats_ex[length(trace_stats_ex) - 1] else NA,
      Crit_5pct_r1_Ex = if(!is.null(jo_ex) && length(crit_vals_ex) > 1) crit_vals_ex[length(crit_vals_ex) - 1] else NA,
      
      Status = if(!has_dummies) "No bubbles - Basic only" else "Complete"
    )
    
    message("  ✓ Analysis complete for ", metal_name)
    
    # Store the full results
    list(
      results = results,
      jo_basic = jo_basic,
      jo_ex = jo_ex,
      data = metal_data,
      dummies = if(has_dummies) metal_dummies else NULL,
      n_bubbles_removed = length(constant_cols)
    )
    
  }, error = function(e) {
    message("  ✗ ERROR in ", metal_name, ": ", e$message)
    
    # Return error results
    list(
      results = tibble(
        Metal = metal_name,
        N_Series = length(columns),
        N_Obs = NA,
        N_Bubble_Vars = NA,
        Lag_Basic = NA,
        Rank_Basic = NA,
        Trace_r0_Basic = NA,
        Crit_5pct_r0_Basic = NA,
        Trace_r1_Basic = NA,
        Crit_5pct_r1_Basic = NA,
        Lag_Ex = NA,
        Rank_Ex = NA,
        Trace_r0_Ex = NA,
        Crit_5pct_r0_Ex = NA,
        Trace_r1_Ex = NA,
        Crit_5pct_r1_Ex = NA,
        Status = paste("ERROR:", e$message)
      ),
      jo_basic = NULL,
      jo_ex = NULL,
      data = NULL,
      dummies = NULL,
      n_bubbles_removed = NA
    )
  })
}

# --- Metal Configurations ---
metal_configs <- list(
  "Cobalt" = c("CODALY", "COLMEX", "COLMEA", "COWUXI"),
  "Copper" = c("CUDALY", "CUCOMX", "CULMEX", "CUSMMG", "CUSHFE"), 
  "Lithium" = c("LIDALY", "LISAME", "LICOMX", "LILMEX"),
  "Nickel" = c("NIDALY", "NILMEX", "NISHFE", "NIWUXI", "NIINDA")
)

# --- Run Analysis for All Metals ---
all_results <- map(names(metal_configs), function(metal_name) {
  run_metal_analysis(metal_name, metal_configs[[metal_name]])
})

# Name the list elements
names(all_results) <- names(metal_configs)

# --- Extract Summary Table ---
summary_table <- map_dfr(all_results, ~ .x$results)

print(summary_table)

# --- Summary of bubble detection ---
message("\n=== Bubble Detection Summary ===")
for (metal in names(all_results)) {
  n_removed <- all_results[[metal]]$n_bubbles_removed
  n_used <- all_results[[metal]]$results$N_Bubble_Vars
  message(metal, ": ", n_used, " bubble variables used, ", 
          ifelse(is.na(n_removed), 0, n_removed), " removed (no bubbles)")
}

# --- Export Results ---
#write_csv(summary_table, here("results", "johansen_results_all_metals.csv"))

# --- Optional: View specific results ---
# summary(all_results$Cobalt$jo_basic)
# summary(all_results$Copper$jo_ex)

simple_latex <- latex_table_data %>%
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = "Johansen Cointegration Tests and Price Discovery",
      label = "tab:johansen_results")
cat(simple_latex)

##--- Function to Extract Full Results Including All Trace Stats ----
extract_full_results <- function(metal_name, jo_obj, model_name, price_data) {

  if (is.null(jo_obj)) {
    return(NULL)
  }

  # Extract trace statistics and critical values
  trace_stats <- jo_obj@teststat
  crit_vals_10 <- jo_obj@cval[, 3]  # 10%
  crit_vals_5 <- jo_obj@cval[, 2]   # 5%
  crit_vals_1 <- jo_obj@cval[, 1]   # 1%

  r <- sum(trace_stats > crit_vals_5)

  # Safe rank for VECM estimation
  safe_r <- max(1, min(r, ncol(price_data) - 1))

  # Estimate VECM to get alpha (adjustment speeds)
  vecm <- cajorls(jo_obj, r = safe_r)

  # Extract alpha values
  if (is.matrix(vecm$rlm$coefficients)) {
    alpha_vals <- vecm$rlm$coefficients[grep("^ect", rownames(vecm$rlm$coefficients)), ]
    if (is.matrix(alpha_vals)) {
      alpha_vals <- alpha_vals[, 1]
    }
  } else {
    alpha_vals <- vecm$rlm$coefficients[grep("^ect", names(vecm$rlm$coefficients))]
  }

  # Get exchange names
  exchange_names <- colnames(price_data)

  # Find leader and follower
  abs_alphas <- abs(alpha_vals)
  leader_idx <- which.min(abs_alphas)
  follower_idx <- which.max(abs_alphas)

  leader_name <- exchange_names[leader_idx]
  follower_name <- exchange_names[follower_idx]
  leader_alpha <- alpha_vals[leader_idx]
  follower_alpha <- alpha_vals[follower_idx]

  # Function to add significance stars
  add_stars <- function(stat, crit_10, crit_5, crit_1) {
    if (stat > crit_1) return(paste0(sprintf("%.2f", stat), "***"))
    if (stat > crit_5) return(paste0(sprintf("%.2f", stat), "**"))
    if (stat > crit_10) return(paste0(sprintf("%.2f", stat), "*"))
    return(sprintf("%.2f", stat))
  }

  # Format all trace statistics
  n_tests <- length(trace_stats)
  trace_formatted <- character(n_tests)

  for (i in 1:n_tests) {
    trace_formatted[i] <- add_stars(trace_stats[n_tests - i + 1],
                                    crit_vals_10[n_tests - i + 1],
                                    crit_vals_5[n_tests - i + 1],
                                    crit_vals_1[n_tests - i + 1])
  }

  # Create result tibble with all trace stats
  result <- tibble(
    Metal = metal_name,
    Model = model_name,
    Rank = r
  )

  # Add trace columns dynamically based on number of series
  for (i in 0:(n_tests - 1)) {
    col_name <- if (i == 0) "r=0" else paste0("r≤", i)
    result[[col_name]] <- trace_formatted[i + 1]
  }

  # Add alpha values WITHOUT exchange names inline
  result$`Max α` <- sprintf("%.3f", follower_alpha)
  result$Follower <- follower_name
  result$`Min α` <- sprintf("%.3f", leader_alpha)
  result$Leader <- leader_name

  result
}
# 
# # --- Extract Results for All Metals ---
latex_table_data <- map_dfr(names(all_results), function(metal) {

  res <- all_results[[metal]]

  # Basic model
  basic_row <- extract_full_results(
    metal_name = toupper(metal),
    jo_obj = res$jo_basic,
    model_name = "Raw",
    price_data = res$data
  )

  # Exogenous model (if available)
  if (!is.null(res$jo_ex)) {
    ex_row <- extract_full_results(
      metal_name = "",
      jo_obj = res$jo_ex,
      model_name = "Filtered",
      price_data = res$data
    )
    bind_rows(basic_row, ex_row)
  } else {
    basic_row
  }
})

# --- Display preview ---
print(latex_table_data)

#----Gemini 2 ----

trace_cols <- names(latex_table_data)[grepl("^r", names(latex_table_data))]

# --- main rows---
main_rows <- latex_table_data %>%
  mutate(
    Metal = ifelse(Metal == "", Model, Metal),
    # Główne wartości Alpha
    Follower_Disp = sprintf("%.3f", as.numeric(`Max α`)),
    Leader_Disp = sprintf("%.3f", as.numeric(`Min α`))
  ) %>%
  # Wybieramy kolumny w konkretnej kolejności
  dplyr::select(Metal, Rank, all_of(trace_cols), Follower_Disp, Leader_Disp) %>%
  mutate(across(everything(), as.character))

# --- proxy rows ---
name_rows <- latex_table_data %>%
  mutate(
    Metal = "",
    Rank = "",
    Follower_Disp = Follower,
    Leader_Disp = Leader
  ) %>%
  mutate(across(all_of(trace_cols), ~ "")) %>% #fill empty cols with ""
  dplyr::select(Metal, Rank, all_of(trace_cols), Follower_Disp, Leader_Disp) %>%
  mutate(across(everything(), as.character))

# --- mix of rows ---
final_table <- map_dfr(1:nrow(main_rows), ~ {
  bind_rows(main_rows[.x, ], name_rows[.x, ])
})

trace_cols_names <- names(final_table)[grepl("^r", names(final_table))]

latex_output_final <- final_table %>%
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      align = c("l", "c", rep("c", length(trace_cols_names)), "c", "c"),
      # rename cols:
      col.names = c("Metal", "$r$", trace_cols_names, "Follower", "Leader"),
      linesep = c("", "\\addlinespace"), 
      caption = "Johansen Cointegration Tests and Price Discovery",
      label = "tab:johansen") %>% 
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  column_spec(1, bold = TRUE, width = "2.5cm") %>%
  # dynamic width of two last cols (Follower/Leader)
  column_spec((ncol(final_table)-1):ncol(final_table), width = "2.2cm") %>%
  add_header_above(c(" " = 2, 
                   "Trace Statistics" = length(trace_cols_names), 
                   "Max $|\\alpha|$" = 1, 
                   "Min $|\\alpha|$" = 1))


cat(latex_output_final)
writeLines(latex_output_final, here(\"outputs\", \"tables\", \"johansen_results_final.tex\"))
