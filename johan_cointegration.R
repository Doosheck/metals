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

#---- Four metals and function----
# --- 1. Load Data if necessary---

#df_final <- read_csv(here("data", "combined_metals_cleaned.csv"), show_col_types = FALSE)
#df_dummies <- read_csv(here("data", "bubble_dummies.csv"), show_col_types = FALSE)

# --- 2. Define Analysis Function ---

run_metal_analysis <- function(metal_name, columns) {
  
  # Prepare Price Matrix (Logged)
  price_mat <- df_final %>%
    dplyr::select(all_of(columns)) %>%
    mutate(across(everything(), log)) %>%
    drop_na() %>%
    as.matrix()
  
  # Prepare Dummy Matrix
  dummy_mat <- df_dummies %>%
    dplyr::select(all_of(columns)) %>%
    rename_with(~ paste0(.x, "_DV")) %>%
    drop_na() %>%
    as.matrix()
  
  # Scenarios
  scenarios <- list(
    "Basic" = NULL,
    "With Bubbles" = dummy_mat
  )
  
  map_dfr(names(scenarios), function(s_name) {
    ex_data <- scenarios[[s_name]]
    
    # 1. Lag Selection
    lags <- VARselect(price_mat, lag.max = 10, type = "const", exogen = ex_data)
    k_opt <- lags$selection["AIC(n)"]
    
    # 2. Johansen Test
    jo <- ca.jo(price_mat, type = "trace", ecdet = "const", K = k_opt, dumvar = ex_data)
    
    # 3. Dynamic Extraction of Stats and Critical Values
    # jo@teststat is a named vector. We need to compare it to the 5% column.
    t_stats <- jo@teststat
    c_vals  <- jo@cval[, "5%"]
    
    # The order in ca.jo is usually r <= n-1, r <= n-2 ... r = 0 
    # BUT the teststat vector is stored in reverse order of the hypothesis:
    # Index 1 is often the smallest r, Index N is the largest.
    # To be safe, we match by name or use tail/head logic.
    
    # Identify Rank r: Count how many tests reject the Null
    r_rank <- sum(t_stats > c_vals)
    
    # 4. Extract Alpha (Adjustment Speeds)
    # VECM rank must be: 0 < r < Number of Variables
    safe_r <- max(1, min(r_rank, ncol(price_mat) - 1))
    vecm <- cajorls(jo, r = safe_r)
    
    # Names of exchanges to identify Leader/Follower
    exchange_names <- colnames(price_mat)
    alpha_vals     <- vecm$alpha[, 1]
    abs_alphas     <- abs(alpha_vals)
    
    # Identify names
    leader_name   <- exchange_names[which.min(abs_alphas)]
    follower_name <- exchange_names[which.max(abs_alphas)]
    
    tibble(
      Metal = metal_name,
      Model = s_name,
      Rank  = r_rank,
      Trace_r0 = t_stats[length(t_stats)], # Typically the test for r=0
      Trace_r1 = ifelse(length(t_stats) > 1, t_stats[length(t_stats) - 1], NA),
      Leader   = leader_name,
      Follower = follower_name,
      Max_Alpha = max(abs_alphas),
      Min_Alpha = min(abs_alphas)
    )
  })
}



# --- 3. Execute for All Metals ---
# Define your metals and their respective columns here
metal_configs <- list(
  "Cobalt" = c("CODALY", "COLMEX", "COLMEA", "COWUXI"),
  "Copper" = c("CUDALY", "CUCOMX", "CULMEX", "CUSMMG", "CUSHFE"), 
  "Lithium" = c("LIDALY", "LISAME", "LICOMX", "LILMEX"),
  "Nickel" = c("NIDALY", "NILMEX", "NISHFE", "NIWUXI", "NIINDA")
)

results_df <- map_dfr(names(metal_configs), ~run_metal_analysis(.x, metal_configs[[.x]]))

# --- 3. LaTeX Table ---
latex_table <- results_df %>%
  kbl(format = "latex", 
      booktabs = TRUE, 
      digits = 3,
      escape = FALSE,
      col.names = c("Metal", "Model", "$r$", "Trace ($r=0$)", "Trace ($r \\le 1$)", 
                    "Leader", "Follower", "Max $\\alpha$", "Min $\\alpha$"),
      caption = "Johansen Cointegration and Price Discovery") %>%
  collapse_rows(columns = 1, latex_hline = "major") %>%
  kable_styling(latex_options = "hold_position")

cat(latex_table)

#---- Sonet 4.5----

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
write_csv(summary_table, here("results", "johansen_results_all_metals.csv"))

# --- Optional: View specific results ---
# summary(all_results$Cobalt$jo_basic)
# summary(all_results$Copper$jo_ex)

# --- Function to Extract Full Results Including Alpha ---
extract_full_results <- function(metal_name, jo_obj, model_name, price_data) {
  
  if (is.null(jo_obj)) {
    return(NULL)
  }
  
  # Extract trace statistics and critical values
  trace_stats <- jo_obj@teststat
  crit_vals <- jo_obj@cval[, 2]  # 5% level
  r <- sum(trace_stats > crit_vals)
  
  # Safe rank for VECM estimation
  safe_r <- max(1, min(r, ncol(price_data) - 1))
  
  # Estimate VECM to get alpha (adjustment speeds)
  vecm <- cajorls(jo_obj, r = safe_r)
  
  # Extract alpha values (loading matrix)
  if (is.matrix(vecm$rlm$coefficients)) {
    # If multiple cointegrating vectors, take the first
    alpha_vals <- vecm$rlm$coefficients[grep("^ect", rownames(vecm$rlm$coefficients)), ]
    if (is.matrix(alpha_vals)) {
      alpha_vals <- alpha_vals[, 1]  # First equation's alphas
    }
  } else {
    alpha_vals <- vecm$rlm$coefficients[grep("^ect", names(vecm$rlm$coefficients))]
  }
  
  # Get exchange names
  exchange_names <- colnames(price_data)
  
  # Find leader (min |alpha|) and follower (max |alpha|)
  abs_alphas <- abs(alpha_vals)
  
  leader_idx <- which.min(abs_alphas)
  follower_idx <- which.max(abs_alphas)
  
  leader_name <- exchange_names[leader_idx]
  follower_name <- exchange_names[follower_idx]
  
  leader_alpha <- alpha_vals[leader_idx]
  follower_alpha <- alpha_vals[follower_idx]
  
  # Function to add significance stars
  add_stars <- function(stat, crit_10pct, crit_5pct, crit_1pct) {
    if (stat > crit_1pct) return(paste0(round(stat, 2), "***"))
    if (stat > crit_5pct) return(paste0(round(stat, 2), "**"))
    if (stat > crit_10pct) return(paste0(round(stat, 2), "*"))
    return(as.character(round(stat, 2)))
  }
  
  # Get critical values for significance testing
  crit_10pct <- jo_obj@cval[, 3]  # 10% level
  crit_5pct <- jo_obj@cval[, 2]   # 5% level
  crit_1pct <- jo_obj@cval[, 1]   # 1% level
  
  # Format trace statistics with stars
  trace_r0 <- add_stars(trace_stats[length(trace_stats)],
                        crit_10pct[length(crit_10pct)],
                        crit_5pct[length(crit_5pct)],
                        crit_1pct[length(crit_1pct)])
  
  trace_r1 <- if (length(trace_stats) > 1) {
    add_stars(trace_stats[length(trace_stats) - 1],
              crit_10pct[length(crit_10pct) - 1],
              crit_5pct[length(crit_5pct) - 1],
              crit_1pct[length(crit_1pct) - 1])
  } else {
    "—"
  }
  
  tibble(
    Metal = metal_name,
    Model = model_name,
    Rank = r,
    `Trace (r=0)` = trace_r0,
    `Trace (r≤1)` = trace_r1,
    `Max α (Follower)` = paste0(round(follower_alpha, 3), " (", follower_name, ")"),
    `Min α (Leader)` = paste0(round(leader_alpha, 3), " (", leader_name, ")")
  )
}

# --- Extract Results for All Metals ---
latex_table_data <- map_dfr(names(all_results), function(metal) {
  
  res <- all_results[[metal]]
  
  # Basic model
  basic_row <- extract_full_results(
    metal_name = toupper(metal),
    jo_obj = res$jo_basic,
    model_name = "Raw (No Dummies)",
    price_data = res$data
  )
  
  # Exogenous model (if available)
  if (!is.null(res$jo_ex)) {
    ex_row <- extract_full_results(
      metal_name = "",  # Empty for second row
      jo_obj = res$jo_ex,
      model_name = "Filtered (Bubbles)",
      price_data = res$data
    )
    bind_rows(basic_row, ex_row)
  } else {
    basic_row
  }
})

# --- Display the table ---
print(latex_table_data)

# --- Create LaTeX Table ---
latex_output <- latex_table_data %>%
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,  # Important: allows LaTeX symbols like * to render
      caption = "Johansen Cointegration Tests and Price Discovery",
      label = "tab:johansen_results",
      align = c("l", "l", "c", "c", "c", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  column_spec(1, bold = TRUE) %>%
  footnote(general = "Trace statistics reported for null hypotheses of r=0 and r≤1. Significance levels: *** p<0.01, ** p<0.05, * p<0.1. Max α identifies the follower (fastest adjustment), Min α identifies the leader (slowest adjustment). Exchange abbreviations in parentheses.",
           threeparttable = TRUE)

# --- Save LaTeX table ---
writeLines(latex_output, here("tables", "johansen_results.tex"))

# --- Also create a simple version without kableExtra formatting ---
simple_latex <- latex_table_data %>%
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = "Johansen Cointegration Tests and Price Discovery",
      label = "tab:johansen_results")

writeLines(simple_latex, here("tables", "johansen_results_simple.tex"))

message("\n=== LaTeX tables saved ===")
message("Full formatted version: tables/johansen_results.tex")
message("Simple version: tables/johansen_results_simple.tex")

# --- Preview in console ---
cat("\n=== Preview of LaTeX Table ===\n")
cat(simple_latex)

#---- Sonet 4.5 second approach ----
# --- Function to Extract Full Results Including All Trace Stats ---
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

# --- Extract Results for All Metals ---
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

# --- Create LaTeX Table with Line Breaks for Exchange Names ---
# We'll merge the alpha and exchange name columns using linebreak

latex_table_formatted <- latex_table_data %>%
  mutate(
    `Max α\\newline(Follower)` = paste0(`Max α`, "\\newline\\scriptsize ", Follower),
    `Min α\\newline(Leader)` = paste0(`Min α`, "\\newline\\scriptsize ", Leader)
  ) %>%
  select(-`Max α`, -Follower, -`Min α`, -Leader)

latex_output <- latex_table_formatted %>%
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = "Johansen Cointegration Tests and Price Discovery",
      label = "tab:johansen_results",
      align = c("l", "l", "c", rep("c", sum(grepl("^r", names(latex_table_formatted)))), "r", "r"),
      linesep = "") %>%  # Remove extra spacing between rows
  kable_styling(latex_options = c("hold_position", "scale_down", "striped"),
                font_size = 9) %>%
  column_spec(1, bold = TRUE, width = "1.2cm") %>%
  column_spec(2, width = "1.5cm") %>%
  footnote(general = c(
    "Trace statistics for sequential null hypotheses H$_0$: rank = r.",
    "Significance: *** p<0.01, ** p<0.05, * p<0.1.",
    "Max $\\\\alpha$ = fastest adjustment (follower); Min $\\\\alpha$ = slowest adjustment (leader)."
  ),
  threeparttable = TRUE,
  escape = FALSE)

# --- Save ---
writeLines(latex_output, here("tables", "johansen_results.tex"))

# --- Alternative: Put exchange names in footnotes ---
# This version is even more compact

latex_table_footnotes <- latex_table_data %>%
  group_by(Metal) %>%
  mutate(
    row_id = 1:n(),
    footnote_mark = letters[row_id]
  ) %>%
  ungroup() %>%
  mutate(
    Model = paste0(Model, "$^{", footnote_mark, "}$"),
    `Max α` = as.numeric(`Max α`),
    `Min α` = as.numeric(`Min α`)
  ) %>%
  select(Metal, Model, Rank, starts_with("r"), `Max α`, `Min α`, Follower, Leader, footnote_mark)

# Create footnote text
footnote_text <- latex_table_footnotes %>%
  mutate(note = paste0("$^{", footnote_mark, "}$Follower: ", Follower, "; Leader: ", Leader)) %>%
  pull(note) %>%
  unique()

latex_output_footnotes <- latex_table_footnotes %>%
  select(-Follower, -Leader, -footnote_mark) %>%
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = "Johansen Cointegration Tests and Price Discovery",
      label = "tab:johansen_results_footnotes",
      align = c("l", "l", "c", rep("c", sum(grepl("^r", names(.)))), "r", "r"),
      digits = 3,
      linesep = "") %>%
  kable_styling(latex_options = c("hold_position", "scale_down"),
                font_size = 9) %>%
  column_spec(1, bold = TRUE) %>%
  footnote(
    alphabet = footnote_text,
    general = c(
      "Trace statistics for sequential null hypotheses H$_0$: rank = r. Significance: *** p<0.01, ** p<0.05, * p<0.1.",
      "Max $\\\\alpha$ identifies follower (fastest adjustment); Min $\\\\alpha$ identifies leader (slowest adjustment)."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(latex_output_footnotes, here("tables", "johansen_results_footnotes.tex"))
print(latex_output_footnotes)

message("\n=== LaTeX tables saved ===")
message("Version 1 (linebreaks): tables/johansen_results.tex")
message("Version 2 (footnotes): tables/johansen_results_footnotes.tex")

# --- Preview ---
cat("\n=== Preview of Table Structure ===\n")
print(latex_table_formatted %>% select(1:9))

# --- Preview in console ---
cat("\n=== Preview of LaTeX Table ===\n")
cat(simple_latex)

#---- latex table new-----
# --- Extract Results for All Metals (same as before) ---
# --- Create readable table with proper formatting ---

latex_table_clean <- latex_table_data %>%
  mutate(
    `Follower` = paste0(`Max α`, " {\\scriptsize (", Follower, ")}"),
    `Leader` = paste0(`Min α`, " {\\scriptsize (", Leader, ")}")
  ) %>%
  select(-`Max α`, -`Min α`)

# Version 1: Standard table (no longtable, no full_width)
latex_output <- latex_table_clean %>%
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = "Johansen Cointegration Tests and Price Discovery",
      label = "tab:johansen",
      align = c("l", "l", "c", rep("c", sum(grepl("^r", names(latex_table_clean)))), "r", "r"),
      col.names = c("Metal", "Model", "Rank", 
                    names(latex_table_clean)[grepl("^r", names(latex_table_clean))],
                    "Max $\\alpha$ (Follower)", "Min $\\alpha$ (Leader)"),
      linesep = c("", "", "\\addlinespace")) %>%
  kable_styling(latex_options = c("hold_position"),
                font_size = 11) %>%
  column_spec(1, bold = TRUE, width = "1.5cm") %>%
  column_spec(2, width = "1.8cm") %>%
  footnote(general = "Trace statistics test sequential null hypotheses $H_0$: cointegration rank = $r$. Significance levels: $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$. Max $\\\\alpha$ identifies the follower exchange (fastest error correction), Min $\\\\alpha$ identifies the leader (slowest error correction). Exchange abbreviations in parentheses.",
           general_title = "Note:",
           escape = FALSE)

writeLines(latex_output, here("tables", "johansen_results.tex"))

# Version 2: With longtable (if you prefer)
latex_output_long <- latex_table_clean %>%
  kbl(format = "latex",
      booktabs = TRUE,
      longtable = TRUE,
      escape = FALSE,
      caption = "Johansen Cointegration Tests and Price Discovery",
      label = "tab:johansen_long",
      align = c("l", "l", "c", rep("c", sum(grepl("^r", names(latex_table_clean)))), "r", "r"),
      col.names = c("Metal", "Model", "Rank", 
                    names(latex_table_clean)[grepl("^r", names(latex_table_clean))],
                    "Max $\\alpha$ (Follower)", "Min $\\alpha$ (Leader)"),
      linesep = c("", "", "\\addlinespace")) %>%
  kable_styling(latex_options = c("repeat_header"),
                font_size = 11,
                full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  footnote(general = "Trace statistics test sequential null hypotheses $H_0$: cointegration rank = $r$. Significance levels: $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$. Max $\\\\alpha$ identifies the follower exchange (fastest error correction), Min $\\\\alpha$ identifies the leader (slowest error correction).",
           threeparttable = TRUE,
           general_title = "Note:",
           escape = FALSE,
           footnote_as_chunk = TRUE)

writeLines(latex_output_long, here("tables", "johansen_results_longtable.tex"))

# Version 3: Compact with grouped headers (RECOMMENDED)
latex_table_compact <- latex_table_data %>%
  mutate(
    Follower = paste0(sprintf("%.3f", as.numeric(`Max α`)), " {\\scriptsize (", Follower, ")}"),
    Leader = paste0(sprintf("%.3f", as.numeric(`Min α`)), " {\\scriptsize (", Leader, ")}")
  ) %>%
  select(-`Max α`, -`Min α`)

latex_output_compact <- latex_table_compact %>%
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = "Johansen Cointegration Tests and Price Discovery",
      label = "tab:johansen_compact",
      align = c("l", "l", "c", rep("c", sum(grepl("^r", names(latex_table_compact)))), "r", "r"),
      col.names = c("Metal", "Model", "$r$", 
                    names(latex_table_compact)[grepl("^r", names(latex_table_compact))],
                    "Follower", "Leader"),
      linesep = c("", "", "\\addlinespace")) %>%
  kable_styling(latex_options = c("hold_position"),
                font_size = 11) %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c(" " = 3, "Trace Statistics" = sum(grepl("^r", names(latex_table_compact))), 
                     "Price Discovery (Error Correction Speed)" = 2)) %>%
  footnote(general = c("Trace statistics test $H_0$: rank = $r$ versus $H_1$: rank $> r$. Significance levels: $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
                       "Follower has fastest error correction (max $|\\\\alpha|$); Leader has slowest (min $|\\\\alpha|$). Exchange abbreviations in parentheses."),
           general_title = "Notes:",
           escape = FALSE)

writeLines(latex_output_compact, here("tables", "johansen_results_compact.tex"))

# Version 4: Landscape orientation (for very wide tables)
latex_output_landscape <- latex_table_compact %>%
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = "Johansen Cointegration Tests and Price Discovery",
      label = "tab:johansen_landscape",
      align = c("l", "l", "c", rep("c", sum(grepl("^r", names(latex_table_compact)))), "r", "r"),
      col.names = c("Metal", "Model", "$r$", 
                    names(latex_table_compact)[grepl("^r", names(latex_table_compact))],
                    "Follower (Max $|\\alpha|$)", "Leader (Min $|\\alpha|$)"),
      linesep = c("", "", "\\addlinespace")) %>%
  kable_styling(latex_options = c("hold_position"),
                font_size = 10) %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c(" " = 3, "Trace Statistics" = sum(grepl("^r", names(latex_table_compact))), 
                     "Price Discovery" = 2)) %>%
  footnote(general = "Trace statistics test $H_0$: rank = $r$. Significance: $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$. Exchange abbreviations in parentheses.",
           general_title = "Note:",
           escape = FALSE)

# Wrap in landscape
latex_landscape <- paste0(
  "\\begin{landscape}\n",
  latex_output_landscape,
  "\n\\end{landscape}"
)

writeLines(latex_landscape, here("tables", "johansen_results_landscape.tex"))

message("\n=== Four versions created ===")
message("1. Standard (RECOMMENDED): tables/johansen_results.tex")
message("2. Longtable version: tables/johansen_results_longtable.tex")
message("3. Compact with headers: tables/johansen_results_compact.tex")
message("4. Landscape: tables/johansen_results_landscape.tex")

# Preview
cat("\n=== Table preview ===\n")
print(latex_table_compact)
print(latex_landscape)

cat(latex_output)

#---- Sonet v.2----
# --- Create table with two-line cells for Leader/Follower ---

latex_table_twolines <- latex_table_data %>%
  mutate(
    # Create two-line cells: alpha value on first line, exchange name on second
    Follower = paste0(sprintf("%.3f", as.numeric(`Max α`)), " \\\\ {\\scriptsize ", Follower, "}"),
    Leader = paste0(sprintf("%.3f", as.numeric(`Min α`)), " \\\\ {\\scriptsize ", Leader, "}")
  ) %>%
  select(-`Max α`, -`Min α`)

# Get trace column names
trace_cols <- names(latex_table_twolines)[grepl("^r", names(latex_table_twolines))]

latex_output_twolines <- latex_table_twolines %>%
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = "Johansen Cointegration Tests and Price Discovery",
      label = "tab:johansen",
      align = c("l", "l", "c", rep("c", length(trace_cols)), "c", "c"),
      col.names = c("Metal", "Model", "$r$", 
                    trace_cols,
                    "Follower", "Leader"),
      linesep = c("", "", "\\addlinespace")) %>%
  kable_styling(latex_options = c("hold_position"),
                font_size = 10) %>%
  column_spec(1, bold = TRUE, width = "1.2cm") %>%
  column_spec(2, width = "1.5cm") %>%
  column_spec((4 + length(trace_cols)):(5 + length(trace_cols)), width = "2cm") %>%  # Follower/Leader columns
  add_header_above(c(" " = 3, "Trace Statistics" = length(trace_cols), 
                     "Max $|\\\\alpha|$" = 1, "Min $|\\\\alpha|$" = 1)) %>%
  footnote(general = c("Trace statistics test $H_0$: cointegration rank = $r$ vs $H_1$: rank $> r$.",
                       "Significance: $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
                       "Max $|\\\\alpha|$ = follower (fastest adjustment); Min $|\\\\alpha|$ = leader (slowest adjustment)."),
           general_title = "Notes:",
           escape = FALSE)

# Save to file
writeLines(latex_output_twolines, here("tables", "johansen_results_twolines.tex"))

# Print in console
cat("\n=== LaTeX Table Code ===\n\n")
cat(latex_output_twolines)

message("\n\n=== Table saved to: tables/johansen_results_twolines.tex ===")

#---- v3 spytaj o to -----
#Error in `bind_rows()`:
#! Can't combine `..1$Rank` <integer> and `..2$Rank` <character>.Run `rlang::last_trace()` to see where the error occurred.
# --- Create table with exchange names in separate rows ---

# First, create main rows with alpha values
main_rows <- latex_table_data %>%
  mutate(
    Metal = ifelse(Metal == "", paste0("\\hspace{0.5cm}", Model), Metal),
    Follower = sprintf("%.3f", as.numeric(`Max α`)),
    Leader = sprintf("%.3f", as.numeric(`Min α`))
  ) %>%
  select(-Model, -`Max α`, -`Min α`)

# Then create exchange name rows (to go underneath)
name_rows <- latex_table_data %>%
  mutate(
    Metal = "",
    Rank = "",
    Follower = paste0("{\\scriptsize ", Follower, "}"),
    Leader = paste0("{\\scriptsize ", Leader, "}")
  ) %>%
  select(-Model, -`Max α`, -`Min α`)

# Set trace statistics to empty in name rows
trace_cols <- names(name_rows)[grepl("^r", names(name_rows))]
for (col in trace_cols) {
  name_rows[[col]] <- ""
}

# Interleave main rows and name rows
final_table <- bind_rows(
  lapply(1:nrow(main_rows), function(i) {
    bind_rows(main_rows[i, ], name_rows[i, ])
  })
)

# Get trace column names
trace_cols <- names(final_table)[grepl("^r", names(final_table))]

latex_output_final <- final_table %>%
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = "Johansen Cointegration Tests and Price Discovery",
      label = "tab:johansen",
      align = c("l", "c", rep("c", length(trace_cols)), "c", "c"),
      col.names = c("Metal", "$r$", 
                    trace_cols,
                    "Follower", "Leader"),
      linesep = c("", "", "", "\\addlinespace")) %>%  # Space after every 4th row (2 models x 2 rows each)
  kable_styling(latex_options = c("hold_position"),
                font_size = 10) %>%
  column_spec(1, bold = TRUE, width = "2.2cm") %>%
  column_spec((3 + length(trace_cols)):(4 + length(trace_cols)), width = "1.8cm") %>%
  add_header_above(c(" " = 2, "Trace Statistics" = length(trace_cols), 
                     "Max $|\\\\alpha|$" = 1, "Min $|\\\\alpha|$" = 1)) %>%
  footnote(general = c("Each metal shows two specifications: basic model (first) and with bubble dummies (indented). Exchange names shown in second row under alpha values.",
                       "Trace statistics: $H_0$: rank = $r$. Significance: $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
                       "Follower = max $|\\\\alpha|$ (fastest adjustment); Leader = min $|\\\\alpha|$ (slowest adjustment)."),
           general_title = "Notes:",
           escape = FALSE)

# Save to file
writeLines(latex_output_final, here("tables", "johansen_results_final.tex"))

# Print in console
cat("\n=== LaTeX Table Code ===\n\n")
cat(latex_output_final)

message("\n\n=== Table saved ===")


cat(latex_output_twolines)

#---- Gemini ----
# --- Create table with exchange names in separate rows ---

# 1. Prepare Main Rows (Numeric Values)
# We convert everything to character at the end so it can merge with name_rows
main_rows <- latex_table_data %>%
  mutate(
    Metal = ifelse(Metal == "", paste0("\\hspace{0.5cm}", Model), Metal),
    # Format alphas as 3-decimal strings
    Follower_Val = sprintf("%.3f", as.numeric(`Max α`)),
    Leader_Val = sprintf("%.3f", as.numeric(`Min α`))
  ) %>%
  select(-Model, -`Max α`, -`Min α`) %>%
  mutate(across(everything(), as.character))

# 2. Prepare Name Rows (Exchange Names)
name_rows <- latex_table_data %>%
  mutate(
    Metal = "",
    Rank = "",
    # Use scriptsize for the exchange names
    Follower_Val = paste0("{\\scriptsize ", Follower, "}"),
    Leader_Val = paste0("{\\scriptsize ", Leader, "}")
  ) %>%
  # Set all trace statistics to empty strings
  mutate(across(starts_with("r"), ~ "")) %>%
  select(-Model, -`Max α`, -`Min α`, -Follower, -Leader) %>%
  mutate(across(everything(), as.character))

# 3. Interleave main rows and name rows
# map_dfr is a cleaner way to do the lapply/bind_rows logic
final_table <- map_dfr(1:nrow(main_rows), ~ {
  bind_rows(main_rows[.x, ], name_rows[.x, ])
})

# Identify columns for the LaTeX header
trace_cols_names <- names(final_table)[grepl("^r", names(final_table))]

# 4. Generate LaTeX Output
latex_output_final <- final_table %>%
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = "Johansen Cointegration Tests and Price Discovery",
      label = "tab:johansen",
      align = c("l", "c", rep("c", length(trace_cols_names)), "c", "c"),
      col.names = c("Metal", "$r$", 
                    trace_cols_names,
                    "Follower", "Leader"),
      # Space after every 4th row (2 scenarios * 2 rows each)
      linesep = c("", "\\addlinespace")) %>% 
  kable_styling(latex_options = c("hold_position"),
                font_size = 10) %>%
  column_spec(1, bold = TRUE, width = "2.5cm") %>%
  # Dynamic width for alpha columns
  column_spec((ncol(final_table)-1):ncol(final_table), width = "2.0cm") %>%
  add_header_above(c(" " = 2, "Trace Statistics" = length(trace_cols_names), 
                     "Max $|\\alpha|$" = 1, "Min $|\\alpha|$" = 1)) %>%
  footnote(general = c("Each metal shows two specifications: basic model (first) and with bubble dummies (indented). Exchange names shown in second row under alpha values.",
                       "Trace statistics: $H_0$: rank = $r$. Significance: $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
                       "Follower = max $|\\alpha|$ (fastest adjustment); Leader = min $|\\alpha|$ (slowest adjustment)."),
           general_title = "Notes:",
           escape = FALSE)

# Save and Print
writeLines(latex_output_final, here("tables", "johansen_results_final.tex"))
cat(latex_output_final)

#----Gemini 2 ----
# --- 1. Przygotowanie nazw kolumn pomocniczych ---
# Zakładamy, że Twoje kolumny w latex_table_data to: 
# Metal, Rank, r0, r1, ..., Max_Alpha, Min_Alpha, Follower_Name, Leader_Name

# Pobieramy nazwy kolumn r0, r1 itd.
trace_cols <- names(latex_table_data)[grepl("^r", names(latex_table_data))]

# --- 2. Główne rzędy (Liczby) ---
main_rows <- latex_table_data %>%
  mutate(
    Metal = ifelse(Metal == "", paste0("\\hspace{0.5cm}", Model), Metal),
    # Główne wartości Alpha
    Follower_Disp = sprintf("%.3f", as.numeric(`Max α`)),
    Leader_Disp = sprintf("%.3f", as.numeric(`Min α`))
  ) %>%
  # Wybieramy kolumny w konkretnej kolejności
  dplyr::select(Metal, Rank, all_of(trace_cols), Follower_Disp, Leader_Disp) %>%
  mutate(across(everything(), as.character))

# --- 3. rzędy pomocnicze (Nazwy giełd) ---
name_rows <- latex_table_data %>%
  mutate(
    Metal = "",
    Rank = "",
    # Nazwy giełd lądują w tych samych kolumnach co liczby wyżej
    Follower_Disp = paste0("{\\scriptsize ", Follower, "}"),
    Leader_Disp = paste0("{\\scriptsize ", Leader, "}")
  ) %>%
  # Wypełniamy kolumny Trace pustym znakiem
  mutate(across(all_of(trace_cols), ~ "")) %>%
  # Wybieramy dokładnie te same kolumny co w main_rows
  dplyr::select(Metal, Rank, all_of(trace_cols), Follower_Disp, Leader_Disp) %>%
  mutate(across(everything(), as.character))

# --- 4. Przeplatanie rzędów ---
final_table <- map_dfr(1:nrow(main_rows), ~ {
  bind_rows(main_rows[.x, ], name_rows[.x, ])
})

# --- 5. Generowanie LaTeX ---
latex_output_final <- final_table %>%
  kbl(format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      align = c("l", "c", rep("c", length(trace_cols)), "c", "c"),
      col.names = c("Metal", "$r$", trace_cols, "Follower", "Leader"),
      linesep = c("", "\\addlinespace")) %>% 
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  column_spec(1, bold = TRUE, width = "2.5cm") %>%
  # Wyrównanie kolumn Alpha
  column_spec((ncol(final_table)-1):ncol(final_table), width = "2.2cm") %>%
  add_header_above(c(" " = 2, "Trace Statistics" = length(trace_cols), 
                     "Max $|\\alpha|$" = 1, "Min $|\\alpha|$" = 1))

# Wydrukuj kod
cat(latex_output_final)
