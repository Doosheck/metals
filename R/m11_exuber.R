# --- 1. Load Libraries ---
install.packages("exuber")
install.packages("visdat")
install.packages("here")
#install.packages("exuberdata")
library(tidyverse)
library(exuber)
library(visdat)
library(here)# Critical for OneDrive/Project relative paths
library(kableExtra)

# --- 2. Data Loading ---
# Using here() to ensure paths work across different machines/OneDrive syncs
df_NI <- read_csv(here("data/ALL_nickel_prices_cubic_spline.csv"), 
                  show_col_types = FALSE, 
                  guess_max = 10000)

df_CU <- read_csv(here("data/ALL_copper_prices_cubic_spline.csv"), 
                  show_col_types = FALSE, 
                  guess_max = 10000)

df_LI <- read_csv(here("data/ALL_lithium_prices_cubic_spline.csv"), 
                  show_col_types = FALSE, 
                  guess_max = 10000)

df_CO <- read_csv(here("data/ALL_cobalt_prices_cubic_spline.csv"), 
                  show_col_types = FALSE, 
                  guess_max = 10000)

# --- Visual Inspection ---
# Look at these plots to see where the gaps (NA values) are
# vis_dat(df_NI) + labs(title = "Nickel")
# vis_dat(df_CU) + labs(title = "Copper")
# vis_dat(df_LI) + labs(title = "Lithium")
# vis_dat(df_CO) + labs(title = "Cobalt")



# --- 2.1. Cleaning Individual Series ---

# --- 2.2. Selection ---
# Set the metals you want to include based on your vis_dat results.

target_metals <- c("NI", "CU", "LI", "CO")

# --- 2.3. Synchronizing Overlapping Dates ---
# Define your target window

required_end_date   <- as.Date("2025-07-21")
required_start_date <- as.Date("2021-07-19")

# These are the ONLY columns we explicitly discard
drop_map <- list(
  NI = c("NIETFN", "NIINDA"),
  CU = c("CUETFC", "CUSMMG"),
  LI = c("LILAMC", "LIEALC", "LIEABG"),
  CO = c("COSMMS", "COLMEA")
)

# --- 2.3. Cleaning & Synchronization Pipeline ---

# 1. Load and clean columns first
list_cleaned <- list(NI = df_NI, CU = df_CU, LI = df_LI, CO = df_CO)[target_metals] %>%
  imap(function(df, name) {
    df %>%
      mutate(Date = as.Date(Date)) %>%
      select(Date, everything(), -any_of(drop_map[[name]])) %>%
      # Clip data to your specific window first
      filter(Date >= required_start_date & Date <= required_end_date) %>%
      na.omit()
  })

# 2. Survival Filter (Check if the resulting dataframes are actually populated)
# This ensures we don't try to join an empty dataframe
list_filtered <- list_cleaned %>%
  keep(~ nrow(.x) > 0)

# 3. Find the common dates across the surviving series
# This will find the intersection of dates where ALL remaining series have data
common_dates <- Reduce(intersect, map(list_filtered, ~ .x$Date)) %>% as.Date()

# 4. Combine into the final data frame
# This will now include ALL columns (series) from the surviving metals
df_final <- list_filtered %>%
  map(~ filter(.x, Date %in% common_dates)) %>%
  reduce(full_join, by = "Date") %>%
  arrange(Date)

# --- 2.4. Final Verification ---

message("Metals included: ", paste(names(list_filtered), collapse = ", "))
message("Series included: ", paste(names(df_final)[-1], collapse = ", "))
message("Total columns in df_final: ", ncol(df_final) - 1) 

# --- 2.5 Adding one series:
#Global X Lithium & Battery Tech ETF (LIT)
# library(tidyverse)
# library(tidyquant)
# 
# # 1. Fetch LIT data from Yahoo Finance
# # We select only Date and Adjusted price to keep it concise
# df_lit <- tq_get("LIT", from = min(common_dates), to = max(common_dates)) %>%
#   select(Date = date, LIT = adjusted)
# 
# # 2. Add LIT to the existing list or join directly
# # Assuming list_filtered is your existing list of tibbles
# df_final <- list_filtered %>%
#   map(~ filter(.x, Date %in% common_dates)) %>%
#   reduce(full_join, by = "Date") %>%
#   full_join(df_lit, by = "Date") %>% # Join the Yahoo Finance data
#   arrange(Date)
# 
# print(paste("Analysis starts on:", min(df_final$Date)))
# print(paste("Analysis ends on:", max(df_final$Date)))
# print(paste("Total observations (rows):", nrow(df_final)))
# head(df_final)


# # --- 2.5a Save the Combined File ---
# # This saves the synchronized 'df_final' containing all selected metals
# write_csv(df_final, here("data", "combined_metals_cleaned2.csv"))
# message("Saved combined file: combined_metals_cleaned2.csv")

df_plot <- df_final %>%
  pivot_longer(cols = -Date, names_to = "Series", values_to = "Price")

# 2. Generate the plot
ggplot(df_plot, aes(x = Date, y = Price, color = Series)) +
  geom_line(show.legend = FALSE) + # Legend is redundant if we use facets
  facet_wrap(~ Series, ncol = 4, scales = "free_y") + 
  theme_minimal() +
  labs(
    title = "Time Series Overview: Metals & LIT",
    subtitle = "Standardized by individual scales (free y-axis)",
    x = NULL,
    y = "Adjusted Price / Value"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# --- 4.1. Matrix Preparation ---
# exuber requires a numeric matrix. We log-transform prices here.
data_matrix <- df_final %>%
  select(-Date) %>%
  mutate(across(everything(), log)) %>%
  as.matrix()

# Assign dates as character row names for tracking
rownames(data_matrix) <- as.character(df_final$Date)

# --- 4.2. Run RADF Estimation ---
# Note: This might take a moment depending on the number of series.
est_results <- radf(data_matrix)

# --- 4.3. Calculate Critical Values ---
# Since we have a specific sample size (n), we generate Monte Carlo 
# critical values to perform the statistical test.

# for the first time calculate and save
n_obs <- nrow(data_matrix)

mc_cv <- here("outputs", "R_objects", "mc_cv.rds")

if (file.exists(mc_cv)) {
  mc_cv <- readRDS(mc_cv)
  message("Loaded mc_cv from cache.")
} else {
  mc_cv <- radf_mc_cv(n = nrow(data_matrix), seed = 123)
  saveRDS(mc_cv, mc_cv)
  message("Computed and saved mc_cv.")
}

# --- 4.4. Summary of Results ---
# This displays which series exhibit evidence of speculative bubbles
summary(est_results, cv = mc_cv)


# ---- 5. Bubble Dummies & Plots Claude ----

# --- 5.0 Compute datestamp ONCE ---
bubble_dates_raw <- datestamp(est_results, cv = mc_cv)

# --- 5.1 Helper: OLS trend direction ---
is_positive_trend <- function(start_idx, end_idx, prices) {
  p <- prices[start_idx:end_idx]
  if (length(p) > 2) coef(lm(p ~ seq_along(p)))[2] > 0 else tail(p, 1) > head(p, 1)
}

# --- 5.2 Reusable dummy-builder ---
# upward_only = FALSE  › marks ALL explosive episodes (up + down)
# upward_only = TRUE   › marks only episodes with a positive OLS trend
build_bubble_dummies <- function(bubble_dates, df_prices, upward_only = FALSE) {
  series_names <- names(df_prices)[-1]
  
  map_dfc(series_names, function(series) {
    vec    <- rep(0L, nrow(df_prices))
    periods <- bubble_dates[[series]]
    
    if (!is.null(periods) && nrow(periods) > 0) {
      prices <- as.numeric(df_prices[[series]])
      
      for (i in seq_len(nrow(periods))) {
        s <- periods[i, "Start"]
        e <- periods[i, "End"]
        
        # Apply direction filter only when requested
        if (!upward_only || is_positive_trend(s, e, prices)) {
          vec[s:e] <- 1L
        }
      }
    }
    tibble(!!paste0(series, "_BD") := vec)
  })
}

# --- 5.3 Build both versions ---
df_final_dataset_updown <- bind_cols(df_final, build_bubble_dummies(bubble_dates_raw, df_final, upward_only = FALSE))
df_final_dataset_up     <- bind_cols(df_final, build_bubble_dummies(bubble_dates_raw, df_final, upward_only = TRUE))

write_csv2(df_final_dataset_updown, here("R/results_R", "series_and_bubble_up_down.csv"))
write_csv2(df_final_dataset_up,     here("R/results_R", "series_and_bubble_up.csv"))

# --- 5.4 Filter bubble_dates for plotting (upward only) ---
bubble_dates_filtered <- imap(bubble_dates_raw, ~ {
  if (is.null(.x) || nrow(.x) == 0) return(NULL)
  prices <- as.numeric(df_final[[.y]])
  keep   <- apply(.x, 1, function(row) is_positive_trend(row["Start"], row["End"], prices))
  .x[keep, , drop = FALSE]
}) %>% compact()

# --- 5.5 Plotting rectangles ---
bubble_rects <- imap_dfr(bubble_dates_filtered, ~ tibble(
  Series = .y,
  xmin   = df_final$Date[.x[, "Start"]],
  xmax   = df_final$Date[.x[, "End"]],
  ymin   = -Inf,
  ymax   =  Inf
))

# --- 5.6 Plot Factory ---
# Reusable function so you can call it for either dataset/rect combination

plot_bubbles <- function(df_dataset, rects, title = "") {
  
  # 1. Identify price series (strip Date and _BD dummies)
  price_series <- names(df_dataset)[
    !names(df_dataset) %in% "Date" & !grepl("_BD$", names(df_dataset))
  ]
  
  # 2. Long format for price lines
  df_plot_long <- df_dataset %>%
    select(Date, all_of(price_series)) %>%
    pivot_longer(-Date, names_to = "Series", values_to = "Price")
  
  # 3. Build plot
  p <- ggplot() +
    # Shaded bubble periods
    {if (nrow(rects) > 0)
      geom_rect(data = rects,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "grey70", alpha = 0.5)
    } +
    # Price lines
    geom_line(data = df_plot_long,
              aes(x = Date, y = Price),
              color = "black", linewidth = 0.5) +
    facet_wrap(~ Series, scales = "free_y", ncol = 4) +
    theme_minimal() +
    theme(
      strip.text       = element_text(face = "plain", size = 12), #plot label
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 7),
      axis.text.x      = element_text(angle = 45, hjust = 1)
    ) +
    labs(title = title, x = NULL, y = NULL)
  
  p
}

# --- 5.7 Generate & Save Both Versions ---

# Upward-only bubbles (rects already filtered to upward in 5.4)
plot_up <- plot_bubbles(df_final_dataset_up, bubble_rects, title = "")

# All bubbles — need a separate rect table built from the UNFILTERED dates
bubble_rects_all <- imap_dfr(bubble_dates_raw, ~ {
  if (is.null(.x) || nrow(.x) == 0) return(tibble())
  tibble(
    Series = .y,
    xmin   = df_final$Date[.x[, "Start"]],
    xmax   = df_final$Date[.x[, "End"]],
    ymin   = -Inf,
    ymax   =  Inf
  )
})

plot_updown <- plot_bubbles(df_final_dataset_updown, bubble_rects_all, title = "")

# Save — explicit width/height avoids 16-panel crowding
ggsave(here("graphsR", "bubbles_up_only.pdf"),    plot_up,     width = 14, height = 10)
ggsave(here("graphsR", "bubbles_up_and_down.pdf"), plot_updown, width = 14, height = 10)


# ---- 6. Table Factory ----

make_bubble_table <- function(df_dataset, rects, 
                              caption = "Summary of speculative bubble episodes.",
                              label   = "tab:bubble_summary",
                              csv_path = NULL) {
  
  # 1. Derive price series from the dataset (same logic as plot factory)
  price_series <- names(df_dataset)[
    !names(df_dataset) %in% "Date" & !grepl("_BD$", names(df_dataset))
  ]
  
  # 2. Base calculations
  bubble_counts <- rects %>%
    count(Series, name = "Number_of_Bubbles")
  
  df_summary <- tibble(Series = price_series) %>%
    arrange(Series) %>%
    rowwise() %>%
    mutate(Days_in_Bubble = sum(df_dataset[[paste0(Series, "_BD")]] == 1, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(bubble_counts, by = "Series") %>%
    mutate(Number_of_Bubbles = replace_na(Number_of_Bubbles, 0))
  
  # 3. Reshape into publication layout (blocks of 4)
  n_series <- nrow(df_summary)
  
  df_reshaped <- df_summary %>%
    mutate(Block   = ceiling(row_number() / 4),
           Col_Pos = (row_number() - 1) %% 4 + 1) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(cols      = c(Series, Days_in_Bubble, Number_of_Bubbles),
                 names_to  = "Metric",
                 values_to = "Value") %>%
    mutate(Metric = factor(Metric,
                           levels = c("Series", "Days_in_Bubble", "Number_of_Bubbles"))) %>%
    arrange(Block, Metric, Col_Pos) %>%
    pivot_wider(id_cols     = c(Block, Metric),
                names_from  = Col_Pos,
                values_from = Value,
                names_prefix = "Col_") %>%
    ungroup() %>%
    mutate(Metric = case_when(
      Metric == "Series"            ~ "",
      Metric == "Days_in_Bubble"    ~ "Days in Bubble",
      Metric == "Number_of_Bubbles" ~ "Number of Bubbles"
    )) %>%
    select(Metric, starts_with("Col_"))           # handles <16 series safely
  
  df_reshaped[is.na(df_reshaped)] <- ""
  
  # 4. Determine actual number of columns for col.names and linesep
  n_cols <- ncol(df_reshaped) - 1                 # exclude Metric column
  
  # linesep: every 3rd row gets \addlinespace (one block = 3 rows)
  n_blocks   <- ceiling(n_series / 4)
  linesep_vec <- rep(c("", "", "\\addlinespace"), n_blocks)[seq_len(n_blocks * 3)]
  
  # 5. LaTeX table
  kable_output <- kable(df_reshaped,
                        format    = "latex",
                        booktabs  = TRUE,
                        col.names = NULL,
                        linesep   = linesep_vec,
                        caption   = caption,
                        label     = label)
  
  print(kable_output)
  
  # 6. Optional CSV export
  if (!is.null(csv_path)) {
    write_csv2(df_summary, csv_path)
    message("Saved: ", csv_path)
  }
  
  invisible(df_summary)   # return silently for downstream use if needed
}

# ---- 6.1 Call for both versions ----

make_bubble_table(
  df_dataset = df_final_dataset_up,
  rects      = bubble_rects,
  caption    = "Summary of speculative bubble episodes (upward only).",
  label      = "tab:bubble_summary_up",
  csv_path   = here("R/results_R", "bubble_summary_up.csv")
)

make_bubble_table(
  df_dataset = df_final_dataset_updown,
  rects      = bubble_rects_all,
  caption    = "Summary of speculative bubble episodes (all directions).",
  label      = "tab:bubble_summary_updown",
  csv_path   = here("R/results_R", "bubble_summary_updown.csv")
)

# ---- 7. Descriptive Statistics ----
library(knitr)
library(kableExtra)

# 1. Compute log-returns (same transformation used for RADF)

df_desc <- df_returns %>%
  select(-Date) %>%
  reframe(across(everything(), list(
    Mean = ~ mean(.x, na.rm = TRUE),
    Std  = ~ sd(.x,   na.rm = TRUE),
    Min  = ~ min(.x,  na.rm = TRUE),
    Q1   = ~ quantile(.x, 0.25, na.rm = TRUE),
    Q3   = ~ quantile(.x, 0.75, na.rm = TRUE),
    Max  = ~ max(.x,  na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(),
               names_to      = c("Ticker", ".value"),
               names_pattern = "^(.+)_(Mean|Std|Min|Q1|Q3|Max)$") %>%
  arrange(Ticker)

df_desc_fmt <- df_desc %>%
  mutate(across(c(Mean, Std, Min, Q1, Q3, Max),
                ~ formatC(.x, digits = 6, format = "f")))

kable_output <- kable(df_desc_fmt,
                      format    = "latex",
                      booktabs  = TRUE,
                      linesep   = "",
                      caption   = "Descriptive Statistics of Metal Price Returns",
                      label     = "tab:metals_returns_stats",
                      col.names = c("Ticker", "Mean", "Std", "Min", "Q1", "Q3", "Max"))
print(kable_output)

write_csv2(df_desc_fmt, here("R/results_R", "descriptive_stats.csv"))

# ---- OLD code ----
# --- 5.1. Extract Bubble Information ---

# 1. Identify bubble windows for rectangles (start/end dates)
# bubble_dates <- datestamp(est_results, cv = mc_cv)

# # 2. Create 0-1 Dummies manually from datestamp results
# # Initialize a dataframe with all zeros
# series_names <- names(df_final)[-1]  # Exclude Date column
# bubble_dummies <- df_final %>% select(Date)
# 
# # Add a column for each series, initialized to 0
# for (series_name in series_names) {
#   bubble_dummies[[series_name]] <- 0L
# }
# 
# # Fill in 1s where bubbles are detected
# for (series_name in names(bubble_dates)) {
#   periods <- bubble_dates[[series_name]]
#   
#   if (!is.null(periods) && nrow(periods) > 0) {
#     # For each bubble period, set the corresponding rows to 1
#     for (i in 1:nrow(periods)) {
#       start_idx <- periods[i, "Start"]
#       end_idx <- periods[i, "End"]
#       bubble_dummies[[series_name]][start_idx:end_idx] <- 1L
#     }
#   }
# }

# 2. Pre-allocate an empty matrix for the zero-one dummies

# series_names <- names(df_final)[-1] 
# dummy_matrix <- matrix(0L, nrow = nrow(df_final), ncol = length(series_names))
# colnames(dummy_matrix) <- paste0(series_names, "_BD")
# 
# # 2. Iterate through series and evaluate whole episodes
# for (i in seq_along(series_names)) {
#   series <- series_names[i]
#   periods <- bubble_dates[[series]]
#   
#   if (!is.null(periods) && nrow(periods) > 0) {
#     
#     # Extract prices as a strict numeric vector to avoid tibble/dataframe subsetting issues
#     # Note: Ensure you are referencing the correct dataframe for prices (data_matrix or df_final)
#     prices <- as.numeric(df_final[[series]])
#     
#     for (j in 1:nrow(periods)) {
#       start_idx <- periods[j, "Start"]
#       end_idx   <- periods[j, "End"]
#       
#       # THE EPISODE FILTER: 
#       # Check if the price actually grew over the course of the explosive period.
#       # This completely eliminates declining bubbles (crashes).
#       if (prices[end_idx] > prices[start_idx]) {
#         
#         # If it's a true upward bubble, fill the entire period with 1s
#         dummy_matrix[start_idx:end_idx, i] <- 1L
#         
#       }
#     }
#   }
# }
# 
# # 3. Final merge
# df_final_dataset <- bind_cols(df_final, as_tibble(dummy_matrix))
# head(df_final_dataset)
# ##df_final_dataset is not corrected for the downward trend in the data
# write_csv2(df_final_dataset, here("R/results_R", "series_and_bubble_up_down.csv"))
# 
# # --- 5.2. Prepare Plotting Data ---
# 
# # 1. Helper function: Evaluates the OLS trend for a single episode
# is_positive_trend <- function(start_idx, end_idx, prices) {
#   p <- prices[start_idx:end_idx]
#   # Returns TRUE if OLS slope > 0, otherwise checks simple start/end difference
#   if (length(p) > 2) coef(lm(p ~ seq_along(p)))[2] > 0 else tail(p, 1) > head(p, 1)
# }
# 
# # 2. Filter the raw exuber list cleanly using purrr::imap
# bubble_dates_filtered <- imap(datestamp(est_results, cv = mc_cv), ~ {
#   if (is.null(.x) || nrow(.x) == 0) return(NULL)
#   
#   prices <- as.numeric(df_final[[.y]])
#   # Apply our helper function to every row (episode) in the current series
#   keep <- apply(.x, 1, function(row) is_positive_trend(row["Start"], row["End"], prices))
#   
#   # Return only the rows that passed the filter
#   .x[keep, , drop = FALSE]
# }) %>% compact() # compact() safely removes any series that ended up empty (NULL)
# 
# # 3. Generate plotting rectangles (Ultra-short imap_dfr approach)
# bubble_rects <- imap_dfr(bubble_dates_filtered, ~ tibble(
#   Series = .y,
#   xmin = df_final$Date[.x[, "Start"]],
#   xmax = df_final$Date[.x[, "End"]],
#   ymin = -Inf, 
#   ymax = Inf
# ))
# 
# # 4. Generate 0-1 dummies and attach them to the main dataset
# dummy_df <- map_dfc(names(df_final)[-1], function(series) {
#   vec <- rep(0L, nrow(df_final))
#   periods <- bubble_dates_filtered[[series]]
#   
#   # Inject 1s into the vector for valid periods
#   if (!is.null(periods) && nrow(periods) > 0) {
#     for(i in 1:nrow(periods)) vec[periods[i, "Start"]:periods[i, "End"]] <- 1L
#   }
#   
#   # Create a named column dynamically (e.g., LISAME_BD)
#   tibble(!!paste0(series, "_BD") := vec)
# })
# 
# # Final merge
# df_final_dataset_up <- bind_cols(df_final, dummy_df)
# write_csv2(df_final_dataset_up, here("R/results_R", "series_and_bubble_up.csv"))
# 
# #check
# sum(df_final_dataset$LISAME_BD)
# sum(df_final_dataset_up$LISAME_BD)


# --- 5.3. Final Plotting & Saving ---

# 1. Identify all price series (excluding Date and the dummy _BD columns)
# all_price_series <- names(df_final_dataset)[!names(df_final_dataset) %in% c("Date") & 
#                                               !grepl("_BD$", names(df_final_dataset))]
# 
# # 2. Prepare the price data for ALL series
# df_plot_all <- df_final_dataset %>%
#   select(Date, all_of(all_price_series)) %>%
#   pivot_longer(-Date, names_to = "Series", values_to = "Price")
# 
# # 3. Create the 4x4 Grid Plot
# bubble_plot_final <- ggplot() +
#   # Layer 1: Shaded yellow areas (only where OLS filter passed)
#   # It uses the bubble_rects we generated earlier
#   {if(nrow(bubble_rects) > 0) {
#     geom_rect(data = bubble_rects, 
#               aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
#               fill ="grey2", alpha = 0.5)
#   }} +
#   # Layer 2: Black price lines for ALL series
#   geom_line(data = df_plot_all, aes(x = Date, y = Price), color = "black", linewidth = 0.5) +
#   # Layout: 4 columns to create the 4x4 look (depending on the number of series)
#   facet_wrap(~ Series, scales = "free_y", ncol = 4) +
#   theme_minimal() +
#   theme(
#     strip.text = element_text(face = "plain", size = 9),
#     panel.grid.minor = element_blank(),
#     axis.text = element_text(size = 7)
#   ) +
#   labs(title = "",
#        #subtitle = "",
#        x = NULL, y = "")
# 
# # Display the plot
# print(bubble_plot_final)
# ggsave(here("graphsR", "bubbles_detected_only.pdf"), bubble_plot_final)

# ---- 6 .A Table for bubbles: ----
# library(dplyr)
# library(tidyr)
# library(knitr)
# 
# # 1. Base calculations
# # 1. Base calculations for the metrics
# bubble_counts <- bubble_rects %>%
#   count(Series, name = "Number_of_Bubbles")
# 
# df_summary <- tibble(Series = all_price_series) %>%
#   arrange(Series) %>% 
#   rowwise() %>%
#   mutate(Days_in_Bubble = sum(df_final_dataset[[paste0(Series, "_BD")]] == 1, na.rm = TRUE)) %>%
#   ungroup() %>%
#   left_join(bubble_counts, by = "Series") %>%
#   mutate(Number_of_Bubbles = replace_na(Number_of_Bubbles, 0))
# 
# # 2. Reshape the dataframe into the exact visual layout (Blocks of 4)
# df_reshaped <- df_summary %>%
#   # Assign each metal to a specific block (row of 4) and a specific column position (1 to 4)
#   mutate(Block = ceiling(row_number() / 4),
#          Col_Pos = (row_number() - 1) %% 4 + 1) %>%
#   
#   # Convert all columns to character so we can mix text (Metal Names) and numbers in the final table
#   mutate(across(everything(), as.character)) %>%
#   
#   # Pivot to a long format to stack Series, Days, and Counts vertically
#   pivot_longer(cols = c(Series, Days_in_Bubble, Number_of_Bubbles), 
#                names_to = "Metric", values_to = "Value") %>%
#   
#   # Enforce the correct display order: Series Name -> Days -> Number of Bubbles
#   mutate(Metric = factor(Metric, levels = c("Series", "Days_in_Bubble", "Number_of_Bubbles"))) %>%
#   arrange(Block, Metric, Col_Pos) %>%
#   
#   # Pivot back to a wide format, explicitly creating 4 columns
#   pivot_wider(id_cols = c(Block, Metric), names_from = Col_Pos, values_from = Value, names_prefix = "Col_") %>%
#   ungroup() %>%
#   
#   # Prepare clean row labels for publication
#   mutate(Metric = case_when(
#     Metric == "Series" ~ "",  # Leave the metric name blank for the row containing metal names
#     Metric == "Days_in_Bubble" ~ "Days in Bubble",
#     Metric == "Number_of_Bubbles" ~ "Number of Bubbles"
#   )) %>%
#   
#   # Drop the technical 'Block' column, keeping only the final layout
#   select(Metric, Col_1, Col_2, Col_3, Col_4)
# 
# # Replace any potential NAs with empty strings (useful if you have e.g., 14 series instead of exactly 16)
# df_reshaped[is.na(df_reshaped)] <- ""
# 
# # 3. Generate clean, publication-ready LaTeX code
# kable_output <- kable(df_reshaped, 
#                       format = "latex", 
#                       booktabs = TRUE, 
#                       col.names = NULL, # Hides the default dataframe column names (Col_1, Col_2, etc.)
#                       linesep = c("", "", "\\addlinespace"), # Adds a subtle vertical space between metal blocks
#                       caption = "Summary of speculative bubble episodes.",
#                       label = "tab:bubble_summary")
# 
# # Print to console so you can copy and paste directly into Overleaf
# print(kable_output)
# 
# write_csv2(df_summary, here("R/results_R", "bubble_summary_excel.csv"))
# 
# # --- 5.4. Export Results ---
# 
# # Save the dummy variables for the GARCH model
# write_csv(bubble_dummies, here("data", "bubble_dummies.csv"))




# # --- 5.5. Final Verification ---
# message("--- Final Verification ---")
# message("Total series in dataset: ", length(series_names))
# message("Series with detected bubbles: ", length(active_series))
# if(length(active_series) > 0) {
#   message("Bubble series: ", paste(active_series, collapse = ", "))
# }
# 
# # Display summary of bubble dummies
# message("\nBubble dummy summary (first 10 rows):")
# print(head(bubble_dummies, 10))
# 
# message("\nBubble periods per series (column sums):")
# col_sums <- colSums(bubble_dummies[, -1])  # Exclude Date column
# print(col_sums[col_sums > 0])  # Only show series with bubbles
# 
# message("\nUnique values in dummy columns:")
# unique_vals <- unique(unlist(bubble_dummies[, -1]))
# message("Values found: ", paste(unique_vals, collapse = ", "))


# --- 5.1. Extract Dummies & Identification (Direct Method) ---

# 1. Identify bubble windows for rectangles (start/end dates)
bubble_dates <- datestamp(est_results, cv = mc_cv)

# 2. Extract 0-1 Dummies using the core index logic
# We use the internal 'index' method but force it into a data frame immediately
# This avoids the 'size 0' and 'out of bounds' errors
ind_logical <- index(est_results, cv = mc_cv)

# We convert the list of logical vectors into a data frame of integers
bubble_dummies <- map_dfc(ind_logical, ~ as.integer(.x)) %>%
  bind_cols(Date = df_final$Date, .)

# --- 5.2. Prepare Plotting Data ---

# Identify specific start/end dates for bubble periods for shading
bubble_dates <- datestamp(est_results, cv = mc_cv)

# Create a dataframe with rectangle coordinates for ggplot
bubble_rects <- map_df(names(bubble_dates), function(name) {
  periods <- bubble_dates[[name]]
  if (nrow(periods) > 0) {
    tibble(
      Series = name,
      xmin = df_final$Date[periods[, "Start"]],
      xmax = df_final$Date[periods[, "End"]],
      ymin = -Inf, ymax = Inf
    )
  }
})

# Identify series that actually contained at least one bubble period
active_series <- if(nrow(bubble_rects) > 0) unique(bubble_rects$Series) else character(0)

# --- 5.3. Final Plotting & Saving ---

# Filter price data for active series only and convert to long format
df_plot_filtered <- df_final %>%
  pivot_longer(-Date, names_to = "Series", values_to = "Price") %>%
  filter(Series %in% active_series)

bubble_plot <- ggplot() +
  # Layer 1: Shaded grey areas representing bubble periods
  {if(length(active_series) > 0) 
    geom_rect(data = filter(bubble_rects, Series %in% active_series), 
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "orange2", alpha = 0.5) 
  } +
  # Layer 2: Black price lines
  geom_line(data = df_plot_filtered, aes(x = Date, y = Price), color = "black") +
  facet_wrap(~ Series, scales = "free_y", ncol = 4) +
  theme_minimal() +
  labs(title = "", 
       x = NULL, y = NULL)

# --- 5.4. Export Results ---


# Save the plot as a PDF
ggsave(here("outputs", "figures", "bubble_tests", "bubbles_detected_only.pdf"), bubble_plot, width = 10, height = 8)

message("--- Final Verification ---")
message("Active Series Found: ", paste(active_series, collapse = ", "))
# This should now correctly display 0s and 1s
print(head(bubble_dummies, 5))

print(bubble_plot)

#---- Version with bubble when prices are increasing and decreasing ----
# --- 5.1. Extract Bubble Information ---

# 1. Identify bubble windows for rectangles (start/end dates)
bubble_dates <- datestamp(est_results, cv = mc_cv)

# 2. Create 0-1-2 Dummies: 0 = no bubble, 1 = bubble during price increase, 2 = bubble during price decrease
# Initialize a dataframe with all zeros
series_names <- names(df_final)[-1]  # Exclude Date column
bubble_dummies <- df_final %>% select(Date)

# Add a column for each series, initialized to 0
for (series_name in series_names) {
  bubble_dummies[[series_name]] <- 0L
}

# Fill in 1s (price increase) or 2s (price decrease) where bubbles are detected
for (series_name in names(bubble_dates)) {
  periods <- bubble_dates[[series_name]]
  
  if (!is.null(periods) && nrow(periods) > 0) {
    # For each bubble period
    for (i in 1:nrow(periods)) {
      start_idx <- periods[i, "Start"]
      end_idx <- periods[i, "End"]
      
      # Calculate price change during the bubble period
      start_price <- df_final[[series_name]][start_idx]
      end_price <- df_final[[series_name]][end_idx]
      
      # Assign 1 for increase, 2 for decrease
      if (end_price > start_price) {
        bubble_dummies[[series_name]][start_idx:end_idx] <- 1L  # Bubble with price increase
      } else {
        bubble_dummies[[series_name]][start_idx:end_idx] <- 2L  # Bubble with price decrease
      }
    }
  }
}


#################################################################################
# --- Alternative: Create separate dummy columns for up/down ---
# This creates two columns per series: seriesname_up and seriesname_down

bubble_dummies_separate <- df_final %>% select(Date)

for (series_name in series_names) {
  bubble_dummies_separate[[paste0(series_name, "_up")]] <- 0L
  bubble_dummies_separate[[paste0(series_name, "_down")]] <- 0L
}

for (series_name in names(bubble_dates)) {
  periods <- bubble_dates[[series_name]]
  
  if (!is.null(periods) && nrow(periods) > 0) {
    for (i in 1:nrow(periods)) {
      start_idx <- periods[i, "Start"]
      end_idx <- periods[i, "End"]
      
      start_price <- df_final[[series_name]][start_idx]
      end_price <- df_final[[series_name]][end_idx]
      
      if (end_price > start_price) {
        bubble_dummies_separate[[paste0(series_name, "_up")]][start_idx:end_idx] <- 1L
      } else {
        bubble_dummies_separate[[paste0(series_name, "_down")]][start_idx:end_idx] <- 1L
      }
    }
  }
}

# --- 5.2. Prepare Plotting Data with color coding ---

bubble_rects <- map_df(names(bubble_dates), function(name) {
  periods <- bubble_dates[[name]]
  
  if (!is.null(periods) && nrow(periods) > 0) {
    map_df(1:nrow(periods), function(i) {
      start_idx <- periods[i, "Start"]
      end_idx <- periods[i, "End"]
      
      start_price <- df_final[[name]][start_idx]
      end_price <- df_final[[name]][end_idx]
      
      tibble(
        Series = name,
        xmin = df_final$Date[start_idx],
        xmax = df_final$Date[end_idx],
        ymin = -Inf, 
        ymax = Inf,
        bubble_type = ifelse(end_price > start_price, "Increase", "Decrease")
      )
    })
  } else {
    tibble(Series = character(), xmin = as.Date(character()), 
           xmax = as.Date(character()), ymin = numeric(), ymax = numeric(),
           bubble_type = character())
  }
})

active_series <- if(nrow(bubble_rects) > 0) unique(bubble_rects$Series) else character(0)

# --- 5.3. Final Plotting with color-coded bubbles ---

df_plot_filtered <- df_final %>%
  pivot_longer(-Date, names_to = "Series", values_to = "Price") %>%
  filter(Series %in% active_series)

bubble_plot <- ggplot() +
  # Layer 1: Color-coded shaded areas (green for increase, red for decrease)
  {if(nrow(bubble_rects) > 0) {
    geom_rect(data = bubble_rects, 
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                  fill = bubble_type),
              alpha = 0.3)
  }} +
  scale_fill_manual(values = c("Increase" = "green4", "Decrease" = "red3"),
                    name = "Bubble Type") +
  # Layer 2: Black price lines
  {if(nrow(df_plot_filtered) > 0) {
    geom_line(data = df_plot_filtered, aes(x = Date, y = Price), color = "black")
  }} +
  facet_wrap(~ Series, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(title = "Markets with Active Speculative Bubbles", 
       x = NULL, y = "Log Price")

# --- 5.4. Export Results ---

# Save BOTH versions
write_csv(bubble_dummies, here("data", "bubble_dummies_coded.csv"))  # 0/1/2 format
write_csv(bubble_dummies_separate, here("data", "bubble_dummies_separate.csv"))  # Separate up/down columns

if(length(active_series) > 0) {
  ggsave(here("outputs", "figures", "bubble_tests", "bubbles_detected_color_coded.pdf"), bubble_plot, 
         width = 10, height = 8)
  message("Plot saved successfully")
}

# --- 5.5. Final Verification ---
message("--- Final Verification ---")
message("\nBubble dummy (0/1/2 coding) summary:")
print(head(bubble_dummies, 10))

message("\nValue counts per series (0/1/2 format):")
for (series in series_names) {
  vals <- table(bubble_dummies[[series]])
  if (length(vals) > 1) {
    message(series, ": ", paste(names(vals), "=", vals, collapse = ", "))
  }
}

message("\nSeparate up/down dummies summary:")
print(head(bubble_dummies_separate, 10))

message("\nBubble counts by type:")
up_counts <- colSums(bubble_dummies_separate[, grep("_up$", names(bubble_dummies_separate))])
down_counts <- colSums(bubble_dummies_separate[, grep("_down$", names(bubble_dummies_separate))])
message("Total bubble-up periods: ", sum(up_counts))
message("Total bubble-down periods: ", sum(down_counts))

#-------------------------------------------------------------------------------
#---- OLD code----
# Konwersja daty i filtrowanie "Cz??ci Wsp?lnej" (dla ka?dego szeregu oddzielnie)
df_clean <- df_NI %>%
  select(-c("NIETFN", "NIWUXI")) %>%  #select(-c("NIETFN", "NIWUXI"))
  mutate(Date = as.Date(Date)) %>%  #check for dates
  na.omit()                         

df_clean <- df_CO %>%
  select(-c("COSMMS", "COCOMX")) %>%
  mutate(Date = as.Date(Date)) %>%  #check for dates
  na.omit()                         

df_clean <- df_CU %>%
  select(-"CUETFC") %>%
  mutate(Date = as.Date(Date)) %>%  #check for dates
  na.omit()                         

colnames(df_LI)
df_clean <- df_LI %>%
  select(-c("LILAMC", "LIEALC", "LIEABG")) %>%
  mutate(Date = as.Date(Date)) %>%  #check for dates
  na.omit()                         


# check data
print(paste("the beginning:", min(df_clean$Date)))
print(paste("the end:", max(df_clean$Date)))
print(paste("number of obs:", nrow(df_clean)))
head(df_clean)

# Prepare data for testing
data_matrix <- df_clean %>% 
  select(-Date) %>%  #remove date column for testing
  mutate(across(everything(), log))

# new rownames for the matrix (dates)
rownames(data_matrix) <- as.character(df_clean$Date)

head(data_matrix)

# Uruchomienie testu (GSADF, SADF, ADF w jednym)
# To mo?e chwil? potrwa? w zale?no?ci od d?ugo?ci szeregu
est_results <- radf(data_matrix)

# Wy?wietlenie podsumowania (czy wykryto ba?ki?)
summary(est_results) #je?eli pojawia si? b??d, w?wczas
# prawodpodobnie jest to problem z instalacj? i trzeba uruchomi? 3 linie:
# exuber::install_exuberdata()
# library(exuberdata)
# est_results <- radf(data_matrix)


# poniewa? u mnie exuberdata nie chce si? zaladowa?,
# liczymy warto?ci krytyczne (w zast?pstwie pakietu exuberdata)
# To mo?e potrwa? kilka-kilkana?cie sekund
wartosci_krytyczne_NI <- radf_mc_cv(n = nrow(data_matrix)) 
wartosci_krytyczne_CO <- radf_mc_cv(n = nrow(data_matrix)) 
wartosci_krytyczne_LI <- radf_mc_cv(n = nrow(data_matrix))

# 2. Wyznacz daty i podaj warto?ci krytyczne 
bubble_dates <- datestamp(est_results, cv = wartosci_krytyczne_NI)
bubble_dates <- datestamp(est_results, cv = wartosci_krytyczne_CO)
bubble_dates <- datestamp(est_results, cv = wartosci_krytyczne_LI)
#bubble_dates <- datestamp(est_results)

# o ile s? jakie? ba?ki, to tu je mo?na narysowa?
autoplot(bubble_dates) #+
  #facet_wrap(~ series, scales = "free_y") +
  #labs(title = "Wykryte ba?ki spekulacyjne na rynkach niklu") +
  #theme_minimal()

# Poka? daty 
print(bubble_dates)

# wykres w ggplot (zamiast autoplot)
keep_order <- colnames(data_matrix)

df_plot <- as.data.frame(data_matrix) %>%
  rownames_to_column("Date") %>%
  mutate(Date = as.Date(Date)) %>%
  pivot_longer(-Date, names_to = "Series", values_to = "Price")%>%
  mutate(Series = factor(Series, levels = keep_order))

# --- B. PRZYGOTOWANIE BANIEK  ---
# P?tla przejdzie przez wyniki i zamieni numery wierszy na daty.

bubble_rects <- data.frame() # Pusty kontener na wyniki

# Pobieramy wektor prawdziwych dat z Twoich danych
all_dates <- as.Date(rownames(data_matrix))

# P?tla po ka?dym szeregu 
for (name in names(bubble_dates)) {
  
  # Pobierz tabel? start/koniec dla danego metalu
  periods <- bubble_dates[[name]]
  
  # Je?li wykryto jakie? ba?ki (tabela nie jest pusta)
  if (nrow(periods) > 0) {
    
    # Zamie? numery wierszy na Daty
    starts <- all_dates[periods[, "Start"]]
    ends   <- all_dates[periods[, "End"]]
    
    # Stw?rz ma?? tabelk? i dodaj do g??wnej
    temp_df <- data.frame(
      Series = name,
      xmin = starts,
      xmax = ends,
      ymin = -Inf,  # Prostok?t od samego do?u...
      ymax = Inf    # ...do samej g?ry wykresu
    )
    
    bubble_rects <- rbind(bubble_rects, temp_df)
  }
}
if(nrow(bubble_rects) > 0) {
  bubble_rects$Series <- factor(bubble_rects$Series, levels = keep_order)
}

# Sprawdzamy, czy co? znalaz? (je?eli tak, to b?d? daty)
print(head(bubble_rects))

# jakie jest minimalne okno obserwacji do testu gsadf
T_GSADF <- nrow(data_matrix)
min_window <- floor(T_GSADF * (0.01 + 1.8/sqrt(T_GSADF)))

print(paste("Liczba obserwacji:", T_GSADF))
print(paste("Minimalne okno (w dniach):", min_window))


# wykres do zapisu  

#library(patchwork)

bubble_plot_CO <- ggplot() +
  # WARSTWA 1: Czerwone obszary (ba?ki)
  # Rysujemy je TYLKO je?li bubble_rects nie jest puste
  {if(nrow(bubble_rects) > 0) 
    geom_rect(data = bubble_rects, 
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "red", alpha = 0.3) # alpha to przezroczysto??
  } +
  
  # WARSTWA 2: Ceny (Czarne linie)
  geom_line(data = df_plot, aes(x = Date, y = exp(Price)), color = "black", linewidth = 0.6) +
  
  facet_wrap(~ Series, scales = "free_y", ncol = 2) + 
  
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + # ?adne daty co rok
  labs(title = "",
       #subtitle = ",
       x = "",
       y = "") +
  theme_minimal() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Pochylenie dat, ?eby si? mie?ci?y
  theme(
  strip.text = element_text(size = 14, face = "bold"), # Rozmiar i pogrubienie tytu??w paneli
  #axis.text.x = element_text(angle = 45, hjust = 1)   # Opcjonalne pochylenie dat
)
# zmienic nazw? pliku banki_metal,pdf
ggsave(filename = here("outputs", "figures", "bubble_tests", "bubble_nickel.pdf"), 
       plot = bubble_plot_NI, 
       width = 12, 
       height = 10)

ggsave(filename = here("outputs", "figures", "bubble_tests", "bubble_cobalt.pdf"), 
       plot = bubble_plot_CO, 
       width = 12, 
       height = 10)

ggsave(filename = here("outputs", "figures", "bubble_tests", "bubble_lithium.pdf"), 
       plot = bubble_plot_LI, 
       width = 12, 
       height = 10)
