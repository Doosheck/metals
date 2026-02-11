# --- 1. Load Libraries ---
install.packages("exuber")
install.packages("visdat")
install.packages("here")
#install.packages("exuberdata")
library(tidyverse)
library(exuber)
library(visdat)
library(here) # Critical for OneDrive/Project relative paths


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
vis_dat(df_NI) + labs(title = "Nickel")
vis_dat(df_CU) + labs(title = "Copper")
vis_dat(df_LI) + labs(title = "Lithium")
vis_dat(df_CO) + labs(title = "Cobalt")



# --- 2.1. Cleaning Individual Series ---

# # Nickel: Remove problematic columns and NA rows
# df_NI_clean <- df_NI %>%
#   select(Date, everything(), -any_of(c("NIETFN"))) %>%
#   mutate(Date = as.Date(Date)) %>%
#   na.omit()
# 
# # Copper: Remove ETFC column
# df_CU_clean <- df_CU %>%
#   select(Date, everything(), -any_of(c("CUETFC"))) %>%
#   mutate(Date = as.Date(Date)) %>%
#   na.omit()
# 
# # Lithium: Remove multiple columns
# df_LI_clean <- df_LI %>%
#   select(Date, everything(), -any_of(c("LILAMC", "LIEALC", "LIEABG"))) %>%
#   mutate(Date = as.Date(Date)) %>%
#   na.omit()
# 
# # Cobalt: Remove specific columns
# df_CO_clean <- df_CO %>%
#   select(Date, everything(), -any_of(c("COSMMS", "COCOMX"))) %>%
#   mutate(Date = as.Date(Date)) %>%
#   na.omit()

# --- 2.2. Selection ---
# Set the metals you want to include based on your vis_dat results.
# Simply remove a name from this vector to exclude it (e.g., c("NI", "CU", "CO"))

target_metals <- c("NI", "CU", "LI", "CO")

# --- 2.3. Synchronizing Overlapping Dates ---

# Define your target start and end date
# Required End: Drops series that end too early (e.g., Dec 2024)
# Required Start: Drops series that start too late (e.g., July 2021)

# Define your target window

required_end_date   <- as.Date("2025-07-21")
required_start_date <- as.Date("2021-07-19")

# These are the ONLY columns we explicitly discard
drop_map <- list(
  NI = c("NIETFN"),
  CU = c("CUETFC"),
  LI = c("LILAMC", "LIEALC", "LIEABG"),
  CO = c("COSMMS", "COCOMX")
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
message("--- Final Results ---")
message("Metals included: ", paste(names(list_filtered), collapse = ", "))
message("Series included: ", paste(names(df_final)[-1], collapse = ", "))
message("Total columns in df_final: ", ncol(df_final) - 1) 

print(paste("Analysis starts on:", min(df_final$Date)))
print(paste("Analysis ends on:", max(df_final$Date)))
print(paste("Total observations (rows):", nrow(df_final)))
head(df_final)


# # --- 2.5a Save the Combined File ---
# # This saves the synchronized 'df_final' containing all selected metals
write_csv(df_final, here("data", "combined_metals_cleaned.csv"))
# message("Saved combined file: combined_metals_cleaned.csv")

# --- 4.1. Matrix Preparation ---
# exuber requires a numeric matrix. We log-transform prices here.
data_matrix <- df_final %>%
  select(-Date) %>%
  mutate(across(everything(), log)) %>%
  as.matrix()

# Assign dates as character row names for tracking
rownames(data_matrix) <- as.character(df_final$Date)

# --- 4.2. Run RADF Estimation ---
# This calculates SADF and GSADF statistics for all series in the matrix.
# Note: This might take a moment depending on the number of series.
est_results <- radf(data_matrix)

# --- 4.3. Calculate Critical Values ---
# Since we have a specific sample size (n), we generate Monte Carlo 
# critical values to perform the statistical test.

# for the first time calculate and save
n_obs <- nrow(data_matrix)
mc_cv <- radf_mc_cv(n = n_obs, seed = 123) # Seed ensures reproducibility
mc_cv
saveRDS(mc_cv, "mc_cv.rds")
# for the next time:
mc_cv <- readRDS("mc_cv.rds")

# --- 4.4. Summary of Results ---
# This displays which series exhibit evidence of speculative bubbles
summary(est_results, cv = mc_cv)


#---- Bubbles plots and DV ----

# --- 5.1. Extract Bubble Information ---

# 1. Identify bubble windows for rectangles (start/end dates)
bubble_dates <- datestamp(est_results, cv = mc_cv)

# 2. Create 0-1 Dummies manually from datestamp results
# Initialize a dataframe with all zeros
series_names <- names(df_final)[-1]  # Exclude Date column
bubble_dummies <- df_final %>% select(Date)

# Add a column for each series, initialized to 0
for (series_name in series_names) {
  bubble_dummies[[series_name]] <- 0L
}

# Fill in 1s where bubbles are detected
for (series_name in names(bubble_dates)) {
  periods <- bubble_dates[[series_name]]
  
  if (!is.null(periods) && nrow(periods) > 0) {
    # For each bubble period, set the corresponding rows to 1
    for (i in 1:nrow(periods)) {
      start_idx <- periods[i, "Start"]
      end_idx <- periods[i, "End"]
      bubble_dummies[[series_name]][start_idx:end_idx] <- 1L
    }
  }
}

# --- 5.2. Prepare Plotting Data ---

# Create a dataframe with rectangle coordinates for ggplot
bubble_rects <- map_df(names(bubble_dates), function(name) {
  periods <- bubble_dates[[name]]
  # Check if periods exist and have rows
  if (!is.null(periods) && nrow(periods) > 0) {
    tibble(
      Series = name,
      xmin = df_final$Date[periods[, "Start"]],
      xmax = df_final$Date[periods[, "End"]],
      ymin = -Inf, 
      ymax = Inf
    )
  } else {
    # Return empty tibble with correct structure
    tibble(Series = character(), xmin = as.Date(character()), 
           xmax = as.Date(character()), ymin = numeric(), ymax = numeric())
  }
})

# Identify series that actually contained at least one bubble period
active_series <- if(nrow(bubble_rects) > 0) unique(bubble_rects$Series) else character(0)

# --- 5.3. Final Plotting & Saving ---

# Filter price data for active series only and convert to long format
df_plot_filtered <- df_final %>%
  pivot_longer(-Date, names_to = "Series", values_to = "Price") %>%
  filter(Series %in% active_series)

# Create the plot
bubble_plot <- ggplot() +
  # Layer 1: Shaded grey areas representing bubble periods
  {if(nrow(bubble_rects) > 0) {
    geom_rect(data = bubble_rects, 
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "yellow2", alpha = 0.5)
  }} +
  # Layer 2: Black price lines
  {if(nrow(df_plot_filtered) > 0) {
    geom_line(data = df_plot_filtered, aes(x = Date, y = Price), color = "black")
  }} +
  facet_wrap(~ Series, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(title = "", 
       x = NULL, y = "Log Price")

# --- 5.4. Export Results ---

# Save the dummy variables for the GARCH model
write_csv(bubble_dummies, here("data", "bubble_dummies.csv"))

# Save the plot as a PDF (only if there are active series)
if(length(active_series) > 0) {
  ggsave(here("graphsR", "bubbles_detected_only.pdf"), bubble_plot, 
         width = 10, height = 8)
  message("Plot saved successfully")
} else {
  message("No bubbles detected - no plot generated")
}

print(bubble_plot)

# --- 5.5. Final Verification ---
message("--- Final Verification ---")
message("Total series in dataset: ", length(series_names))
message("Series with detected bubbles: ", length(active_series))
if(length(active_series) > 0) {
  message("Bubble series: ", paste(active_series, collapse = ", "))
}

# Display summary of bubble dummies
message("\nBubble dummy summary (first 10 rows):")
print(head(bubble_dummies, 10))

message("\nBubble periods per series (column sums):")
col_sums <- colSums(bubble_dummies[, -1])  # Exclude Date column
print(col_sums[col_sums > 0])  # Only show series with bubbles

message("\nUnique values in dummy columns:")
unique_vals <- unique(unlist(bubble_dummies[, -1]))
message("Values found: ", paste(unique_vals, collapse = ", "))


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

# Save the dummy variables for the GARCH model
write_csv(bubble_dummies, here("data", "bubble_dummies.csv"))

# Save the plot as a PDF
ggsave(here("graphsR", "bubbles_detected_only.pdf"), bubble_plot, width = 10, height = 8)

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
  ggsave(here("graphsR", "bubbles_detected_color_coded.pdf"), bubble_plot, 
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
ggsave(filename = "graphsR/bubble_nickel.pdf", 
       plot = bubble_plot_NI, 
       width = 12, 
       height = 10)

ggsave(filename = "graphsR/bubble_cobalt.pdf", 
       plot = bubble_plot_CO, 
       width = 12, 
       height = 10)

ggsave(filename = "graphsR/bubble_lithium.pdf", 
       plot = bubble_plot_LI, 
       width = 12, 
       height = 10)
