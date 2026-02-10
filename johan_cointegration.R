install.packages("vars")
library(vars)
library(urca)

# --- 1. Filter Cobalt Series ---
#write_csv(df_final, here("data", "combined_metals_cleaned.csv"))

# Adjust the pattern "CO" to match your specific Cobalt column names
cobalt_data <- df_final %>%
  dplyr::select(Date, c("CODALY", "COLMEX", "COLMEA", "COWUXI")) %>% 
  mutate(across(c(CODALY, COLMEX, COLMEA, COWUXI), ~ log(.x))) %>%
  column_to_rownames("Date")

head(cobalt_data)

# --- 2. Lag Selection ---
# Cointegration tests are highly sensitive to the lag structure

cobalt_lags <- VARselect(cobalt_data, lag.max = 10, type = "const")
best_lag <- cobalt_lags$selection["AIC(n)"]
best_lag

# --- 3. Johansen Test (Trace) ---
# K is the number of lags. ecdet="const" allows for a drift in the price levels

jo_cobalt <- ca.jo(cobalt_data, 
                   type = "trace", 
                   ecdet = "const", 
                   K = best_lag)

# --- 4. Interpretation ---
summary(jo_cobalt)
