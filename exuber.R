install.packages("exuber")
#install_exuberdata()
install.packages("exuberdata")
library(exuber)
library(urca)
library(ggplot2)
library(exuberdata)


#---- example for the package----
# 1. Run the test on your time series (e.g., 'sim_data')
rsim <- radf(sim_data)
rsim_df <- unlist(rsim)
head(sim_data)
plot(rsim_df)
head(rsim_df)
# 2. Get critical values (95% confidence) to see if a bubble exists
summary(rsim)

# 3. If a bubble is detected, find the dates
dates <- datestamp(rsim)
autoplot(dates)

## example 2:  
# 1. Run the master function ONCE
est <- radf(sim_data)

# 2. When diagnosing, you can specify which method to use
# Default is GSADF (recommended)
summary(est) 
str(est)

# If you specifically wanted SADF results, you would do:
summary(est, option = "sadf")

# Identify the bubble periods
dstamp <- datestamp(est)
str(dstamp)

# Optional: View the specific dates in the console
print(dstamp)
autoplot(dstamp)
autoplot(est)


#---- an application to metals----
library(tidyverse)

getwd()
df_NI <- read.csv("data/ALL_nickel_prices_cubic_spline.csv") #bañka na 1 szeregu
#df_NI <- read.csv("data/ALL_copper_prices_cubic_spline.csv") #nie ma baniek
#df_NI <- read.csv("data/ALL_lithium_prices_cubic_spline.csv") #s¹ bañki na LIDAILY i LISAME
#df_NI <- read.csv("data/ALL_cobalt_prices_cubic_spline.csv") #s¹, du¿o i w tym samym czasie

# Konwersja daty i filtrowanie "Czêœci Wspólnej"
df_NI_clean <- df_NI %>%
  mutate(Date = as.Date(Date)) %>%  # Upewnij siê, ¿e R rozumie, ¿e to daty
  na.omit()                         # KLUCZOWE: Usuwa wiersze z jakimkolwiek brakiem (NA)

# Sprawdzenie zakresu dat po oczyszczeniu
print(paste("Pocz¹tek analizy:", min(df_NI_clean$Date)))
print(paste("Koniec analizy:", max(df_NI_clean$Date)))
print(paste("Liczba obserwacji:", nrow(df_NI_clean)))

# Przygotowanie macierzy do testu
# Wyci¹gamy same dane numeryczne (bez kolumny Date)
data_matrix <- df_NI_clean %>% 
  select(-Date) %>% # Usuwamy kolumnê Date, zostaj¹ tylko szeregi cen
  mutate(across(everything(), log))

# Przypisujemy daty jako nazwy wierszy (dla ³adnych wykresów)
rownames(data_matrix) <- as.character(df_NI_clean$Date)

head(data_matrix)

# Uruchomienie testu (GSADF, SADF, ADF w jednym)
# To mo¿e chwilê potrwaæ w zale¿noœci od d³ugoœci szeregu
est_results <- radf(data_matrix)

# Wyœwietlenie podsumowania (czy wykryto bañki?)
summary(est_results) #je¿eli pojawia siê b³¹d, move forward

# problem z instalacj¹
# exuber::install_exuberdata()
library(exuberdata)
est_results <- radf(data_matrix)


# exuberdata nie chce siê zaladowaæ
# liczymy wartoœci krytyczne (zastêpuje to pakiet exuberdata)
# To mo¿e potrwaæ kilka-kilkanaœcie sekund
wartosci_krytyczne <- radf_mc_cv(n = nrow(data_matrix)) 

# 2. Wyznacz daty i podaj wartoœci krytyczne 
# Dziêki temu funkcja nie bêdzie szukaæ brakuj¹cego pakietu
bubble_dates <- datestamp(est_results, cv = wartosci_krytyczne)
#bubble_dates <- datestamp(est_results)

# o ile s¹ jakieœ bañki, to tu je mo¿na narysowaæ
autoplot(bubble_dates) #+
  #facet_wrap(~ series, scales = "free_y") +
  #labs(title = "Wykryte bañki spekulacyjne na rynkach niklu") +
  #theme_minimal()

# Poka¿ daty 
print(bubble_dates)

# wykres w ggplot (zamiast autoplot)
keep_order <- colnames(data_matrix)

df_plot <- as.data.frame(data_matrix) %>%
  rownames_to_column("Date") %>%
  mutate(Date = as.Date(Date)) %>%
  pivot_longer(-Date, names_to = "Series", values_to = "Price")%>%
  mutate(Series = factor(Series, levels = keep_order))

# --- B. PRZYGOTOWANIE BANIEK (CZERWONE PROSTOK¥TY) ---
# To jest ten "trudny" moment, który robimy rêcznie.
# Pêtla przejdzie przez Twoje wyniki i zamieni numery wierszy na prawdziwe daty.

bubble_rects <- data.frame() # Pusty kontener na wyniki

# Pobieramy wektor prawdziwych dat z Twoich danych
all_dates <- as.Date(rownames(data_matrix))

# Pêtla po ka¿dym szeregu (NIDALY, NILMEX itd.)
for (name in names(bubble_dates)) {
  
  # Pobierz tabelê start/koniec dla danego metalu
  periods <- bubble_dates[[name]]
  
  # Jeœli wykryto jakieœ bañki (tabela nie jest pusta)
  if (nrow(periods) > 0) {
    
    # Zamieñ numery wierszy na Daty
    starts <- all_dates[periods[, "Start"]]
    ends   <- all_dates[periods[, "End"]]
    
    # Stwórz ma³¹ tabelkê i dodaj do g³ównej
    temp_df <- data.frame(
      Series = name,
      xmin = starts,
      xmax = ends,
      ymin = -Inf,  # Prostok¹t od samego do³u...
      ymax = Inf    # ...do samej góry wykresu
    )
    
    bubble_rects <- rbind(bubble_rects, temp_df)
  }
}
if(nrow(bubble_rects) > 0) {
  bubble_rects$Series <- factor(bubble_rects$Series, levels = keep_order)
}

# SprawdŸmy, czy coœ znalaz³ (powinny tu byæ daty)
print(head(bubble_rects))

# jakie jest minimalne okno obserwacji do testu gsadf
T_GSADF <- nrow(data_matrix)
min_window <- floor(T_GSADF * (0.01 + 1.8/sqrt(T_GSADF)))

print(paste("Liczba obserwacji:", T_GSADF))
print(paste("Minimalne okno (w dniach):", min_window))


# wykres do zapisu - 3 miejsca do zmiany: nazwa wykresu tu i w ggsave i nazwa pliku 
bubble_NI <- ggplot() +
  # WARSTWA 1: Czerwone obszary (bañki)
  # Rysujemy je TYLKO jeœli bubble_rects nie jest puste
  {if(nrow(bubble_rects) > 0) 
    geom_rect(data = bubble_rects, 
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "red", alpha = 0.3) # alpha to przezroczystoœæ
  } +
  
  # WARSTWA 2: Ceny (Czarne linie)
  geom_line(data = df_plot, aes(x = Date, y = exp(Price)), color = "black", linewidth = 0.6) +
  
  facet_wrap(~ Series, scales = "free_y", ncol = 2) + 
  
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + # £adne daty co rok
  labs(title = "",
       #subtitle = ",
       x = "",
       y = "") +
  theme_minimal() #+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Pochylenie dat, ¿eby siê mieœci³y

ggsave(filename = "graphsR/banki_nikiel.pdf", 
       plot = bubble_NI, 
       width = 12, 
       height = 10)
