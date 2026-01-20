install.packages("exuber")
#install_exuberdata()
#install.packages("exuberdata")
library(exuber)
library(urca)
library(ggplot2)
#library(exuberdata)
install.packages("visdat")
library(visdat)
library(tidyverse)



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

getwd()
df_NI <- read.csv("data/ALL_nickel_prices_cubic_spline.csv") #ba?ka na 1 szeregu NIETFN
df_CU <- read.csv("data/ALL_copper_prices_cubic_spline.csv") #nie ma baniek
df_LI <- read.csv("data/ALL_lithium_prices_cubic_spline.csv") #s? ba?ki na LIDAILY i LISAME
df_CO <- read.csv("data/ALL_cobalt_prices_cubic_spline.csv") #s?, du?o i w tym samym czasie


vis_dat(df_NI) # Pokazuje graficznie, gdzie s? braki (NA)
vis_dat(df_CU)
vis_dat(df_LI)
vis_dat(df_CO)


# Konwersja daty i filtrowanie "Cz??ci Wsp?lnej" (dla ka?dego szeregu oddzielnie)
df_clean <- df_NI %>%
  select(-c("NIETFN", "NIWUXI")) %>%
  mutate(Date = as.Date(Date)) %>%  #sprawdzenie czy R rozumie, ?e to daty
  na.omit()                         

df_clean <- df_CO %>%
  select(-c("COSMMS", "COCOMX")) %>%
  mutate(Date = as.Date(Date)) %>%  #sprawdzenie czy R rozumie, ?e to daty
  na.omit()                         

df_clean <- df_CU %>%
  select(-"CUETFC") %>%
  mutate(Date = as.Date(Date)) %>%  #sprawdzenie czy R rozumie, ?e to daty
  na.omit()                         

colnames(df_LI)
df_clean <- df_LI %>%
  select(-c("LILAMC", "LIEALC", "LIEABG")) %>%
  mutate(Date = as.Date(Date)) %>%  #sprawdzenie czy R rozumie, ?e to daty
  na.omit()                         


# Sprawdzenie zakresu dat po oczyszczeniu
print(paste("Pocz?tek analizy:", min(df_clean$Date)))
print(paste("Koniec analizy:", max(df_clean$Date)))
print(paste("Liczba obserwacji:", nrow(df_clean)))
head(df_clean)

# Przygotowanie macierzy do testu
# Wyci?gamy same dane numeryczne (bez kolumny Date)
data_matrix <- df_clean %>% 
  select(-Date) %>% # Usuwamy kolumn? Date, zostaj? tylko szeregi cen
  mutate(across(everything(), log))

# Przypisujemy daty jako nazwy wierszy (dla ?adnych wykres?w)
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
