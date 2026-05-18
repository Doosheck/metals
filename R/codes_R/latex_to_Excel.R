# --- SKRYPT NIE WYMAGA ¯ADNYCH PAKIETÓW ---

# 1. Wczytanie pliku LaTeX
sciezka_do_tex <- "R/tables/johansen_results_final.tex" # ZMIEÑ NA SWOJ¥ NAZWÊ PLIKU
linie <- readLines(sciezka_do_tex)

# 2. Znalezienie pocz¹tku i koñca tabeli (œrodowisko tabular)
start <- grep("\\\\begin\\{tabular\\}", linie)
koniec <- grep("\\\\end\\{tabular\\}", linie)

if(length(start) > 0 && length(koniec) > 0) {
  # Wycinamy tylko to, co jest wewn¹trz tabeli
  tabela_linie <- linie[(start[1] + 1):(koniec[1] - 1)]
  
  # 3. Czyszczenie kodu ze znaczników LaTeXa
  # Pozbywamy siê linii oddzielaj¹cych
  tabela_linie <- tabela_linie[!grepl("\\\\hline|\\\\toprule|\\\\midrule|\\\\bottomrule", tabela_linie)]
  
  # Pozbywamy siê znaków nowej linii "\\" z koñca wierszy
  tabela_linie <- gsub("\\\\\\\\", "", tabela_linie)
  
  # 4. Rozbicie tekstu na kolumny (separator to "&")
  lista_wierszy <- strsplit(tabela_linie, "&")
  
  # Usuniêcie zbêdnych spacji
  lista_wierszy <- lapply(lista_wierszy, trimws)
  
  # Konwersja do ramki danych
  df <- as.data.frame(do.call(rbind, lista_wierszy), stringsAsFactors = FALSE)
  
  # Ustawienie nag³ówków z pierwszego wiersza
  colnames(df) <- df[1, ]
  df <- df[-1, ]
  rownames(df) <- NULL
  
  # 5. Zapis do pliku CSV (zoptymalizowanego pod polskiego Excela)
  write.csv2(df, "R/tables/johansen_results_final.csv", row.names = FALSE)
  print("??? Sukces! Plik zapisany jako 'Tohansen_results_final1csv. Mo¿esz go dwukrotnie klikn¹æ i otworzy siê prosto w Excelu!")
  
} else {
  print("??? B³¹d: Nie znaleziono znacznika \\begin{tabular} w pliku.")
}
