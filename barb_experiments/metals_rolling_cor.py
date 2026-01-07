"""
Compute rolling correlation windows for metal time series from ALL_metal_prices_cubic_spline.csv.
"""
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from pathlib import Path
import argparse

def analyze_metal_correlations(csv_path: str, windows=[30, 60], top_n=6, output_dir="plots"):
    """
    Analizuje korelacje ruchome dla dowolnego pliku CSV z kolumną 'Date'.
    """
    path = Path(csv_path)
    print(f"--- Przetwarzanie: {path.name} ---")

    # 1. Wczytanie danych
    try:
        df = pd.read_csv(path, parse_dates=['Date'], index_col='Date')
    except ValueError:
        print(f"Błąd: Nie znaleziono kolumny 'Date' w pliku {path.name}")
        return

    # Konwersja na liczby i usunięcie całkowicie pustych kolumn
    df = df.apply(pd.to_numeric, errors='coerce').dropna(axis=1, how='all')
    
    # --- KLUCZOWA ZMIANA: INTERSECTION ---
    # dropna(how='any') usuwa wiersz, jeśli choć jeden metal ma tam NaN
    rows_before = len(df)
    df = df.dropna(how='any')
    rows_after = len(df)
    
    if df.empty:
        print("BŁĄD: Brak wspólnego okresu dla wszystkich szeregów (intersection jest puste)!")
        return

    # Informacje o danych
    start_date = df.index.min().date()
    end_date = df.index.max().date()
    
    print(f"Wspólny zakres dat: {start_date} do {end_date}")
    print(f"Liczba obserwacji: {rows_after} (usunięto {rows_before - rows_after} wierszy z brakami)")

    # 2. Obliczenie zwrotów (Returns)
    # Nie robimy tu globalnego dropna(), aby zachować dane, gdy szeregi mają różne długości
    returns = df.pct_change()

    # 3. Znalezienie najsilniej skorelowanych par (statycznie)
    # Tworzymy macierz korelacji, usuwamy duplikaty i przekątną (wartości 1.0)
    corr_matrix = returns.corr().abs()
    mask = np.triu(np.ones(corr_matrix.shape), k=1).astype(bool)
    top_pairs = corr_matrix.where(mask).unstack().dropna().nlargest(top_n)
    
    if top_pairs.empty:
        print("Brak wystarczających danych do obliczenia korelacji.")
        return

    print(f"Top {top_n} par:")
    print(top_pairs.to_string())

    # 4. Rysowanie wykresów
    ncols = 2
    nrows = int(np.ceil(len(top_pairs) / ncols))
    fig, axes = plt.subplots(nrows, ncols, figsize=(12, 4 * nrows), sharex=True)
    axes = axes.flatten()
    sns.set_style('whitegrid')
    colors = ['#191970', '#8B0000', '#006400'] # Kolory dla okien

    for idx, ((col1, col2), static_corr) in enumerate(top_pairs.items()):
        ax = axes[idx]
        
        # Obliczanie korelacji w oknie ruchomym
        for i, window in enumerate(windows):
            # Rolling corr automatycznie dopasowuje daty po indeksie
            roll_corr = returns[col1].rolling(window).corr(returns[col2])
            ax.plot(roll_corr.index, roll_corr, label=f"{window}D", color=colors[i % len(colors)], lw=1.5)

        ax.set_title(f"{col1} vs {col2} (Corr: {static_corr:.2f})")
        ax.axhline(0, color='black', lw=0.8, alpha=0.5)
        ax.legend(loc='lower left', fontsize='large')
        ax.set_ylim(-1.1, 1.1)

    # Ukrycie pustych wykresów
    for idx in range(len(top_pairs), len(axes)):
        axes[idx].axis('off')

    plt.tight_layout()
    #fig.suptitle(f"Rolling Correlations: {path.stem}", y=1.02, fontsize=14)
    
    # --- ZAPIS Z NOWĄ NAZWĄ (Poprawiona sekcja) ---
    
    # 1. Najpierw definiujemy folder wyjściowy (to tutaj był błąd "not defined")
    out_dir_path = Path(output_dir)
    out_dir_path.mkdir(parents=True, exist_ok=True)
    
    # 2. Logika skracania nazwy (wyciągamy drugie słowo z nazwy pliku CSV)
    filename_parts = path.stem.split('_')
    
    if len(filename_parts) > 1:
        metal_name = filename_parts[1] 
    else:
        metal_name = path.stem 

    # 3. Sklejenie wszystkiego w całość
    out_filename = f"{metal_name}_rolling_corr.png"
    out_path = out_dir_path / out_filename

    plt.savefig(out_path, dpi=300, bbox_inches='tight')
    print(f"Wykres zapisano: {out_path}\n")
    plt.close()

if __name__ == "__main__":
    # Przykład użycia: python script.py data/lithium.csv data/copper.csv
    # Lub domyślne uruchomienie w kodzie
    
    # Możesz podać listę plików ręcznie tutaj:
    files_to_process = [
        #"data/ALL_lithium_prices_interpolated.csv",
         "data/ALL_copper_prices_cubic_spline.csv" 
    ]

    # Tutaj definiujemy folder zapisu (względem miejsca uruchomienia skryptu)
    # Zakładając, że jesteś w Metals/metals, ta ścieżka utworzy odpowiednią strukturę
    target_directory = "barb_experiments/plots_moving_cor"

    for file in files_to_process:
        try:
            # POPRAWKA: Przekazujemy output_dir do funkcji
            analyze_metal_correlations(
                file, 
                windows=[30, 90], 
                output_dir=target_directory  # <--- TO BYŁO POTRZEBNE
            )
        except FileNotFoundError:
            print(f"Błąd: Nie znaleziono pliku {file}")
        except Exception as e:
            print(f"Błąd przy przetwarzaniu {file}: {e}")