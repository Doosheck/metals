#!/usr/bin/env python3
"""
Extract lithium price data from multiple sources and save as lithium_prices_series.csv.
This script mirrors the structure/flow of copper_data_extraction.py for lithium.
"""

import pandas as pd
from pathlib import Path


def extract_lithium_data():
    """Extract and merge lithium price data from multiple sources."""

    print("Loading lithium data sources...")

    # Primary consolidated source for lithium (already harmonized across feeds)
    # Columns expected:
    # Date, Price_DailyMetal, Price_SouthAmericaLOB, Price_COMEXLithiumHydroxide,
    # Price_LithiumAmericasCorp, Price_EastAsiaLithiumCarbonate,
    # Price_EastAsiaLithiumCarbonateBatteryGrade
    print("Loading consolidated lithium data (ALL_lithium_prices_final.csv)...")
    dfl = pd.read_csv('data/ALL_lithium_prices_final.csv')
    dfl['Date'] = pd.to_datetime(dfl['Date'])

    # Keep only the columns we care about and rename for consistency with copper script
    rename_map = {
        'Price_DailyMetal': 'Price_Dailymetal',
        'Price_COMEXLithiumHydroxide': 'Price_COMEX',
        'Price_SouthAmericaLOB': 'Price_SA_LOB',
        'Price_LithiumAmericasCorp': 'Price_LAC',
        'Price_EastAsiaLithiumCarbonate': 'Price_EA_Carbonate',
        'Price_EastAsiaLithiumCarbonateBatteryGrade': 'Price_EA_Carbonate_Batt',
    }
    keep_cols = ['Date'] + list(rename_map.keys())
    existing_cols = [c for c in keep_cols if c in dfl.columns]
    dfl = dfl[existing_cols].rename(columns=rename_map)

    # Convert numeric columns to float, handling strings if needed
    for col in dfl.columns:
        if col == 'Date':
            continue
        dfl[col] = pd.to_numeric(dfl[col], errors='coerce')

    # Sort and de-duplicate
    dfl = dfl.drop_duplicates(subset=['Date']).sort_values(by='Date')

    print(f"  Consolidated lithium: {dfl.shape[0]} rows, {dfl['Date'].min()} to {dfl['Date'].max()}")

    # Remove unwanted metadata columns if they slipped in (matches copper cleanup)
    unwanted_columns = ['Net', '%Chg', 'Open', 'Low', 'High', 'Volume', 'Bid', 'Ask']
    columns_to_remove = [col for col in unwanted_columns if col in dfl.columns]
    if columns_to_remove:
        dfl = dfl.drop(columns=columns_to_remove)
        print(f"Removed unwanted columns: {columns_to_remove}")

    # Interpolate missing values linearly and round
    print("Interpolating missing values...")
    dfl = dfl.interpolate(method='linear', limit_direction='both')
    print("Interpolation completed.")

    print("Rounding price columns to 4 decimal places...")
    price_columns = [col for col in dfl.columns if col != 'Date']
    dfl[price_columns] = dfl[price_columns].round(4)
    print("Rounding completed.")

    print(f"\nMerged data shape: {dfl.shape}")
    print(f"Date range: {dfl['Date'].min()} to {dfl['Date'].max()}")
    print(f"Columns: {list(dfl.columns)}")

    # Create output directory
    output_dir = Path('barb_experiments/barb_data')
    output_dir.mkdir(parents=True, exist_ok=True)

    # Save base series
    out_series_file = output_dir / 'lithium_prices_series.csv'
    dfl.to_csv(out_series_file, index=False)
    print(f"\nLithium data saved to: {out_series_file}")

    # Create tickers mapping similar to copper
    ticker_mapping = {
        'Price_Dailymetal': 'LIDALY',              # Daily Metal lithium
        'Price_COMEX': 'LIHCOMEX',                 # COMEX Lithium Hydroxide
        'Price_SA_LOB': 'LISALOB',                 # South America LOB
        'Price_LAC': 'LILAC',                      # Lithium Americas Corp (equity)
        'Price_EA_Carbonate': 'LIEAC',             # East Asia Lithium Carbonate
        'Price_EA_Carbonate_Batt': 'LIEABG',       # East Asia Li Carbonate Battery Grade
    }

    dfl_tickers = dfl.copy().rename(columns=ticker_mapping)
    out_tickers_file = output_dir / 'lithium_prices_tickers.csv'
    dfl_tickers.to_csv(out_tickers_file, index=False)
    print(f"Lithium data with tickers saved to: {out_tickers_file}")

    print("\nColumn mapping:")
    for old_name, new_name in ticker_mapping.items():
        if old_name in dfl.columns:
            print(f"  {old_name} → {new_name}")

    # Plots akin to copper implementation
    print("\nCreating lithium price plots...")
    try:
        import matplotlib.pyplot as plt
        import seaborn as sns

        plots_dir = output_dir / 'lithium_plots'
        plots_dir.mkdir(parents=True, exist_ok=True)

        # exclude equity by default from per-series subplots only if needed
        ticker_columns = [col for col in dfl_tickers.columns if col != 'Date']

        plt.style.use('seaborn-v0_8')
        sns.set_palette("husl")

        print(f"Creating subplot figure for {len(ticker_columns)} series...")
        n = len(ticker_columns)
        rows = 2
        cols = max(3, (n + rows - 1) // rows)
        fig, axes = plt.subplots(rows, cols, figsize=(6 * cols, 4.5 * rows))
        fig.suptitle('Lithium', fontsize=20, fontweight='bold')

        if hasattr(axes, 'flatten'):
            axes = axes.flatten()
        else:
            axes = [axes]

        colors = [
            '#6A5ACD', '#2E8B57', '#8B0000', '#20B2AA', '#8A2BE2', '#B8860B', '#2F4F4F'
        ]
        while len(colors) < n:
            colors.extend(colors)

        for i, ticker in enumerate(ticker_columns):
            if i >= len(axes):
                break
            ax = axes[i]
            ax.plot(dfl_tickers['Date'], dfl_tickers[ticker], linewidth=2, color=colors[i])
            ax.set_title(f'{ticker}', fontsize=14, fontweight='bold')
            ax.set_xlabel('')
            ax.set_ylabel('')
            ax.grid(True, alpha=0.3)
            ax.tick_params(axis='x', rotation=45, labelsize=8)
            ax.tick_params(axis='y', labelsize=8)

        # Hide unused subplots if any
        for j in range(i + 1, len(axes)):
            axes[j].set_visible(False)

        plt.tight_layout()
        plt.subplots_adjust(top=0.9)

        subplot_file = plots_dir / 'lithium_price_subplots.png'
        plt.savefig(subplot_file, dpi=300, bbox_inches='tight')
        print(f"Subplot figure saved: {subplot_file}")
        plt.close()

        # Combined plot
        print("Creating combined plot...")
        fig, ax = plt.subplots(figsize=(14, 8))
        for i, ticker in enumerate(ticker_columns):
            ax.plot(dfl_tickers['Date'], dfl_tickers[ticker], linewidth=2, label=ticker)
        ax.set_title('All Lithium Price Series Comparison', fontsize=18, fontweight='bold')
        ax.set_xlabel('Date', fontsize=14)
        ax.set_ylabel('Price', fontsize=14)
        ax.legend(fontsize=10, loc='upper left')
        ax.grid(True, alpha=0.3)
        plt.xticks(rotation=45)
        plt.tight_layout()
        combined_file = plots_dir / 'lithium_price_series.png'
        plt.savefig(combined_file, dpi=300, bbox_inches='tight')
        print(f"Combined plot saved: {combined_file}")
        plt.close()

        print(f"Lithium plots saved in: {plots_dir}")
    except ImportError:
        print("Matplotlib not available - skipping plotting")
    except Exception as e:
        print(f"Error creating plots: {e}")

    print("\nLithium data extraction completed successfully!")
    return dfl


if __name__ == "__main__":
    try:
        df = extract_lithium_data()
        print("Lithium data extraction completed successfully!")
    except Exception as e:
        print(f"Error extracting lithium data: {e}")
        import traceback
        traceback.print_exc()


