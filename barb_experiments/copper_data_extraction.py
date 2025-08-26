#!/usr/bin/env python3
"""
Extract copper price data from multiple sources and save as copper_prices_series.csv.
This script replicates the copper data processing from data_overview.ipynb.
"""

import pandas as pd
import os
from pathlib import Path

def extract_copper_data():
    """Extract and merge copper price data from multiple sources."""
    
    print("Loading copper data sources...")
    
    # Load Copper data source #1 - Copper prices per pound from Dailymetalprice
    print("Loading Daily Metal Price copper data...")
    dfcu1 = pd.read_csv('data/Copper_prices_2017-01-01_to_2024-12-31_merged.csv')
    dfcu1['Date'] = pd.to_datetime(dfcu1['Date'])
    dfcu1['Price'] = dfcu1['Price'].astype(float)
    dfcu1 = dfcu1.drop(columns=['Unit'])
    dfcu1 = dfcu1.drop_duplicates(subset=['Date'])
    dfcu1 = dfcu1.sort_values(by='Date')
    print(f"  Daily Metal Price: {dfcu1.shape[0]} rows, {dfcu1['Date'].min()} to {dfcu1['Date'].max()}")
    
    # Load Copper data source #2 - Copper COMEX 1M Future
    print("Loading COMEX copper futures data...")
    dfcu2 = pd.read_csv('data/reuters_data/COMEX Copper Electronic Commodity Future Continuation 1.csv', sep=';')
    dfcu2 = dfcu2.rename(columns={'Exchange Date': 'Date'})
    dfcu2 = dfcu2.rename(columns={'Close': 'Price'})
    dfcu2['Date'] = pd.to_datetime(dfcu2['Date'], format='%d-%b-%Y')
    dfcu2['Price'] = dfcu2['Price'].str.replace(',', '.').astype(float)
    dfcu2 = dfcu2.sort_values(by='Date')
    print(f"  COMEX: {dfcu2.shape[0]} rows, {dfcu2['Date'].min()} to {dfcu2['Date'].max()}")
    
    # Load Copper data source #3 - LME 3M Copper Composite Forward
    print("Loading LME 3M copper data...")
    dfcu3 = pd.read_csv('data/reuters_data/LME 3M Copper Composite Commodity Forward.csv', sep=';')
    dfcu3 = dfcu3.rename(columns={'Exchange Date': 'Date'})
    dfcu3 = dfcu3.rename(columns={'Close': 'Price'})
    dfcu3['Date'] = pd.to_datetime(dfcu3['Date'], format='%d-%b-%Y')
    dfcu3['Price'] = dfcu3['Price'].str.replace('\xa0', '').str.replace(',', '.').astype(float)
    dfcu3 = dfcu3.sort_values(by='Date')
    dfcu3 = dfcu3.iloc[:, :2]
    print(f"  LME 3M: {dfcu3.shape[0]} rows, {dfcu3['Date'].min()} to {dfcu3['Date'].max()}")
    
    # Load Copper data source #4 - SMM Guixi Copper
    print("Loading SMM Guixi copper data...")
    dfcu4 = pd.read_csv('data/reuters_data/SMM Guixi Copper .csv', sep=';')
    dfcu4 = dfcu4.rename(columns={'Exchange Date': 'Date'})
    dfcu4 = dfcu4.rename(columns={'Close': 'Price'})
    dfcu4['Date'] = pd.to_datetime(dfcu4['Date'], format='%d-%b-%Y')
    dfcu4['Price'] = dfcu4['Trade Price'].str.replace('\xa0', '').str.replace(',', '.').astype(float)
    dfcu4 = dfcu4.drop(index=0)
    dfcu4 = dfcu4.sort_values(by='Date')
    dfcu4 = dfcu4.iloc[:, [0, -1]]
    print(f"  SMM Guixi: {dfcu4.shape[0]} rows, {dfcu4['Date'].min()} to {dfcu4['Date'].max()}")
    
    # Load Copper data source #5 - SHFE Copper Future 1M
    print("Loading SHFE copper futures data...")
    dfcu5 = pd.read_csv('data/reuters_data/SHFE Copper Commodity Future Continuation 1.csv', sep=';')
    dfcu5 = dfcu5.rename(columns={'Exchange Date': 'Date'})
    dfcu5 = dfcu5.rename(columns={'Close': 'Price'})
    dfcu5['Date'] = pd.to_datetime(dfcu5['Date'], format='%d-%b-%Y')
    dfcu5['Price'] = dfcu5['Price'].str.replace('\xa0', '').str.replace(',', '.').astype(float)
    dfcu5 = dfcu5.sort_values(by='Date')
    print(f"  SHFE: {dfcu5.shape[0]} rows, {dfcu5['Date'].min()} to {dfcu5['Date'].max()}")
    
    # Load Copper data source #6 - Sprott Copper Miners ETF
    print("Loading Sprott Copper Miners ETF data...")
    dfcu6 = pd.read_csv('data/reuters_data/Sprott Copper Miners ETF.csv', sep=';')
    dfcu6 = dfcu6.rename(columns={'Exchange Date': 'Date'})
    dfcu6 = dfcu6.rename(columns={'Close': 'Price'})
    dfcu6['Date'] = pd.to_datetime(dfcu6['Date'], format='%d-%b-%Y')
    dfcu6['Price'] = dfcu6['Price'].str.replace('\xa0', '').str.replace(',', '.').astype(float)
    dfcu6 = dfcu6.sort_values(by='Date')
    dfcu6 = dfcu6.drop(index=0)
    print(f"  Sprott ETF: {dfcu6.shape[0]} rows, {dfcu6['Date'].min()} to {dfcu6['Date'].max()}")
    
    print("\nMerging all copper data sources...")
    
    # Merge all dataframes on the 'Date' column
    dfcu_merged = pd.merge(dfcu1, dfcu2, on='Date', suffixes=('_Dailymetal', '_COMEX'))
    dfcu_merged = pd.merge(dfcu_merged, dfcu3[['Date', 'Price']], on='Date', how='outer')
    dfcu_merged = dfcu_merged.rename(columns={'Price': 'Price_LME_3M'})
    dfcu_merged = pd.merge(dfcu_merged, dfcu4[['Date', 'Price']], on='Date', how='outer')
    dfcu_merged = dfcu_merged.rename(columns={'Price': 'Price_SMM_Guixi'})
    dfcu_merged = pd.merge(dfcu_merged, dfcu5[['Date', 'Price']], on='Date', how='outer')
    dfcu_merged = dfcu_merged.rename(columns={'Price': 'Price_SHFE'})
    dfcu_merged = pd.merge(dfcu_merged, dfcu6[['Date', 'Price']], on='Date', how='outer')
    dfcu_merged = dfcu_merged.rename(columns={'Price': 'Price_ETF'})
    
    # Convert all price columns to float
    dfcu_merged['Price_Dailymetal'] = dfcu_merged['Price_Dailymetal'].astype(float)
    dfcu_merged['Price_COMEX'] = dfcu_merged['Price_COMEX'].astype(float)
    dfcu_merged['Price_LME_3M'] = dfcu_merged['Price_LME_3M'].astype(float)
    dfcu_merged['Price_SMM_Guixi'] = dfcu_merged['Price_SMM_Guixi'].astype(float)
    dfcu_merged['Price_SHFE'] = dfcu_merged['Price_SHFE'].astype(float)
    dfcu_merged['Price_ETF'] = dfcu_merged['Price_ETF'].astype(float)
    
    # Sort by date and reset index
    dfcu_merged = dfcu_merged.sort_values(by='Date').reset_index(drop=True)
    
    # Remove unwanted columns
    unwanted_columns = ['Net', '%Chg', 'Open', 'Low', 'High', 'Volume', 'Bid', 'Ask']
    columns_to_remove = [col for col in unwanted_columns if col in dfcu_merged.columns]
    if columns_to_remove:
        dfcu_merged = dfcu_merged.drop(columns=columns_to_remove)
        print(f"Removed unwanted columns: {columns_to_remove}")
    
    # Interpolate missing values using linear interpolation
    print("Interpolating missing values...")
    dfcu_merged = dfcu_merged.interpolate(method='linear', limit_direction='both')
    print("Interpolation completed.")
    
    # Round all price columns to 4 decimal places
    print("Rounding price columns to 4 decimal places...")
    price_columns = [col for col in dfcu_merged.columns if col != 'Date']
    dfcu_merged[price_columns] = dfcu_merged[price_columns].round(4)
    print("Rounding completed.")
    
    print(f"\nMerged data shape: {dfcu_merged.shape}")
    print(f"Date range: {dfcu_merged['Date'].min()} to {dfcu_merged['Date'].max()}")
    print(f"Columns: {list(dfcu_merged.columns)}")
    
    # Create output directory
    output_dir = Path('barb_experiments/barb_data')
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Save the merged dataframe
    output_file = output_dir / 'copper_prices_series.csv'
    dfcu_merged.to_csv(output_file, index=False)
    print(f"\nCopper data saved to: {output_file}")
    
    # Create ticker mapping for copper
    ticker_mapping = {
        'Price_Dailymetal': 'CUDALY',      # Daily Metal Copper prices
        'Price_COMEX': 'CUCOMEX',          # COMEX Copper futures
        'Price_LME_3M': 'CULME',           # LME 3M Copper
        'Price_SMM_Guixi': 'CUGUIXI',      # SMM Guixi Copper
        'Price_SHFE': 'CUSHFE',            # SHFE Copper futures
        'Price_ETF': 'CUETFN'              # Sprott Copper ETF
    }
    
    # Create a copy with ticker names
    dfcu_tickers = dfcu_merged.copy()
    dfcu_tickers = dfcu_tickers.rename(columns=ticker_mapping)
    
    # Save the ticker version
    ticker_file = output_dir / 'copper_prices_tickers.csv'
    dfcu_tickers.to_csv(ticker_file, index=False)
    print(f"Copper data with tickers saved to: {ticker_file}")
    
    # Show the mapping for reference
    print(f"\nColumn mapping:")
    for old_name, new_name in ticker_mapping.items():
        print(f"  {old_name} → {new_name}")
    
    # Create plots similar to plot_nickel_series.py
    print("\nCreating copper price plots...")
    try:
        import matplotlib.pyplot as plt
        import seaborn as sns
        
        # Get the ticker columns (excluding Date and ETF)
        ticker_columns = [col for col in dfcu_tickers.columns if col != 'Date' and col != 'CUETFN']
        
        # Create output directory for plots
        plots_dir = output_dir / 'copper_plots'
        plots_dir.mkdir(parents=True, exist_ok=True)
        
        # Set up the plotting style
        plt.style.use('seaborn-v0_8')
        sns.set_palette("husl")
        
        print(f"Creating subplot figure for {len(ticker_columns)} series...")
        
        # Create figure with subplots (2 rows, 3 columns for 5 series)
        fig, axes = plt.subplots(2, 3, figsize=(18, 12))
        fig.suptitle('Copper', fontsize=20, fontweight='bold')
        
        # Flatten axes array for easier indexing
        axes = axes.flatten()
        
        # Colors for each subplot
        colors = ['#4682B4', '#708090', '#2F4F4F', '#36454F', '#1C1C1C']
        
        # Plot each series in its own subplot
        for i, ticker in enumerate(ticker_columns):
            print(f"Plotting {ticker} in subplot {i+1}...")
            
            ax = axes[i]
            
            # Plot the series
            ax.plot(dfcu_tickers['Date'], dfcu_tickers[ticker], linewidth=2, color=colors[i])
            
            # Customize the subplot
            ax.set_title(f'{ticker}', fontsize=14, fontweight='bold')
            ax.set_xlabel('', fontsize=10)
            ax.set_ylabel('', fontsize=10)
            ax.grid(True, alpha=0.3)
            
            # Rotate x-axis labels for better readability
            ax.tick_params(axis='x', rotation=45, labelsize=8)
            ax.tick_params(axis='y', labelsize=8)
        
        # Hide the last unused subplot
        axes[-1].set_visible(False)
        
        # Adjust layout to prevent overlap
        plt.tight_layout()
        plt.subplots_adjust(top=0.93)  # Make room for suptitle
        
        # Save the subplot figure
        subplot_file = plots_dir / 'copper_price_subplots.png'
        plt.savefig(subplot_file, dpi=300, bbox_inches='tight')
        print(f"Subplot figure saved: {subplot_file}")
        
        # Close the subplot figure
        plt.close()
        
        # Create a combined plot showing all series on the same graph
        print("Creating combined plot...")
        fig, ax = plt.subplots(figsize=(14, 8))
        
        # Plot all series on the same graph (excluding ETF)
        for i, ticker in enumerate(ticker_columns):
            ax.plot(dfcu_tickers['Date'], dfcu_tickers[ticker], linewidth=2, label=ticker, color=colors[i])
        
        # Customize the combined plot
        ax.set_title('All Copper Price Series Comparison', fontsize=18, fontweight='bold')
        ax.set_xlabel('Date', fontsize=14)
        ax.set_ylabel('Price', fontsize=14)
        ax.legend(fontsize=12, loc='upper left')
        ax.grid(True, alpha=0.3)
        
        # Rotate x-axis labels
        plt.xticks(rotation=45)
        
        # Tight layout
        plt.tight_layout()
        
        # Save the combined plot
        combined_file = plots_dir / 'copper_price_series.png'
        plt.savefig(combined_file, dpi=300, bbox_inches='tight')
        print(f"Combined plot saved: {combined_file}")
        
        # Close the combined figure
        plt.close()
        
        print(f"Copper plots saved in: {plots_dir}")
        
    except ImportError:
        print("Matplotlib not available - skipping plotting")
    except Exception as e:
        print(f"Error creating plots: {e}")
    
    print("\nCopper data extraction completed successfully!")
    
    return dfcu_merged

if __name__ == "__main__":
    try:
        df = extract_copper_data()
        print("Copper data extraction completed successfully!")
    except Exception as e:
        print(f"Error extracting copper data: {e}")
        import traceback
        traceback.print_exc()
