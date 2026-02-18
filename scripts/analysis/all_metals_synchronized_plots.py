#!/usr/bin/env python3
"""
Generate synchronized plots with missing value indicators for all metals.
Creates 2x3 subplots for each metal showing price series with red markers for missing data.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
import json

# Get the project root directory (two levels up from this script)
PROJECT_ROOT = Path(__file__).parent.parent.parent


def load_ticker_mappings():
    """Load ticker mappings from JSON file."""
    try:
        with open(PROJECT_ROOT / 'config' / 'ticker_mapping.json', 'r') as f:
            return json.load(f)
    except FileNotFoundError:
        print("Warning: ticker_mapping.json not found. Using default mappings.")
        return {}


def get_default_ticker_mapping(metal_name):
    """Get default ticker mapping if JSON not available (not used when JSON exists)."""
    # Fallback only - should use ticker_mapping.json
    return {}


def create_synchronized_missing_value_plot(df, metal_name, output_dir, ticker_mappings):
    """Create a plot showing price series with missing value indicators."""
    try:
        print(f"\nCreating synchronized plot with missing indicators for {metal_name}...")
        
        # Get ticker mapping for this metal (maps original names to ticker codes)
        metal_mappings = ticker_mappings.get(metal_name, {})
        
        # Get all ticker codes that should be excluded
        excluded_tickers = set()
        if metal_mappings:
            # Get ticker values (LISAME, etc.) that should be excluded
            excluded_tickers = {ticker for ticker in metal_mappings.values() if ticker == 'LISAME'}
        
        # Get price columns (CSV already has ticker codes as column names)
        # Exclude Date and any excluded tickers (like LISAME)
        price_cols = [col for col in df.columns 
                     if col != 'Date' and col not in excluded_tickers]
        
        # Limit to 6 for 2x3 grid
        price_cols = price_cols[:6]
        
        if not price_cols:
            print(f"No valid price columns found for {metal_name}")
            return
        
        print(f"Using columns: {price_cols}")
        
        # Create 2x3 subplot grid
        fig, axes = plt.subplots(2, 3, figsize=(18, 12))
        axes = axes.flatten()
        
        # Define blue color palette
        blue_colors = ['#000080', '#0000CD', '#0066CC', '#4169E1', '#1E90FF', '#87CEEB']
        
        # Get global date range
        global_start = df['Date'].min()
        global_end = df['Date'].max()
        
        for i, col in enumerate(price_cols):
            if i >= 6:  # Only plot first 6 series
                break
                
            ax = axes[i]
            
            # Get data for this series
            series_data = df[['Date', col]].copy()
            
            # Find first and last valid observations
            valid_data = series_data[series_data[col].notna()]
            
            if len(valid_data) > 0:
                series_start = valid_data['Date'].min()
                series_end = valid_data['Date'].max()
                
                # Filter to series-specific date range (no extrapolation)
                plot_data = series_data[
                    (series_data['Date'] >= series_start) & 
                    (series_data['Date'] <= series_end)
                ].copy()
                
                # Plot the data
                ax.plot(plot_data['Date'], plot_data[col], 
                       linewidth=2, color=blue_colors[i], alpha=0.8, 
                       marker='o', markersize=2)
                
                # Find missing values
                missing_data = plot_data[plot_data[col].isna()]
                
                if len(missing_data) > 0:
                    # Get current y-axis limits
                    y_lim = ax.get_ylim()
                    y_min = y_lim[0]
                    
                    # Red markers on bottom of plot for missing values
                    ax.scatter(missing_data['Date'], 
                             [y_min] * len(missing_data), 
                             color='red', marker='|', s=75, alpha=0.8,
                             linewidths=1)
                
                # Set title with ticker name
                ticker_label = col  # CSV already has ticker codes as column names
                data_points = len(valid_data)
                
                ax.set_title(f'{ticker_label}', fontsize=11, fontweight='bold')
                    
            else:
                ax.text(0.5, 0.5, 'No Data Available', 
                       horizontalalignment='center', verticalalignment='center',
                       transform=ax.transAxes, fontsize=14, color='red')
                ticker_label = col  # CSV already has ticker codes as column names
                ax.set_title(f'{ticker_label}\nNo Data', fontsize=12, fontweight='bold')
            
            # Set consistent time scale for all subplots
            ax.set_xlim(global_start, global_end)
            ax.set_ylabel('Price', fontsize=10)
            ax.grid(True, alpha=0.3)
            ax.tick_params(axis='x', rotation=45, labelsize=8)
            ax.tick_params(axis='y', labelsize=8)
        
        # Hide unused subplots if any
        for j in range(len(price_cols), len(axes)):
            axes[j].set_visible(False)
        
        # Add overall title
        # fig.suptitle(f'{metal_name.capitalize()} Price Series - Synchronized View with Missing Data Indicators', 
        #             fontsize=16, fontweight='bold', y=0.98)
        
        plt.tight_layout()
        plt.subplots_adjust(top=0.95)
        
        # Save figure
        output_dir.mkdir(parents=True, exist_ok=True)
        sync_file = output_dir / f'{metal_name}_synchronized_missing_indicators.png'
        plt.savefig(sync_file, dpi=300, bbox_inches='tight')
        print(f"Synchronized plot saved: {sync_file}")
        plt.close()
        
    except Exception as e:
        print(f"Error creating synchronized plot for {metal_name}: {e}")
        import traceback
        traceback.print_exc()


def main():
    """Generate synchronized plots for all metals."""
    
    print("="*60)
    print("GENERATING SYNCHRONIZED PLOTS WITH MISSING VALUE INDICATORS")
    print("="*60)
    
    # Load ticker mappings
    ticker_mappings = load_ticker_mappings()
    
    # Define metals
    metals = ['cobalt', 'copper', 'lithium', 'nickel']
    
    # Output directory
    output_dir = PROJECT_ROOT / 'outputs' / 'synchronized_plots'
    output_dir.mkdir(parents=True, exist_ok=True)
    
    for metal in metals:
        try:
            print(f"\n{'='*60}")
            print(f"Processing {metal.upper()}")
            print(f"{'='*60}")
            
            # Load data (outer data shows actual missing values)
            file_path = PROJECT_ROOT / 'data' / f'ALL_{metal}_prices_outer.csv'
            df = pd.read_csv(file_path)
            df['Date'] = pd.to_datetime(df['Date'])
            df = df.sort_values('Date').reset_index(drop=True)
            
            print(f"Loaded {len(df)} rows")
            print(f"Date range: {df['Date'].min()} to {df['Date'].max()}")
            
            # Get columns (excluding Date)
            all_cols = [col for col in df.columns if col != 'Date']
            print(f"Available columns: {all_cols}")
            
            # Create plot
            create_synchronized_missing_value_plot(df, metal, output_dir, ticker_mappings)
            
        except FileNotFoundError:
            print(f"Warning: Data file not found for {metal}")
        except Exception as e:
            print(f"Error processing {metal}: {e}")
            import traceback
            traceback.print_exc()
    
    print(f"\n{'='*60}")
    print("ALL PLOTS COMPLETED")
    print(f"Output directory: {output_dir.absolute()}")
    print(f"{'='*60}")


if __name__ == "__main__":
    main()
