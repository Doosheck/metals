#!/usr/bin/env python3
"""
Extract nickel price data from multiple sources and save as nickel_prices_series.csv.
This script mirrors the structure/flow of lithium_data_extraction.py for nickel.
Enhanced with missing value analysis and smart plotting.
"""

import pandas as pd
import numpy as np
import json
from pathlib import Path


def analyze_missing_data(df, name="Dataset"):
    """Analyze and report missing data patterns."""
    print(f"\n=== Missing Data Analysis for {name} ===")
    
    # Overall statistics
    total_cells = df.shape[0] * (df.shape[1] - 1)  # Exclude Date column
    missing_cells = df.drop(columns=['Date']).isnull().sum().sum()
    completeness_pct = ((total_cells - missing_cells) / total_cells) * 100
    
    print(f"Total data points: {total_cells:,}")
    print(f"Missing data points: {missing_cells:,}")
    print(f"Data completeness: {completeness_pct:.2f}%")
    
    # Per-column analysis
    print(f"\nMissing data by column:")
    price_cols = [col for col in df.columns if col != 'Date']
    for col in price_cols:
        missing_count = df[col].isnull().sum()
        missing_pct = (missing_count / len(df)) * 100
        available_count = len(df) - missing_count
        print(f"  {col}: {missing_count:,} missing ({missing_pct:.1f}%), {available_count:,} available")
    
    # Date range analysis per column
    print(f"\nData availability by date range:")
    for col in price_cols:
        valid_data = df[df[col].notna()]
        if len(valid_data) > 0:
            start_date = valid_data['Date'].min()
            end_date = valid_data['Date'].max()
            print(f"  {col}: {start_date.strftime('%Y-%m-%d')} to {end_date.strftime('%Y-%m-%d')}")
        else:
            print(f"  {col}: No valid data")
    
    return {
        'total_cells': total_cells,
        'missing_cells': missing_cells,
        'completeness_pct': completeness_pct,
        'column_stats': {col: df[col].isnull().sum() for col in price_cols}
    }


def create_missing_data_heatmap(df, output_dir):
    """Create a heatmap showing missing data patterns."""
    try:
        import matplotlib.pyplot as plt
        import seaborn as sns
        
        print("Creating missing data heatmap...")
        
        # Prepare data for heatmap (exclude Date column)
        price_cols = [col for col in df.columns if col != 'Date']
        missing_matrix = df[price_cols].isnull()
        
        # Create figure
        fig, ax = plt.subplots(figsize=(12, 8))
        
        # Create heatmap
        sns.heatmap(missing_matrix.T, 
                   cmap='RdYlBu_r', 
                   cbar_kws={'label': 'Missing Data'}, 
                   yticklabels=price_cols,
                   xticklabels=False,
                   ax=ax)
        
        ax.set_title('Missing Data Pattern in Nickel Price Series', fontsize=16, fontweight='bold')
        ax.set_xlabel('Time Sequence', fontsize=12)
        ax.set_ylabel('Price Series', fontsize=12)
        
        # Add date range information
        date_range = f"Date Range: {df['Date'].min().strftime('%Y-%m-%d')} to {df['Date'].max().strftime('%Y-%m-%d')}"
        ax.text(0.02, 0.98, date_range, transform=ax.transAxes, 
                bbox=dict(boxstyle="round,pad=0.3", facecolor="white", alpha=0.8),
                verticalalignment='top', fontsize=10)
        
        plt.tight_layout()
        heatmap_file = output_dir / 'nickel_missing_data_heatmap.png'
        plt.savefig(heatmap_file, dpi=300, bbox_inches='tight')
        print(f"Missing data heatmap saved: {heatmap_file}")
        plt.close()
        
    except Exception as e:
        print(f"Error creating missing data heatmap: {e}")


def create_smart_plots(df, output_dir):
    """Create plots that intelligently handle missing values."""
    try:
        import matplotlib.pyplot as plt
        import seaborn as sns
        
        print("Creating smart plots with missing value handling...")
        
        plots_dir = output_dir / 'nickel_plots'
        plots_dir.mkdir(parents=True, exist_ok=True)
        
        price_cols = [col for col in df.columns if col != 'Date']
        
        # 1. Individual series plots (only plot non-missing values)
        print("Creating individual series plots...")
        n_cols = len(price_cols)
        rows = 2
        cols = max(3, (n_cols + rows - 1) // rows)
        
        fig, axes = plt.subplots(rows, cols, figsize=(6 * cols, 4.5 * rows))
        fig.suptitle('Nickel Price Series (Missing Values Excluded)', fontsize=20, fontweight='bold')
        
        if hasattr(axes, 'flatten'):
            axes = axes.flatten()
        else:
            axes = [axes]
        
        colors = ['#6A5ACD', '#2E8B57', '#8B0000', '#20B2AA', '#8A2BE2', '#B8860B', '#2F4F4F']
        while len(colors) < n_cols:
            colors.extend(colors)
        
        for i, col in enumerate(price_cols):
            if i >= len(axes):
                break
            
            ax = axes[i]
            
            # Filter out missing values
            valid_data = df[df[col].notna()]
            
            if len(valid_data) > 0:
                ax.plot(valid_data['Date'], valid_data[col], 
                       linewidth=2, color=colors[i], marker='o', markersize=1)
                
                # Add data availability info
                data_points = len(valid_data)
                start_date = valid_data['Date'].min().strftime('%Y-%m-%d')
                end_date = valid_data['Date'].max().strftime('%Y-%m-%d')
                
                ax.set_title(f'{col}\n{data_points} points: {start_date} to {end_date}', 
                           fontsize=12, fontweight='bold')
            else:
                ax.text(0.5, 0.5, 'No Data Available', 
                       horizontalalignment='center', verticalalignment='center',
                       transform=ax.transAxes, fontsize=14, color='red')
                ax.set_title(f'{col}\nNo Data', fontsize=12, fontweight='bold')
            
            ax.set_xlabel('')
            ax.set_ylabel('Price')
            ax.grid(True, alpha=0.3)
            ax.tick_params(axis='x', rotation=45, labelsize=8)
            ax.tick_params(axis='y', labelsize=8)
        
        # Hide unused subplots
        for j in range(i + 1, len(axes)):
            axes[j].set_visible(False)
        
        plt.tight_layout()
        plt.subplots_adjust(top=0.9)
        
        smart_subplot_file = plots_dir / 'nickel_smart_subplots.png'
        plt.savefig(smart_subplot_file, dpi=300, bbox_inches='tight')
        print(f"Smart subplot figure saved: {smart_subplot_file}")
        plt.close()
        
        # 2. Overlapping series plot (only where data exists)
        print("Creating overlapping series plot...")
        fig, ax = plt.subplots(figsize=(16, 10))
        
        for i, col in enumerate(price_cols):
            valid_data = df[df[col].notna()]
            if len(valid_data) > 0:
                ax.plot(valid_data['Date'], valid_data[col], 
                       linewidth=2, label=f"{col} ({len(valid_data)} pts)", 
                       color=colors[i], alpha=0.8)
        
        ax.set_title('Nickel Price Series Comparison (Missing Values Excluded)', 
                    fontsize=18, fontweight='bold')
        ax.set_xlabel('Date', fontsize=14)
        ax.set_ylabel('Price', fontsize=14)
        ax.legend(fontsize=10, loc='upper left', bbox_to_anchor=(1.02, 1))
        ax.grid(True, alpha=0.3)
        plt.xticks(rotation=45)
        plt.tight_layout()
        
        smart_combined_file = plots_dir / 'nickel_smart_combined.png'
        plt.savefig(smart_combined_file, dpi=300, bbox_inches='tight')
        print(f"Smart combined plot saved: {smart_combined_file}")
        plt.close()
        
        # 3. Data availability timeline
        print("Creating data availability timeline...")
        fig, ax = plt.subplots(figsize=(16, 8))
        
        # Create a binary matrix for data availability
        availability_matrix = df[price_cols].notna().astype(int)
        
        # Plot each series as a horizontal bar showing data availability
        y_positions = range(len(price_cols))
        
        for i, col in enumerate(price_cols):
            # Find continuous segments of data
            data_available = availability_matrix[col].values
            dates = df['Date'].values
            
            # Find start and end of data segments
            diff = np.diff(np.concatenate(([0], data_available, [0])))
            starts = np.where(diff == 1)[0]
            ends = np.where(diff == -1)[0]
            
            for start, end in zip(starts, ends):
                ax.barh(i, (dates[end-1] - dates[start]).astype('timedelta64[D]').astype(int), 
                       left=dates[start], height=0.6, 
                       color=colors[i], alpha=0.7, edgecolor='black', linewidth=0.5)
        
        ax.set_yticks(y_positions)
        ax.set_yticklabels(price_cols)
        ax.set_xlabel('Date', fontsize=14)
        ax.set_title('Data Availability Timeline for Nickel Price Series', 
                    fontsize=16, fontweight='bold')
        ax.grid(True, alpha=0.3, axis='x')
        
        plt.tight_layout()
        availability_file = plots_dir / 'nickel_data_availability.png'
        plt.savefig(availability_file, dpi=300, bbox_inches='tight')
        print(f"Data availability timeline saved: {availability_file}")
        plt.close()
        
        # 4. Missing data summary bar chart
        print("Creating missing data summary chart...")
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 6))
        
        # Missing counts
        missing_counts = [df[col].isnull().sum() for col in price_cols]
        available_counts = [len(df) - missing for missing in missing_counts]
        
        x_pos = np.arange(len(price_cols))
        
        ax1.bar(x_pos, missing_counts, color='red', alpha=0.7, label='Missing')
        ax1.bar(x_pos, available_counts, bottom=missing_counts, color='green', alpha=0.7, label='Available')
        ax1.set_xlabel('Price Series')
        ax1.set_ylabel('Number of Data Points')
        ax1.set_title('Data Completeness by Series', fontweight='bold')
        ax1.set_xticks(x_pos)
        ax1.set_xticklabels(price_cols, rotation=45, ha='right')
        ax1.legend()
        ax1.grid(True, alpha=0.3, axis='y')
        
        # Percentage completeness
        completeness_pcts = [(available / len(df)) * 100 for available in available_counts]
        
        bars = ax2.bar(x_pos, completeness_pcts, color=colors[:len(price_cols)], alpha=0.7)
        ax2.set_xlabel('Price Series')
        ax2.set_ylabel('Completeness (%)')
        ax2.set_title('Data Completeness Percentage', fontweight='bold')
        ax2.set_xticks(x_pos)
        ax2.set_xticklabels(price_cols, rotation=45, ha='right')
        ax2.set_ylim(0, 100)
        ax2.grid(True, alpha=0.3, axis='y')
        
        # Add percentage labels on bars
        for bar, pct in zip(bars, completeness_pcts):
            height = bar.get_height()
            ax2.text(bar.get_x() + bar.get_width()/2., height + 1,
                    f'{pct:.1f}%', ha='center', va='bottom', fontsize=10)
        
        plt.tight_layout()
        summary_file = plots_dir / 'nickel_missing_data_summary.png'
        plt.savefig(summary_file, dpi=300, bbox_inches='tight')
        print(f"Missing data summary saved: {summary_file}")
        plt.close()
    except ImportError:
        print("Matplotlib/seaborn not available - skipping smart plotting")


def create_synchronized_missing_value_plot(df, output_dir):
    """Create a plot showing all series on same time scale with missing value indicators."""
    try:
        import matplotlib.pyplot as plt
        import seaborn as sns
        
        print("Creating synchronized plot with missing value indicators...")
        
        plots_dir = output_dir / 'nickel_plots'
        plots_dir.mkdir(parents=True, exist_ok=True)
        
        # Define the ticker mapping for labels
        ticker_mapping = {
            "Price_DailyMetal": "NIDALY",
            "Price_LME": "NILMEX",
            "Price_ETF": "NIETFN",
            "Price_SHFE": "NISHFE",
            "Price_WUXI": "NIWUXI",
            "Price_India": "NIINDA"
        }
        
        # Only keep columns that exist in df and limit to 6 for 2x3 grid
        price_cols = [col for col in ticker_mapping.keys() if col in df.columns][:6]
        
        # Create 2x3 subplot grid for exactly 6 plots
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
            
            # Find first and last valid observations for this series
            valid_data = series_data[series_data[col].notna()]
            
            if len(valid_data) > 0:
                series_start = valid_data['Date'].min()
                series_end = valid_data['Date'].max()
                
                # Filter to series-specific date range (no extrapolation)
                plot_data = series_data[
                    (series_data['Date'] >= series_start) & 
                    (series_data['Date'] <= series_end)
                ].copy()
                
                # Plot the main line (matplotlib will automatically skip NaN values)
                ax.plot(plot_data['Date'], plot_data[col], 
                       linewidth=2, color=blue_colors[i], alpha=0.8, 
                       marker='o', markersize=2, label='Data Points')
                
                # Mark missing values between first and last observation
                missing_in_range = plot_data[plot_data[col].isna()]
                if len(missing_in_range) > 0:
                    # Red dots on x-axis for missing values
                    y_min = ax.get_ylim()[0] if len(valid_data) > 0 else 0
                    ax.scatter(missing_in_range['Date'], 
                             [y_min] * len(missing_in_range), 
                             color='red', marker='|', s=50, alpha=0.8,
                             label='Missing Values')
                
                # Set title with ticker name and data info
                ticker_label = ticker_mapping[col]
                data_points = len(valid_data)
                missing_points = len(missing_in_range)
                ax.set_title(f'{ticker_label}', 
                           fontsize=11, fontweight='bold')
                
                # Add legend if there are missing values
                # if len(missing_in_range) > 0:
                #     ax.legend(fontsize=8, loc='upper left')
                    
            else:
                ax.text(0.5, 0.5, 'No Data Available', 
                       horizontalalignment='center', verticalalignment='center',
                       transform=ax.transAxes, fontsize=14, color='red')
                ticker_label = ticker_mapping.get(col, col)
                ax.set_title(f'{ticker_label}\nNo Data', fontsize=12, fontweight='bold')
            
            # Set consistent time scale for all subplots
            ax.set_xlim(global_start, global_end)
            # ax.set_xlabel('Date', fontsize=10)
            ax.set_ylabel('Price', fontsize=10)
            ax.grid(True, alpha=0.3)
            ax.tick_params(axis='x', rotation=45, labelsize=8)
            ax.tick_params(axis='y', labelsize=8)
        
        # Hide unused subplots if any (should be none with exactly 6 plots)
        for j in range(len(price_cols), len(axes)):
            axes[j].set_visible(False)
        
        plt.tight_layout()
        plt.subplots_adjust(top=0.9)
        
        sync_file = plots_dir / 'nickel_synchronized_missing_indicators.png'
        plt.savefig(sync_file, dpi=300, bbox_inches='tight')
        print(f"Synchronized plot with missing indicators saved: {sync_file}")
        plt.close()
        
    except Exception as e:
        print(f"Error creating synchronized missing value plot: {e}")
        import traceback
        traceback.print_exc()


def extract_nickel_data():
    """Extract and merge nickel price data from multiple sources."""

    print("Loading nickel data sources...")

    # Primary consolidated source for nickel (already harmonized across feeds)
    print("Loading consolidated nickel data (ALL_nickel_prices_outer.csv)...")
    dfni = pd.read_csv('data/ALL_nickel_prices_outer.csv')
    dfni['Date'] = pd.to_datetime(dfni['Date'])

    # Load ticker mapping from JSON file
    print("Loading ticker mapping from ticker_mapping.json...")
    try:
        with open('../config/ticker_mapping.json', 'r') as f:
            ticker_mappings = json.load(f)
        nickel_ticker_mapping_raw = ticker_mappings.get('nickel', {})
        
        # Map JSON keys to actual CSV column names
        csv_to_json_mapping = {
            "Price_DailyMetal": "Price_DailyMetal",
            "Price_LME_NI": "Price_LME_NI",
            "Price_LME_NI_All": "Price_LME_NI_All",
            "Price_WUXI": "Price_WUXI",
            "Price_ETF": "Price_ETF",
            "Price_India": "Price_India"
        }
        
        # Create mapping from CSV columns to tickers
        nickel_ticker_mapping = {}
        for csv_col, json_key in csv_to_json_mapping.items():
            if json_key in nickel_ticker_mapping_raw:
                nickel_ticker_mapping[csv_col] = nickel_ticker_mapping_raw[json_key]
        
        print(f"Loaded {len(nickel_ticker_mapping)} nickel ticker mappings")
        print(f"Mapped columns: {list(nickel_ticker_mapping.keys())}")
        
    except FileNotFoundError:
        print("Warning: ticker_mapping.json not found. Using fallback mapping.")
        nickel_ticker_mapping = {
            "Price_DailyMetal": "NIDALY",
            "Price_LME": "NILMEX",
            "Price_ETF": "NIETFN",
            "Price_SHFE": "NISHFE",
            "Price_WUXI": "NIWUXI",
            "Price_India": "NIINDA"
        }
    except Exception as e:
        print(f"Error loading ticker mapping: {e}. Using fallback mapping.")
        nickel_ticker_mapping = {
            "Price_DailyMetal": "NIDALY",
            "Price_LME": "NILMEX",
            "Price_ETF": "NIETFN",
            "Price_SHFE": "NISHFE",
            "Price_WUXI": "NIWUXI",
            "Price_India": "NIINDA"
        }

    # Keep only the columns we care about (those that exist in our data and mapping)
    keep_cols = ['Date'] + [col for col in nickel_ticker_mapping.keys() if col in dfni.columns]
    existing_cols = [c for c in keep_cols if c in dfni.columns]
    dfni = dfni[existing_cols]
    
    print(f"Available columns from mapping: {[col for col in nickel_ticker_mapping.keys() if col in dfni.columns]}")
    print(f"Missing columns from mapping: {[col for col in nickel_ticker_mapping.keys() if col not in dfni.columns]}")

    # Convert numeric columns to float, handling strings if needed
    for col in dfni.columns:
        if col == 'Date':
            continue
        dfni[col] = pd.to_numeric(dfni[col], errors='coerce')

    # Sort and de-duplicate
    dfni = dfni.drop_duplicates(subset=['Date']).sort_values(by='Date')

    print(f"  Consolidated nickel: {dfni.shape[0]} rows, {dfni['Date'].min()} to {dfni['Date'].max()}")

    # Remove unwanted metadata columns if they slipped in
    unwanted_columns = ['Net', '%Chg', 'Open', 'Low', 'High', 'Volume', 'Bid', 'Ask']
    columns_to_remove = [col for col in unwanted_columns if col in dfni.columns]
    if columns_to_remove:
        dfni = dfni.drop(columns=columns_to_remove)
        print(f"Removed unwanted columns: {columns_to_remove}")

    # Analyze missing data BEFORE interpolation
    missing_stats_before = analyze_missing_data(dfni, "Before Interpolation")

    # Create a copy without interpolation for missing data analysis
    dfni_raw = dfni.copy()

    # Interpolate missing values linearly and round
    print("Interpolating missing values...")
    dfni = dfni.interpolate(method='linear', limit_direction='both')
    print("Interpolation completed.")

    print("Rounding price columns to 4 decimal places...")
    price_columns = [col for col in dfni.columns if col != 'Date']
    dfni[price_columns] = dfni[price_columns].round(4)
    print("Rounding completed.")

    # Analyze missing data AFTER interpolation
    missing_stats_after = analyze_missing_data(dfni, "After Interpolation")

    print(f"\nMerged data shape: {dfni.shape}")
    print(f"Date range: {dfni['Date'].min()} to {dfni['Date'].max()}")
    print(f"Columns: {list(dfni.columns)}")

    # Create output directory
    output_dir = Path('barb_experiments/barb_data')
    output_dir.mkdir(parents=True, exist_ok=True)

    # Save base series
    out_series_file = output_dir / 'nickel_prices_series.csv'
    dfni.to_csv(out_series_file, index=False)
    print(f"\nNickel data saved to: {out_series_file}")

    # Save raw (non-interpolated) data as well
    out_raw_file = output_dir / 'nickel_prices_series_raw.csv'
    dfni_raw.to_csv(out_raw_file, index=False)
    print(f"Raw nickel data (no interpolation) saved to: {out_raw_file}")

    # Create tickers mapping from loaded JSON
    # Only map columns that actually exist in our data
    ticker_mapping = {col: ticker for col, ticker in nickel_ticker_mapping.items() 
                     if col in dfni.columns}

    dfni_tickers = dfni.copy().rename(columns=ticker_mapping)
    out_tickers_file = output_dir / 'nickel_prices_tickers.csv'
    dfni_tickers.to_csv(out_tickers_file, index=False)
    print(f"Nickel data with tickers saved to: {out_tickers_file}")

    print("\nColumn mapping:")
    for old_name, new_name in ticker_mapping.items():
        if old_name in dfni.columns:
            print(f"  {old_name} → {new_name}")

    # Enhanced plotting section
    print("\nCreating enhanced nickel price plots...")
    try:
        import matplotlib.pyplot as plt
        import seaborn as sns

        plots_dir = output_dir / 'nickel_plots'
        plots_dir.mkdir(parents=True, exist_ok=True)

        # Set style
        plt.style.use('seaborn-v0_8')
        sns.set_palette("husl")

        # Create missing data visualizations using raw data
        create_missing_data_heatmap(dfni_raw, plots_dir)
        
        # Create smart plots that handle missing values
        create_smart_plots(dfni_raw, output_dir)
        
        # Create synchronized plot with missing value indicators
        create_synchronized_missing_value_plot(dfni_raw, output_dir)

        # Original plots (with interpolated data) - keep existing functionality
        ticker_columns = [col for col in dfni_tickers.columns if col != 'Date']

        print(f"Creating original subplot figure for {len(ticker_columns)} series...")
        n = len(ticker_columns)
        rows = 2
        cols = max(3, (n + rows - 1) // rows)
        fig, axes = plt.subplots(rows, cols, figsize=(6 * cols, 4.5 * rows))
        fig.suptitle('Nickel (Interpolated Data)', fontsize=20, fontweight='bold')

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
            ax.plot(dfni_tickers['Date'], dfni_tickers[ticker], linewidth=2, color=colors[i])
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

        subplot_file = plots_dir / 'nickel_price_subplots.png'
        plt.savefig(subplot_file, dpi=300, bbox_inches='tight')
        print(f"Original subplot figure saved: {subplot_file}")
        plt.close()

        # Combined plot (interpolated)
        print("Creating original combined plot...")
        fig, ax = plt.subplots(figsize=(14, 8))
        for i, ticker in enumerate(ticker_columns):
            ax.plot(dfni_tickers['Date'], dfni_tickers[ticker], linewidth=2, label=ticker)
        ax.set_title('All Nickel Price Series Comparison (Interpolated)', fontsize=18, fontweight='bold')
        ax.set_xlabel('Date', fontsize=14)
        ax.set_ylabel('Price', fontsize=14)
        ax.legend(fontsize=10, loc='upper left')
        ax.grid(True, alpha=0.3)
        plt.xticks(rotation=45)
        plt.tight_layout()
        combined_file = plots_dir / 'nickel_price_series.png'
        plt.savefig(combined_file, dpi=300, bbox_inches='tight')
        print(f"Original combined plot saved: {combined_file}")
        plt.close()

        print(f"All nickel plots saved in: {plots_dir}")
        
    except ImportError:
        print("Matplotlib not available - skipping plotting")
    except Exception as e:
        print(f"Error creating plots: {e}")
        import traceback
        traceback.print_exc()

    print("\nNickel data extraction completed successfully!")
    return dfni


if __name__ == "__main__":
    try:
        df = extract_nickel_data()
        print("Nickel data extraction completed successfully!")
    except Exception as e:
        print(f"Error extracting nickel data: {e}")
        import traceback
        traceback.print_exc()