#!/usr/bin/env python3
"""
Compute rolling correlation windows for lithium time series from ALL_lithium_prices_interpolated.csv.
This script analyzes rolling correlations between different copper price series.
"""

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import os
from pathlib import Path
from typing import List, Tuple


def load_and_prepare_data(csv_path: str) -> pd.DataFrame:
    """Load CSV and prepare data for correlation analysis."""
    #df = pd.read_csv("barb_experiments/barb_data/lithium_prices_tickers.csv")
    df = pd.read_csv("data/ALL_lithium_prices_interpolated.csv")
    
    if 'Date' not in df.columns:
        raise ValueError(f"Expected a 'Date' column in {csv_path}")
    
    # Convert Date column
    df['Date'] = pd.to_datetime(df['Date'], errors='coerce')
    df = df.dropna(subset=['Date']).sort_values('Date').reset_index(drop=True)
    
    # Convert all non-Date columns to numeric
    for col in df.columns:
        if col == 'Date':
            continue
        if not pd.api.types.is_numeric_dtype(df[col]):
            df[col] = pd.to_numeric(df[col], errors='coerce')
    ticker_mapping = {
            "Price_DailyMetal": "LIDALY",
            "Price_SouthAmericaLOB": "LISAME",
            "Price_COMEXLithiumHydroxide": "LICOMX",
            "Price_LithiumAmericasCorp": "LILAMC",
            "Price_EastAsiaLithiumCarbonate": "LIEALC",
            "Price_LithiumLME": "LILMEX"
        }
    # Apply the mapping to rename columns
    df = df.rename(columns=ticker_mapping)

    # Optional: Print which columns were renamed for debugging
    renamed_cols = [old for old in ticker_mapping.keys() if old in df.columns]
    if renamed_cols:
        print(f"Renamed columns: {renamed_cols}")
    
    # Optional: Check for unmapped columns (excluding Date)
    unmapped_cols = [col for col in df.columns if col != 'Date' and col not in ticker_mapping.values()]
    if unmapped_cols:
        print(f"Warning: Unmapped columns found: {unmapped_cols}")
    return df


def compute_returns(df: pd.DataFrame) -> pd.DataFrame:
    """Compute returns for all numeric columns."""
    # Get all numeric columns (excluding Date)
    numeric_cols = [col for col in df.columns if col != 'Date' and pd.api.types.is_numeric_dtype(df[col])]
    
    returns = df[['Date'] + numeric_cols].copy()
    
    # Calculate percentage changes
    for col in numeric_cols:
        returns[col] = returns[col].pct_change()
    
    # Drop NaN values (first row will be NaN after pct_change)
    returns = returns.dropna().reset_index(drop=True)
    
    return returns


def get_top_correlated_pairs(returns: pd.DataFrame, top_n: int = 6) -> List[Tuple[str, str, float]]:
    """Find top correlated pairs based on absolute correlation."""
    # Get numeric columns only
    numeric_cols = [col for col in returns.columns if col != 'Date']
    
    # Compute correlation matrix
    corr_matrix = returns[numeric_cols].corr(method='pearson')
    
    # Extract pairs with their correlations
    pairs = []
    for i in range(len(numeric_cols)):
        for j in range(i + 1, len(numeric_cols)):
            correlation = corr_matrix.iloc[i, j]
            pairs.append((numeric_cols[i], numeric_cols[j], float(correlation)))
    
    # Sort by absolute correlation
    pairs.sort(key=lambda x: abs(x[2]), reverse=True)
    
    return pairs[:top_n]


def plot_rolling_correlations(returns: pd.DataFrame, pairs: List[Tuple[str, str, float]], 
                            windows: List[int] = [30, 60], output_path: str = None, 
                            ncols: int = 2) -> None:
    """Plot rolling correlations for top pairs."""
    
    if not pairs:
        print("No pairs to plot")
        return
    
    n_pairs = len(pairs)
    nrows = int(np.ceil(n_pairs / ncols))
    
    # Set up colors
    #palette = sns.color_palette('tab10', n_colors=max(3, len(windows)))
    palette = sns.color_palette(['#191970', '#006400'], n_colors=max(3, len(windows)))
    sns.set_style('whitegrid')
    
    # Create subplots
    fig, axes = plt.subplots(nrows=nrows, ncols=ncols, figsize=(6.5 * ncols, 3.2 * nrows), 
                           sharex=True, squeeze=False)
    axes_flat = axes.flatten()
    
    # Plot each pair
    for idx, (series1, series2, static_corr) in enumerate(pairs):
        ax = axes_flat[idx]
        
        # Plot rolling correlations for different windows
        for win_idx, window in enumerate(windows):
            rolling_corr = returns[series1].rolling(window=window).corr(returns[series2])
            ax.plot(returns['Date'], rolling_corr, label=f"{window}D", color=palette[win_idx])
        
        # Add horizontal line at zero
        ax.axhline(0.0, color='k', linewidth=1, alpha=0.6)
        
        # Customize subplot
        ax.set_title(f"{series1} vs {series2} (|corr|={abs(static_corr):.2f})", fontsize=11)
        ax.set_ylabel('')
        ax.legend(fontsize=9)
        ax.grid(True, alpha=0.3)
    
    # Hide unused subplots
    for idx in range(len(pairs), len(axes_flat)):
        axes_flat[idx].axis('off')
    
    # Format the plot
    win_str = ', '.join(f"{w}D" for w in windows)
    fig.suptitle(f"Lithium Price Series Rolling Correlations", fontsize=14, fontweight='bold')
    fig.autofmt_xdate()  # Format x-axis dates
    fig.tight_layout(rect=[0, 0.03, 1, 0.97])
    
    # Save or show plot
    if output_path:
        # Create output directory if it doesn't exist
        os.makedirs(os.path.dirname(output_path), exist_ok=True)
        fig.savefig(output_path, dpi=300, bbox_inches='tight')
        print(f"Plot saved to: {output_path}")
    else:
        plt.show()
    
    plt.close(fig)


def analyze_csv_correlations(csv_path: str, windows: List[int] = [30, 60], 
                           top_n_pairs: int = 6, output_path: str = None, ncols: int = 2):
    """Main function to analyze correlations in a CSV file."""
    
    print(f"Loading data from: {csv_path}")
    
    # Load and prepare data
    df = load_and_prepare_data(csv_path)
    print(f"Data shape: {df.shape}")
    print(f"Columns: {list(df.columns)}")
    
    # Compute returns
    returns = compute_returns(df)
    print(f"Returns computed. Shape: {returns.shape}")
    
    # Get top correlated pairs
    top_pairs = get_top_correlated_pairs(returns, top_n_pairs)
    
    print(f"\nTop {len(top_pairs)} correlated pairs:")
    for i, (s1, s2, corr) in enumerate(top_pairs, 1):
        print(f"{i}. {s1} vs {s2}: {corr:.3f}")
    
    # Plot rolling correlations
    if not output_path:
        # Create default output path
        csv_name = Path(csv_path).stem
        output_dir = Path(csv_path).parent / "correlation_plots"
        win_str = '_'.join(str(w) for w in windows)
        output_path = str(output_dir / f"{csv_name}_rolling_correlations_{win_str}D.png")
    
    plot_rolling_correlations(returns, top_pairs, windows, output_path, ncols)
    
    return top_pairs


# Example usage
if __name__ == "__main__":
    # Example parameters - modify these for your use case
    csv_file = "data/ALL_lithium_prices_interpolated.csv"  # Copper CSV path
    rolling_windows = [30, 60]  # Rolling window sizes in days
    top_pairs = 6  # Number of top correlated pairs to plot
    columns_per_row = 2  # Number of columns in subplot grid
    
    # Optional: specify custom output path
    output_file = None  # Will auto-generate if None
    
    try:
        pairs = analyze_csv_correlations(
            csv_path=csv_file,
            windows=rolling_windows,
            top_n_pairs=top_pairs,
            output_path=output_file,
            ncols=columns_per_row
        )
        print("\nLithium correlation analysis completed successfully!")
        
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()

