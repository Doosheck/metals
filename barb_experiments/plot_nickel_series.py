#!/usr/bin/env python3
"""
Plot individual nickel price series using ticker names.
This script creates separate graphs for each of the 6 nickel price series.
"""

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path

def plot_nickel_series():
    """Plot each nickel price series in separate subplots."""
    
    # Set up the plotting style
    plt.style.use('seaborn-v0_8')
    sns.set_palette("husl")
    
    # Read the nickel data with tickers
    print("Reading nickel price data...")
    df = pd.read_csv('barb_experiments/barb_data/nickel_prices_tickers.csv')
    df['Date'] = pd.to_datetime(df['Date'])
    
    print(f"Data shape: {df.shape}")
    print(f"Columns: {list(df.columns)}")
    
    # Get the ticker columns (excluding Date)
    ticker_columns = [col for col in df.columns if col != 'Date']
    
    # Create output directory
    output_dir = Path('barb_experiments/barb_data/nickel_plots')
    output_dir.mkdir(parents=True, exist_ok=True)
    
    print(f"Creating subplot figure for {len(ticker_columns)} series (third series removed)...")
    
    # Remove the third series (index 2) from ticker_columns
    ticker_columns = [col for i, col in enumerate(ticker_columns) if i != 2]
    
    # Create figure with subplots (2 rows, 3 columns for 5 series, with one empty subplot)
    fig, axes = plt.subplots(2, 3, figsize=(18, 12))
    fig.suptitle('Nickel', fontsize=20, fontweight='bold')
    
    # Flatten axes array for easier indexing
    axes = axes.flatten()
    
    # Colors for each subplot
    #colors = ['steelblue', 'darkred', 'orange', 'purple', 'brown']
    colors = ['#4682B4', '#708090', '#2F4F4F', '#36454F', '#1C1C1C']
    
    # Plot each series in its own subplot
    for i, ticker in enumerate(ticker_columns):
        print(f"Plotting {ticker} in subplot {i+1}...")
        
        ax = axes[i]
        
        # Plot the series
        ax.plot(df['Date'], df[ticker], linewidth=2, color=colors[i])
        
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
    subplot_file = output_dir / 'nickel_price_subplots.png'
    plt.savefig(subplot_file, dpi=300, bbox_inches='tight')
    print(f"Subplot figure saved: {subplot_file}")
    
    # Show the plot
    plt.show()
    
    # Create a combined plot showing all series (keeping this as well)
    print("Creating combined plot...")
    fig, ax = plt.subplots(figsize=(14, 8))
    
    # Plot all series on the same graph
    for i, ticker in enumerate(ticker_columns):
        ax.plot(df['Date'], df[ticker], linewidth=2, label=ticker, color=colors[i])
    
    # Customize the combined plot
    ax.set_title('All Nickel Price Series Comparison', fontsize=18, fontweight='bold')
    ax.set_xlabel('Date', fontsize=14)
    ax.set_ylabel('Price', fontsize=14)
    ax.legend(fontsize=12, loc='upper left')
    ax.grid(True, alpha=0.3)
    
    # Rotate x-axis labels
    plt.xticks(rotation=45)
    
    # Tight layout
    plt.tight_layout()
    
    # Save the combined plot
    combined_file = output_dir / 'all_nickel_series_combined.png'
    plt.savefig(combined_file, dpi=300, bbox_inches='tight')
    print(f"Combined plot saved: {combined_file}")
    
    # Close the figures
    plt.close('all')
    
    print(f"\nAll plots saved in: {output_dir}")
    
    # Show summary statistics
    print(f"\nSummary Statistics:")
    print("=" * 50)
    for ticker in ticker_columns:
        series = df[ticker]
        print(f"{ticker}:")
        print(f"  Min: {series.min():.2f}")
        print(f"  Max: {series.max():.2f}")
        print(f"  Mean: {series.mean():.2f}")
        print(f"  Std: {series.std():.2f}")
        print()

if __name__ == "__main__":
    plot_nickel_series()