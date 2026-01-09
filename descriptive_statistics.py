#!/usr/bin/env python3
"""
Descriptive statistics analysis of metals' returns (copper, cobalt, nickel, lithium).
Calculates daily returns and provides statistical summary with data quality assessment.
Uses ticker mappings from ticker_mapping.json for series identification.
"""

import pandas as pd
import numpy as np
import json
from pathlib import Path

def load_ticker_mappings():
    """Load ticker mappings from JSON file."""
    try:
        with open('barb_experiments/ticker_mapping.json', 'r') as f:
            ticker_mappings = json.load(f)
        return ticker_mappings
    except FileNotFoundError:
        print("Warning: ticker_mapping.json not found. Using original column names.")
        return {}
    except Exception as e:
        print(f"Error loading ticker mappings: {e}")
        return {}

def load_metal_data(metal_name, ticker_mappings):
    """Load metal price data and calculate returns."""
    file_path = f'data/ALL_{metal_name}_prices_cubic_spline.csv'

    try:
        df = pd.read_csv(file_path)
        df['Date'] = pd.to_datetime(df['Date'])
        df = df.sort_values('Date').reset_index(drop=True)
        
        # Get price columns (exclude Date)
        price_cols = [col for col in df.columns if col != 'Date']
        
        # Exclude LISAME for lithium
        if metal_name.lower() == 'lithium' and 'LISAME' in price_cols:
            price_cols = [col for col in price_cols if col != 'LISAME']
            print(f"  Excluding LISAME series for {metal_name}")
        
        # Calculate returns for all price columns at once
        returns_df = df.copy()
        
        # Calculate percentage returns for each price column
        for col in price_cols:
            returns_df[f'{col}_returns'] = df[col].pct_change()
        
        # Keep only Date and returns columns, drop the first row (which has NaN returns)
        returns_cols = [f'{col}_returns' for col in price_cols]
        returns_df = returns_df[['Date'] + returns_cols].iloc[1:].reset_index(drop=True)
        
        # Create mapping from original column names to tickers
        metal_mappings = ticker_mappings.get(metal_name, {})
        ticker_map = {}
        
        for original_col in price_cols:
            if original_col in metal_mappings:
                ticker_map[original_col] = metal_mappings[original_col]
            else:
                # Fallback: use original column name if no mapping found
                ticker_map[original_col] = original_col
        
        return returns_df, price_cols, ticker_map
        
    except FileNotFoundError:
        print(f"Warning: File {file_path} not found. Skipping {metal_name}.")
        return None, None, None
    except Exception as e:
        print(f"Error loading {metal_name} data: {e}")
        return None, None, None

def calculate_descriptive_stats(returns_df, metal_name, price_cols, ticker_map):
    """Calculate descriptive statistics for returns."""
    stats_list = []
    
    returns_cols = [f'{col}_returns' for col in price_cols]
    
    for i, col in enumerate(returns_cols):
        if col in returns_df.columns:
            series = returns_df[col].dropna()
            
            if len(series) > 0:
                original_col = price_cols[i]
                ticker = ticker_map.get(original_col, original_col)
                
                stats = {
                    'Metal': metal_name.capitalize(),
                    'Series': original_col,
                    'Ticker': ticker,
                    'Observations': len(series),
                    'Mean': series.mean(),
                    'Std': series.std(),
                    'Min': series.min(),
                    'Q1': series.quantile(0.25),
                    'Q3': series.quantile(0.75),
                    'Max': series.max()
                }
                stats_list.append(stats)
    
    return stats_list

def assess_data_quality(returns_df, metal_name, price_cols):
    """Assess data quality for the metal's time series."""
    returns_cols = [f'{col}_returns' for col in price_cols]
    available_series = [col for col in returns_cols if col in returns_df.columns]
    
    if not available_series:
        return f"{metal_name.capitalize()}: No data available"
    
    # Calculate average observations per series
    avg_obs = np.mean([len(returns_df[col].dropna()) for col in available_series])
    
    # Check for extreme values (outliers beyond 3 standard deviations)
    extreme_counts = []
    for col in available_series:
        series = returns_df[col].dropna()
        if len(series) > 0:
            mean_val = series.mean()
            std_val = series.std()
            extremes = np.sum(np.abs(series - mean_val) > 3 * std_val)
            extreme_counts.append(extremes / len(series) * 100)
    
    avg_extreme_pct = np.mean(extreme_counts) if extreme_counts else 0
    
    # Assess volatility level
    avg_volatility = np.mean([returns_df[col].dropna().std() for col in available_series])
    
    # Quality assessment
    quality_notes = []
    
    if avg_obs < 100:
        quality_notes.append("Limited observations")
    elif avg_obs > 1000:
        quality_notes.append("Rich dataset")
    
    if avg_extreme_pct > 2:
        quality_notes.append("Contains outliers")
    
    if avg_volatility > 0.05:
        quality_notes.append("High volatility")
    elif avg_volatility < 0.01:
        quality_notes.append("Low volatility")
    
    if len(available_series) < len(price_cols):
        quality_notes.append("Some series missing")
    
    quality_summary = "; ".join(quality_notes) if quality_notes else "Standard quality"
    
    return f"{metal_name.capitalize()}: {quality_summary}"

def main():
    """Main function to analyze all metals and generate statistics."""
    
    # Load ticker mappings
    ticker_mappings = load_ticker_mappings()
    
    metals = ['copper', 'cobalt', 'nickel', 'lithium']
    all_stats = []
    quality_assessments = []
    
    print("Analyzing metals returns statistics...")
    print(f"Using ticker mappings: {'Yes' if ticker_mappings else 'No'}")
    
    for metal in metals:
        print(f"\nProcessing {metal}...")
        
        returns_df, price_cols, ticker_map = load_metal_data(metal, ticker_mappings)
        
        if returns_df is not None and price_cols is not None:
            # Calculate statistics
            stats = calculate_descriptive_stats(returns_df, metal, price_cols, ticker_map)
            all_stats.extend(stats)
            
            # Assess data quality
            quality = assess_data_quality(returns_df, metal, price_cols)
            quality_assessments.append(quality)
            
            print(f"  Found {len(stats)} price series for {metal}")
            
            # Print ticker mappings for this metal
            if ticker_mappings and metal in ticker_mappings:
                print(f"  Ticker mappings:")
                for original, ticker in ticker_map.items():
                    print(f"    {original} -> {ticker}")
        else:
            quality_assessments.append(f"{metal.capitalize()}: Data not available")
    
    # Create summary statistics DataFrame
    if all_stats:
        stats_df = pd.DataFrame(all_stats)
        
        # Round numerical columns for better presentation
        numerical_cols = ['Mean', 'Std', 'Min', 'Q1', 'Q3', 'Max']
        stats_df[numerical_cols] = stats_df[numerical_cols].round(6)
        
        print(f"\n=== DESCRIPTIVE STATISTICS SUMMARY ===")
        print(f"Total series analyzed: {len(all_stats)}")
        print("\nStatistics DataFrame:")
        print(stats_df.to_string(index=False))
        
        # Generate LaTeX table
        print(f"\n=== LATEX TABLE ===")
        latex_table = generate_latex_table(stats_df)
        print(latex_table)
        
        # Print data quality assessments
        print(f"\n=== DATA QUALITY ASSESSMENT ===")
        for assessment in quality_assessments:
            print(assessment)
        
        return stats_df
    else:
        print("No data could be loaded for analysis.")
        return None

def generate_latex_table(stats_df):
    """Generate LaTeX formatted table with tabularx and custom formatting."""
    
    latex = r"""\begin{table}[htbp]
\centering
\caption{Descriptive Statistics of Metal Price Returns}
\label{tab:metals_returns_stats}
\setlength{\tabcolsep}{8pt} % adjust column spacing
\renewcommand{\arraystretch}{1.1} % row height
\begin{tabularx}{\textwidth + 2cm}{l p{1cm} X X X X X X}
\hline
\textbf{Ticker} & \textbf{Obs} & \textbf{Mean} & \textbf{Std} & \textbf{Min} & \textbf{Q1} & \textbf{Q3} & \textbf{Max} \\
\hline
"""
    
    # Group by metal to add separators
    current_metal = None
    for _, row in stats_df.iterrows():
        # Add separator line between different metals
        if current_metal is not None and row['Metal'] != current_metal:
            latex += r"\hline" + "\n"
        
        current_metal = row['Metal']
        
        # Format the row
        latex += f"{row['Ticker']} & {row['Observations']} & {row['Mean']:.6f} & {row['Std']:.6f} & {row['Min']:.6f} & {row['Q1']:.6f} & {row['Q3']:.6f} & {row['Max']:.6f} \\\\\n"
    
    latex += r"""\hline
\hline
\end{tabularx}
\end{table}"""
    
    return latex

if __name__ == "__main__":
    try:
        stats_df = main()
        print("\nAnalysis completed successfully!")
    except Exception as e:
        print(f"Error in analysis: {e}")
        import traceback
        traceback.print_exc()