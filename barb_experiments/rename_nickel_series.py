#!/usr/bin/env python3
"""
Rename nickel price series columns to use ticker symbols.
This creates a new file with the ticker names for easier analysis.
"""

import pandas as pd

def rename_nickel_series():
    """Rename nickel price series to use ticker symbols."""
    
    # Read the nickel prices file
    print("Reading nickel prices file...")
    df = pd.read_csv('data/nickel_prices_6_series.csv')
    
    print(f"Original columns: {list(df.columns)}")
    
    # Define the ticker mapping for nickel
    ticker_mapping = {
        'Price_Nickel': 'NIDALY',      # Dailymetal Nickel prices
        'Price_LME': 'NILME',          # LME Nickel prices
        'Price_ETF': 'NIETFN',         # Sprott Nickel ETF
        'Price_SHFE': 'NISHFE',        # SHFE Nickel futures
        'Price_WUXI': 'NIWUXI',        # WUXI Nickel futures
        'Price_India': 'NIINDI'        # India exchange Nickel futures
    }
    
    # Rename the columns
    df_renamed = df.rename(columns=ticker_mapping)
    
    print(f"Renamed columns: {list(df_renamed.columns)}")
    
    # Save the renamed file
    output_file = 'data/nickel_prices_tickers.csv'
    df_renamed.to_csv(output_file, index=False)
    print(f"Saved renamed nickel prices to: {output_file}")
    
    # Show first few rows to confirm the changes
    print(f"\nFirst 5 rows with ticker names:")
    print(df_renamed.head())
    
    # Show the mapping for reference
    print(f"\nColumn mapping:")
    for old_name, new_name in ticker_mapping.items():
        print(f"  {old_name} → {new_name}")
    
    return df_renamed

if __name__ == "__main__":
    rename_nickel_series()

