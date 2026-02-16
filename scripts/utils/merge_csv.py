"""This script merges all the csv files of the prices of a given metal into a single csv file.
The script retrieves the names of the metal and the date range from the filenames of the csv files."""

import os
import pandas as pd
from pathlib import Path

# Get the project root directory (two levels up from this script)
PROJECT_ROOT = Path(__file__).parent.parent.parent

def merge_csv_files(metal: str, from_date: str, to_date: str):
    """
    Merge all CSV files containing metal prices into a single CSV file.
    """
    # Get a list of all CSV files in the 'data' folder
    data_dir = PROJECT_ROOT / "data"
    csv_files = [f for f in os.listdir(data_dir) if f.endswith(".csv")]

    # Filter the list of CSV files to only include those for the given metal
    metal_files = [f for f in csv_files if metal.replace(' ', '_') in f]

    # Initialize an empty DataFrame to store the merged data
    merged_df = pd.DataFrame()

    # Iterate over the metal-specific CSV files and merge them into a single DataFrame
    for file in metal_files:
        df = pd.read_csv(data_dir / file)
        merged_df = pd.concat([merged_df, df])

    # Sort the merged DataFrame by date
    merged_df["Date"] = pd.to_datetime(merged_df["Date"])
    merged_df = merged_df.sort_values("Date")

    # Save the merged DataFrame to a new CSV file
    merged_filename = data_dir / f"{metal.replace(' ', '_')}_prices_{from_date}_to_{to_date}_merged.csv"
    merged_df.to_csv(merged_filename, index=False)
    print(f"Merged data saved to {merged_filename}")

# Example usage
merge_csv_files("Lithium", "2017-01-01", "2021-12-31")
