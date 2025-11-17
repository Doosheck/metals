import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path

# Define materials to compare
MATERIALS = ['lithium', 'nickel', 'cobalt', 'copper']

# Load all data
data = {}
for material in MATERIALS:
    data[material] = {
        'outer': pd.read_csv(f'data/ALL_{material}_prices_outer.csv'),
        'spline': pd.read_csv(f'data/ALL_{material}_prices_cubic_spline.csv'),
        'linear': pd.read_csv(f'data/ALL_{material}_prices_interpolated.csv')
    }
    
    # Convert Date column to datetime
    for method in ['outer', 'spline', 'linear']:
        data[material][method]['Date'] = pd.to_datetime(data[material][method]['Date'])

# Get variable names for each material (all columns except Date)
variables = {}
for material in MATERIALS:
    variables[material] = [col for col in data[material]['outer'].columns if col != 'Date']
    print(f"{material.capitalize()}: {len(variables[material])} variables - {variables[material]}")


def find_differences(material, tolerance=1e-9):
    """
    Find observations where interpolation methods produce different results.
    Returns a merged dataframe with only rows where methods differ.
    """
    # Merge all dataframes on Date with explicit suffixes
    merged = data[material]['outer'].merge(
        data[material]['spline'], on='Date', suffixes=('_outer', '_spline')
    )
    merged = merged.merge(
        data[material]['linear'], on='Date'
    )
    
    # Rename linear columns to add _linear suffix
    for var in variables[material]:
        if var in merged.columns:
            merged.rename(columns={var: f'{var}_linear'}, inplace=True)
    
    # Find rows where methods differ for any variable
    diff_mask = pd.Series([False] * len(merged))
    
    for var in variables[material]:
        # Check if methods differ
        outer_col = f'{var}_outer'
        spline_col = f'{var}_spline'
        linear_col = f'{var}_linear'
        
        # Compare linear and spline to outer (reference)
        diff_outer_spline = ~np.isclose(merged[outer_col], merged[spline_col], rtol=tolerance, atol=tolerance)
        diff_outer_linear = ~np.isclose(merged[outer_col], merged[linear_col], rtol=tolerance, atol=tolerance)
        diff_spline_linear = ~np.isclose(merged[spline_col], merged[linear_col], rtol=tolerance, atol=tolerance)
        
        # Any difference means this row should be included
        var_diff = diff_outer_spline | diff_outer_linear | diff_spline_linear
        diff_mask = diff_mask | var_diff
    
    # Filter to only different observations
    diff_data = merged[diff_mask].copy()
    
    return diff_data


def approach_3_scatter_matrix_differences(material, diff_data):
    """
    Approach 3: Scatter Matrix Comparison - Only for observations with differences
    """
    if len(diff_data) == 0:
        print(f"No differences found for {material}")
        return
    
    for var in variables[material]:
        fig = plt.figure(figsize=(20, 12))
        fig.suptitle(f'{material.capitalize()} - Scatter Matrix for {var} (Only Different Observations)\n'
                     f'n = {len(diff_data)} observations with differences', 
                     fontsize=16, fontweight='bold')
        
        # Create 3x2 grid: 6 scatter plots
        gs = fig.add_gridspec(3, 2, hspace=0.35, wspace=0.3)
        
        outer_col = f'{var}_outer'
        kalman_col = f'{var}_kalman'
        spline_col = f'{var}_spline'
        linear_col = f'{var}_linear'
        
        # Scatter 1: Kalman vs Outer
        ax1 = fig.add_subplot(gs[0, 0])
        ax1.scatter(diff_data[outer_col], diff_data[kalman_col], 
                   alpha=0.6, s=50, color='#2ca02c', edgecolors='black', linewidth=0.5)
        min_val = min(diff_data[outer_col].min(), diff_data[kalman_col].min())
        max_val = max(diff_data[outer_col].max(), diff_data[kalman_col].max())
        ax1.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2, alpha=0.5, label='Perfect Agreement')
        ax1.set_xlabel('Outer Join', fontsize=11)
        ax1.set_ylabel('Kalman Filter', fontsize=11)
        ax1.set_title('Kalman vs Outer Join', fontsize=12, fontweight='bold')
        ax1.grid(True, alpha=0.3)
        ax1.legend()
        
        # Scatter 2: Spline vs Outer
        ax2 = fig.add_subplot(gs[0, 1])
        ax2.scatter(diff_data[outer_col], diff_data[spline_col], 
                   alpha=0.6, s=50, color='#ff7f0e', edgecolors='black', linewidth=0.5)
        min_val = min(diff_data[outer_col].min(), diff_data[spline_col].min())
        max_val = max(diff_data[outer_col].max(), diff_data[spline_col].max())
        ax2.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2, alpha=0.5, label='Perfect Agreement')
        ax2.set_xlabel('Outer Join', fontsize=11)
        ax2.set_ylabel('Cubic Spline', fontsize=11)
        ax2.set_title('Spline vs Outer Join', fontsize=12, fontweight='bold')
        ax2.grid(True, alpha=0.3)
        ax2.legend()
        
        # Scatter 3: Linear vs Outer
        ax3 = fig.add_subplot(gs[1, 0])
        ax3.scatter(diff_data[outer_col], diff_data[linear_col], 
                   alpha=0.6, s=50, color='#d62728', edgecolors='black', linewidth=0.5)
        min_val = min(diff_data[outer_col].min(), diff_data[linear_col].min())
        max_val = max(diff_data[outer_col].max(), diff_data[linear_col].max())
        ax3.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2, alpha=0.5, label='Perfect Agreement')
        ax3.set_xlabel('Outer Join', fontsize=11)
        ax3.set_ylabel('Linear Interpolation', fontsize=11)
        ax3.set_title('Linear vs Outer Join', fontsize=12, fontweight='bold')
        ax3.grid(True, alpha=0.3)
        ax3.legend()
        
        # Scatter 4: Kalman vs Spline
        ax4 = fig.add_subplot(gs[1, 1])
        ax4.scatter(diff_data[spline_col], diff_data[kalman_col], 
                   alpha=0.6, s=50, color='#1f77b4', edgecolors='black', linewidth=0.5)
        min_val = min(diff_data[spline_col].min(), diff_data[kalman_col].min())
        max_val = max(diff_data[spline_col].max(), diff_data[kalman_col].max())
        ax4.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2, alpha=0.5, label='Perfect Agreement')
        ax4.set_xlabel('Cubic Spline', fontsize=11)
        ax4.set_ylabel('Kalman Filter', fontsize=11)
        ax4.set_title('Kalman vs Spline', fontsize=12, fontweight='bold')
        ax4.grid(True, alpha=0.3)
        ax4.legend()
        
        # Scatter 5: Kalman vs Linear
        ax5 = fig.add_subplot(gs[2, 0])
        ax5.scatter(diff_data[linear_col], diff_data[kalman_col], 
                   alpha=0.6, s=50, color='#9467bd', edgecolors='black', linewidth=0.5)
        min_val = min(diff_data[linear_col].min(), diff_data[kalman_col].min())
        max_val = max(diff_data[linear_col].max(), diff_data[kalman_col].max())
        ax5.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2, alpha=0.5, label='Perfect Agreement')
        ax5.set_xlabel('Linear Interpolation', fontsize=11)
        ax5.set_ylabel('Kalman Filter', fontsize=11)
        ax5.set_title('Kalman vs Linear', fontsize=12, fontweight='bold')
        ax5.grid(True, alpha=0.3)
        ax5.legend()
        
        # Scatter 6: Spline vs Linear
        ax6 = fig.add_subplot(gs[2, 1])
        ax6.scatter(diff_data[linear_col], diff_data[spline_col], 
                   alpha=0.6, s=50, color='#8c564b', edgecolors='black', linewidth=0.5)
        min_val = min(diff_data[linear_col].min(), diff_data[spline_col].min())
        max_val = max(diff_data[linear_col].max(), diff_data[spline_col].max())
        ax6.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2, alpha=0.5, label='Perfect Agreement')
        ax6.set_xlabel('Linear Interpolation', fontsize=11)
        ax6.set_ylabel('Cubic Spline', fontsize=11)
        ax6.set_title('Spline vs Linear', fontsize=12, fontweight='bold')
        ax6.grid(True, alpha=0.3)
        ax6.legend()
        
        filename = f'differences_scatter_matrix_{material}_{var}.png'
        plt.savefig(filename, dpi=300, bbox_inches='tight')
        plt.close()
        print(f"Saved: {filename}")


def plot_differences_over_time(material, diff_data):
    """
    Plot the absolute differences between methods over time for different observations only.
    Compares Linear and Cubic Spline against Outer Join (reference).
    """
    if len(diff_data) == 0:
        print(f"No differences found for {material}")
        return
    
    vars_list = variables[material]
    n_vars = len(vars_list)
    fig, axes = plt.subplots(n_vars, 1, figsize=(16, 5*n_vars))
    
    if n_vars == 1:
        axes = [axes]
    
    fig.suptitle(f'{material.capitalize()} - Absolute Differences Over Time (Only Different Observations)\n'
                 f'n = {len(diff_data)} observations with differences', 
                 fontsize=16, fontweight='bold', y=0.995)
    
    for idx, var in enumerate(vars_list):
        ax = axes[idx]
        
        outer_col = f'{var}_outer'
        spline_col = f'{var}_spline'
        linear_col = f'{var}_linear'
        
        # Calculate absolute differences from reference (outer join)
        diff_linear_outer = np.abs(diff_data[linear_col] - diff_data[outer_col])
        diff_spline_outer = np.abs(diff_data[spline_col] - diff_data[outer_col])
        diff_spline_linear = np.abs(diff_data[spline_col] - diff_data[linear_col])
        
        # Plot differences
        ax.plot(diff_data['Date'], diff_linear_outer, label='Linear vs Reference', 
                linewidth=2.5, alpha=0.8, color='#1f77b4', marker='o', markersize=5)
        ax.plot(diff_data['Date'], diff_spline_outer, label='Cubic Spline vs Reference', 
                linewidth=2.5, alpha=0.8, color='#ff7f0e', marker='s', markersize=5)
        ax.plot(diff_data['Date'], diff_spline_linear, label='Cubic Spline vs Linear', 
                linewidth=2, alpha=0.6, color='#2ca02c', marker='^', markersize=4, linestyle='--')
        
        ax.set_title(f'{var} - Deviation from Reference', fontsize=13, fontweight='bold')
        ax.set_xlabel('Date', fontsize=11)
        ax.set_ylabel('Absolute Difference', fontsize=11)
        ax.legend(loc='best', fontsize=10)
        ax.grid(True, alpha=0.3)
        ax.tick_params(axis='x', rotation=45)
    
    plt.tight_layout()
    filename = f'differences_over_time_{material}.png'
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    plt.close()
    print(f"Saved: {filename}")


def generate_difference_statistics(material, diff_data):
    """
    Generate summary statistics for differences (only for observations where methods differ).
    Compares Linear and Cubic Spline against Outer Join (reference).
    """
    if len(diff_data) == 0:
        print(f"\nNo differences found for {material}")
        return
    
    print(f"\n{'='*80}")
    print(f"DIFFERENCE STATISTICS: {material.upper()}")
    print(f"Total observations: {len(data[material]['outer'])}")
    print(f"Observations with differences: {len(diff_data)} ({len(diff_data)/len(data[material]['outer'])*100:.2f}%)")
    print(f"{'='*80}")
    
    for var in variables[material]:
        print(f"\n{var}:")
        print("-" * 70)
        
        outer_col = f'{var}_outer'
        spline_col = f'{var}_spline'
        linear_col = f'{var}_linear'
        
        # Calculate differences from reference (outer join)
        diff_linear_outer = diff_data[linear_col] - diff_data[outer_col]
        diff_spline_outer = diff_data[spline_col] - diff_data[outer_col]
        diff_spline_linear = diff_data[spline_col] - diff_data[linear_col]
        
        # Statistics
        stats = pd.DataFrame({
            'Linear vs Reference': {
                'Mean Diff': diff_linear_outer.mean(),
                'Abs Mean Diff': np.abs(diff_linear_outer).mean(),
                'Max Abs Diff': np.abs(diff_linear_outer).max(),
                'Std Dev': diff_linear_outer.std(),
                'Correlation': diff_data[linear_col].corr(diff_data[outer_col])
            },
            'Cubic Spline vs Reference': {
                'Mean Diff': diff_spline_outer.mean(),
                'Abs Mean Diff': np.abs(diff_spline_outer).mean(),
                'Max Abs Diff': np.abs(diff_spline_outer).max(),
                'Std Dev': diff_spline_outer.std(),
                'Correlation': diff_data[spline_col].corr(diff_data[outer_col])
            },
            'Cubic Spline vs Linear': {
                'Mean Diff': diff_spline_linear.mean(),
                'Abs Mean Diff': np.abs(diff_spline_linear).mean(),
                'Max Abs Diff': np.abs(diff_spline_linear).max(),
                'Std Dev': diff_spline_linear.std(),
                'Correlation': diff_data[spline_col].corr(diff_data[linear_col])
            }
        }).T
        
        print(stats.to_string())
    
    print(f"\n{'='*80}")


# Main execution
if __name__ == "__main__":
    print("="*80)
    print("INTERPOLATION COMPARISON - DIFFERENCES ONLY")
    print("Materials: " + ", ".join([m.capitalize() for m in MATERIALS]))
    print("Reference: Outer Join | Competing Methods: Linear Interpolation, Cubic Spline")
    print("="*80)
    
    # Store difference counts
    diff_counts = {}
    
    # Process each material
    for material in MATERIALS:
        print(f"\n{'#'*80}")
        print(f"# Processing: {material.upper()}")
        print(f"{'#'*80}")
        
        # Find differences
        print(f"\n--- Finding observations with differences ({material}) ---")
        diff_data = find_differences(material)
        diff_counts[material] = len(diff_data)
        
        print(f"Total observations: {len(data[material]['outer'])}")
        print(f"Observations with differences: {len(diff_data)} ({len(diff_data)/len(data[material]['outer'])*100:.2f}%)")
        
        if len(diff_data) > 0:
            # Generate visualizations
            print(f"\n--- Generating Scatter Matrix for Different Observations ({material}) ---")
            approach_3_scatter_matrix_differences(material, diff_data)
            
            print(f"\n--- Generating Time Series of Differences ({material}) ---")
            plot_differences_over_time(material, diff_data)
            
            # Generate statistics
            generate_difference_statistics(material, diff_data)
    
    # Summary
    print("\n" + "="*80)
    print("✓ All visualizations generated successfully!")
    print("="*80)
    print("\nDIFFERENCE SUMMARY:")
    for material in MATERIALS:
        total = len(data[material]['outer'])
        diff = diff_counts[material]
        pct = (diff/total)*100 if total > 0 else 0
        print(f"{material.capitalize():10s}: {diff:6d} / {total:6d} ({pct:5.2f}%) observations differ")
    print("="*80)