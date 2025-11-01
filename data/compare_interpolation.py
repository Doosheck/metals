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
        'kalman': pd.read_csv(f'data/ALL_{material}_prices_kalman.csv'),
        'spline': pd.read_csv(f'data/ALL_{material}_prices_cubic_spline.csv')
    }
    
    # Convert Date column to datetime
    for method in ['outer', 'kalman', 'spline']:
        data[material][method]['Date'] = pd.to_datetime(data[material][method]['Date'])

# Get variable names for each material (all columns except Date)
variables = {}
for material in MATERIALS:
    variables[material] = [col for col in data[material]['outer'].columns if col != 'Date']
    print(f"{material.capitalize()}: {len(variables[material])} variables - {variables[material]}")


def approach_1_side_by_side(material):
    """
    Approach 1: Side-by-Side Subplots
    Each variable gets its own subplot showing all three interpolation methods overlaid.
    """
    vars_list = variables[material]
    n_vars = len(vars_list)
    fig, axes = plt.subplots(n_vars, 1, figsize=(14, 4*n_vars))
    
    if n_vars == 1:
        axes = [axes]
    
    fig.suptitle(f'Approach 1: {material.capitalize()} - Side-by-Side Comparison of Interpolation Methods', 
                 fontsize=16, fontweight='bold', y=0.995)
    
    for idx, var in enumerate(vars_list):
        ax = axes[idx]
        
        # Plot all three methods
        ax.plot(data[material]['outer']['Date'], data[material]['outer'][var], 
                label='Outer Join', linewidth=2, alpha=0.8, color='#1f77b4')
        ax.plot(data[material]['kalman']['Date'], data[material]['kalman'][var], 
                label='Kalman Filter', linewidth=2, alpha=0.8, color='#2ca02c')
        ax.plot(data[material]['spline']['Date'], data[material]['spline'][var], 
                label='Cubic Spline', linewidth=2, alpha=0.8, color='#ff7f0e')
        
        ax.set_title(var, fontsize=12, fontweight='bold')
        ax.set_xlabel('Date', fontsize=10)
        ax.set_ylabel('Value', fontsize=10)
        ax.legend(loc='best')
        ax.grid(True, alpha=0.3)
        ax.tick_params(axis='x', rotation=45)
    
    plt.tight_layout()
    filename = f'approach1_side_by_side_{material}.png'
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    plt.close()
    print(f"Saved: {filename}")


def approach_2_differences(material):
    """
    Approach 2: Difference Highlighting
    Shows the absolute differences between interpolation methods over time.
    """
    vars_list = variables[material]
    n_vars = len(vars_list)
    fig, axes = plt.subplots(n_vars, 1, figsize=(14, 4*n_vars))
    
    if n_vars == 1:
        axes = [axes]
    
    fig.suptitle(f'Approach 2: {material.capitalize()} - Absolute Differences Between Interpolation Methods', 
                 fontsize=16, fontweight='bold', y=0.995)
    
    # Merge dataframes on Date
    merged = data[material]['outer'].merge(data[material]['kalman'], on='Date', suffixes=('_outer', '_kalman'))
    merged = merged.merge(data[material]['spline'], on='Date')
    
    for idx, var in enumerate(vars_list):
        ax = axes[idx]
        
        # Calculate absolute differences
        diff_kalman_outer = np.abs(merged[f'{var}_kalman'] - merged[f'{var}_outer'])
        diff_spline_outer = np.abs(merged[var] - merged[f'{var}_outer'])
        diff_kalman_spline = np.abs(merged[f'{var}_kalman'] - merged[var])
        
        # Plot differences
        ax.plot(merged['Date'], diff_kalman_outer, label='Kalman vs Outer', 
                linewidth=2, alpha=0.8, color='#9467bd')
        ax.plot(merged['Date'], diff_spline_outer, label='Spline vs Outer', 
                linewidth=2, alpha=0.8, color='#d62728')
        ax.plot(merged['Date'], diff_kalman_spline, label='Kalman vs Spline', 
                linewidth=2, alpha=0.8, color='#17becf')
        
        ax.set_title(f'{var} - Method Differences', fontsize=12, fontweight='bold')
        ax.set_xlabel('Date', fontsize=10)
        ax.set_ylabel('Absolute Difference', fontsize=10)
        ax.legend(loc='best')
        ax.grid(True, alpha=0.3)
        ax.tick_params(axis='x', rotation=45)
    
    plt.tight_layout()
    filename = f'approach2_differences_{material}.png'
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    plt.close()
    print(f"Saved: {filename}")


def approach_3_scatter_matrix(material):
    """
    Approach 3: Scatter Matrix Comparison
    Direct method-vs-method scatter plots show correlation and deviation patterns.
    """
    # Merge dataframes on Date
    merged = data[material]['outer'].merge(data[material]['kalman'], on='Date', suffixes=('_outer', '_kalman'))
    merged = merged.merge(data[material]['spline'], on='Date')
    
    for var in variables[material]:
        fig = plt.figure(figsize=(16, 10))
        fig.suptitle(f'Approach 3: {material.capitalize()} - Scatter Matrix for {var}', 
                     fontsize=16, fontweight='bold')
        
        # Create 2x2 grid: 3 scatter plots + 1 time series
        gs = fig.add_gridspec(2, 2, hspace=0.3, wspace=0.3)
        
        # Scatter 1: Kalman vs Outer
        ax1 = fig.add_subplot(gs[0, 0])
        ax1.scatter(merged[f'{var}_outer'], merged[f'{var}_kalman'], 
                   alpha=0.6, s=30, color='#2ca02c', edgecolors='black', linewidth=0.5)
        
        # Add diagonal line
        min_val = min(merged[f'{var}_outer'].min(), merged[f'{var}_kalman'].min())
        max_val = max(merged[f'{var}_outer'].max(), merged[f'{var}_kalman'].max())
        ax1.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2, alpha=0.5, label='Perfect Agreement')
        
        ax1.set_xlabel('Outer Join', fontsize=11)
        ax1.set_ylabel('Kalman Filter', fontsize=11)
        ax1.set_title('Kalman vs Outer Join', fontsize=12, fontweight='bold')
        ax1.grid(True, alpha=0.3)
        ax1.legend()
        
        # Scatter 2: Spline vs Outer
        ax2 = fig.add_subplot(gs[0, 1])
        ax2.scatter(merged[f'{var}_outer'], merged[var], 
                   alpha=0.6, s=30, color='#ff7f0e', edgecolors='black', linewidth=0.5)
        
        min_val = min(merged[f'{var}_outer'].min(), merged[var].min())
        max_val = max(merged[f'{var}_outer'].max(), merged[var].max())
        ax2.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2, alpha=0.5, label='Perfect Agreement')
        
        ax2.set_xlabel('Outer Join', fontsize=11)
        ax2.set_ylabel('Cubic Spline', fontsize=11)
        ax2.set_title('Spline vs Outer Join', fontsize=12, fontweight='bold')
        ax2.grid(True, alpha=0.3)
        ax2.legend()
        
        # Scatter 3: Kalman vs Spline
        ax3 = fig.add_subplot(gs[1, 0])
        ax3.scatter(merged[var], merged[f'{var}_kalman'], 
                   alpha=0.6, s=30, color='#1f77b4', edgecolors='black', linewidth=0.5)
        
        min_val = min(merged[var].min(), merged[f'{var}_kalman'].min())
        max_val = max(merged[var].max(), merged[f'{var}_kalman'].max())
        ax3.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2, alpha=0.5, label='Perfect Agreement')
        
        ax3.set_xlabel('Cubic Spline', fontsize=11)
        ax3.set_ylabel('Kalman Filter', fontsize=11)
        ax3.set_title('Kalman vs Spline', fontsize=12, fontweight='bold')
        ax3.grid(True, alpha=0.3)
        ax3.legend()
        
        # Time series comparison
        ax4 = fig.add_subplot(gs[1, 1])
        ax4.plot(merged['Date'], merged[f'{var}_outer'], label='Outer Join', 
                linewidth=2, alpha=0.8, color='#1f77b4')
        ax4.plot(merged['Date'], merged[f'{var}_kalman'], label='Kalman Filter', 
                linewidth=2, alpha=0.8, color='#2ca02c')
        ax4.plot(merged['Date'], merged[var], label='Cubic Spline', 
                linewidth=2, alpha=0.8, color='#ff7f0e')
        
        ax4.set_xlabel('Date', fontsize=11)
        ax4.set_ylabel('Value', fontsize=11)
        ax4.set_title('All Methods Over Time', fontsize=12, fontweight='bold')
        ax4.legend(loc='best')
        ax4.grid(True, alpha=0.3)
        ax4.tick_params(axis='x', rotation=45)
        
        filename = f'approach3_scatter_matrix_{material}_{var}.png'
        plt.savefig(filename, dpi=300, bbox_inches='tight')
        plt.close()
        print(f"Saved: {filename}")


def generate_summary_statistics(material):
    """
    Generate summary statistics for the differences between methods.
    """
    # Merge dataframes
    merged = data[material]['outer'].merge(data[material]['kalman'], on='Date', suffixes=('_outer', '_kalman'))
    merged = merged.merge(data[material]['spline'], on='Date')
    
    print(f"\n{'='*80}")
    print(f"SUMMARY STATISTICS: {material.upper()} - Method Differences")
    print(f"{'='*80}")
    
    for var in variables[material]:
        print(f"\n{var}:")
        print("-" * 60)
        
        # Calculate differences
        diff_kalman_outer = merged[f'{var}_kalman'] - merged[f'{var}_outer']
        diff_spline_outer = merged[var] - merged[f'{var}_outer']
        diff_kalman_spline = merged[f'{var}_kalman'] - merged[var]
        
        # Statistics
        stats = pd.DataFrame({
            'Kalman vs Outer': {
                'Mean Diff': diff_kalman_outer.mean(),
                'Abs Mean Diff': np.abs(diff_kalman_outer).mean(),
                'Max Abs Diff': np.abs(diff_kalman_outer).max(),
                'Std Dev': diff_kalman_outer.std(),
                'Correlation': merged[f'{var}_kalman'].corr(merged[f'{var}_outer'])
            },
            'Spline vs Outer': {
                'Mean Diff': diff_spline_outer.mean(),
                'Abs Mean Diff': np.abs(diff_spline_outer).mean(),
                'Max Abs Diff': np.abs(diff_spline_outer).max(),
                'Std Dev': diff_spline_outer.std(),
                'Correlation': merged[var].corr(merged[f'{var}_outer'])
            },
            'Kalman vs Spline': {
                'Mean Diff': diff_kalman_spline.mean(),
                'Abs Mean Diff': np.abs(diff_kalman_spline).mean(),
                'Max Abs Diff': np.abs(diff_kalman_spline).max(),
                'Std Dev': diff_kalman_spline.std(),
                'Correlation': merged[f'{var}_kalman'].corr(merged[var])
            }
        }).T
        
        print(stats.to_string())
    
    print(f"\n{'='*80}")


def compare_materials_by_variable():
    """
    BONUS: Compare the same variable across different materials to see if
    interpolation method performance varies by material type.
    """
    # Find common variables across all materials
    common_vars = set(variables[MATERIALS[0]])
    for material in MATERIALS[1:]:
        common_vars = common_vars.intersection(set(variables[material]))
    
    if not common_vars:
        print("\nNo common variables found across all materials.")
        return
    
    print(f"\n{'='*80}")
    print(f"CROSS-MATERIAL COMPARISON")
    print(f"Common variables: {sorted(common_vars)}")
    print(f"{'='*80}")
    
    for var in sorted(common_vars):
        fig, axes = plt.subplots(len(MATERIALS), 1, figsize=(14, 4*len(MATERIALS)))
        
        if len(MATERIALS) == 1:
            axes = [axes]
        
        fig.suptitle(f'Cross-Material Comparison: {var} - Method Differences', 
                     fontsize=16, fontweight='bold', y=0.995)
        
        for idx, material in enumerate(MATERIALS):
            ax = axes[idx]
            
            # Merge dataframes
            merged = data[material]['outer'].merge(data[material]['kalman'], on='Date', suffixes=('_outer', '_kalman'))
            merged = merged.merge(data[material]['spline'], on='Date')
            
            # Calculate absolute differences
            diff_kalman_outer = np.abs(merged[f'{var}_kalman'] - merged[f'{var}_outer'])
            diff_spline_outer = np.abs(merged[var] - merged[f'{var}_outer'])
            diff_kalman_spline = np.abs(merged[f'{var}_kalman'] - merged[var])
            
            # Plot differences
            ax.plot(merged['Date'], diff_kalman_outer, label='Kalman vs Outer', 
                    linewidth=2, alpha=0.8, color='#9467bd')
            ax.plot(merged['Date'], diff_spline_outer, label='Spline vs Outer', 
                    linewidth=2, alpha=0.8, color='#d62728')
            ax.plot(merged['Date'], diff_kalman_spline, label='Kalman vs Spline', 
                    linewidth=2, alpha=0.8, color='#17becf')
            
            ax.set_title(f'{material.capitalize()}', fontsize=12, fontweight='bold')
            ax.set_xlabel('Date', fontsize=10)
            ax.set_ylabel('Absolute Difference', fontsize=10)
            ax.legend(loc='best')
            ax.grid(True, alpha=0.3)
            ax.tick_params(axis='x', rotation=45)
        
        plt.tight_layout()
        filename = f'cross_material_comparison_{var}.png'
        plt.savefig(filename, dpi=300, bbox_inches='tight')
        plt.close()
        print(f"Saved: {filename}")


# Main execution
if __name__ == "__main__":
    print("="*80)
    print("MULTI-MATERIAL INTERPOLATION COMPARISON")
    print("Materials: " + ", ".join([m.capitalize() for m in MATERIALS]))
    print("="*80)
    
    # Generate visualizations for each material
    for material in MATERIALS:
        print(f"\n{'#'*80}")
        print(f"# Processing: {material.upper()}")
        print(f"{'#'*80}")
        
        print(f"\n--- Generating Approach 1: Side-by-Side Subplots ({material}) ---")
        approach_1_side_by_side(material)
        
        print(f"\n--- Generating Approach 2: Difference Highlighting ({material}) ---")
        approach_2_differences(material)
        
        print(f"\n--- Generating Approach 3: Scatter Matrix ({material}) ---")
        approach_3_scatter_matrix(material)
        
        # Generate summary statistics
        generate_summary_statistics(material)
    
    # Generate cross-material comparison
    print(f"\n{'#'*80}")
    print("# Generating Cross-Material Comparisons")
    print(f"{'#'*80}")
    compare_materials_by_variable()
    
    print("\n" + "="*80)
    print("✓ All visualizations generated successfully!")
    print("="*80)
    print("\nFiles saved:")
    for material in MATERIALS:
        print(f"\n{material.capitalize()}:")
        print(f"  - approach1_side_by_side_{material}.png")
        print(f"  - approach2_differences_{material}.png")
        print(f"  - approach3_scatter_matrix_{material}_[variable].png")
    print("\nCross-Material:")
    print(f"  - cross_material_comparison_[variable].png")