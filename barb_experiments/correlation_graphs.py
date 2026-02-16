import argparse
import os
from typing import Dict, List, Tuple

import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import pandas as pd
import seaborn as sns
from scipy.cluster import hierarchy
from scipy.spatial.distance import squareform
import json


def ensure_output_dir(path: str) -> None:
    os.makedirs(path, exist_ok=True)


def load_and_prepare(csv_path: str) -> pd.DataFrame:
    df = pd.read_csv(csv_path)
    if 'Date' not in df.columns:
        raise ValueError(f"Expected a 'Date' column in {csv_path}")
    df['Date'] = pd.to_datetime(df['Date'], errors='coerce')
    df = df.dropna(subset=['Date']).sort_values('Date').reset_index(drop=True)
    for col in df.columns:
        if col == 'Date':
            continue
        if not pd.api.types.is_numeric_dtype(df[col]):
            df[col] = pd.to_numeric(df[col], errors='coerce')
    return df


def detect_price_columns(df: pd.DataFrame) -> List[str]:
    numeric_cols = [c for c in df.columns if c != 'Date' and pd.api.types.is_numeric_dtype(df[c])]
    price_like = [c for c in numeric_cols if c.startswith('Price') or 'Price_' in c]
    return price_like if price_like else numeric_cols


# ----- Ticker mapping utilities (read-only) -----

def load_mapping(mapping_path: str, metal: str) -> Dict[str, str]:
    if not mapping_path or not os.path.exists(mapping_path):
        return {}
    try:
        with open(mapping_path, 'r', encoding='utf-8') as f:
            data = json.load(f)
            if isinstance(data, dict) and metal in data and isinstance(data[metal], dict):
                return {str(k): str(v) for k, v in data[metal].items()}
            return {}
    except Exception:
        return {}


def apply_ticker_mapping(price_cols: List[str], mapping_path: str, metal: str) -> Dict[str, str]:
    mapping = load_mapping(mapping_path, metal)
    # Use only columns found in the mapping; ignore others
    return {col: mapping[col] for col in price_cols if col in mapping}


# ----- Correlation/plotting -----

def compute_returns(df: pd.DataFrame, price_cols: List[str]) -> pd.DataFrame:
    returns = df[['Date'] + price_cols].copy()
    for col in price_cols:
        returns[col] = returns[col].pct_change(fill_method=None)
    returns = returns.dropna().reset_index(drop=True)
    return returns


def correlation_matrix(returns: pd.DataFrame, cols: List[str]) -> pd.DataFrame:
    return returns[cols].corr(method='pearson')


def rank_top_pairs(corr: pd.DataFrame, top_n: int) -> List[Tuple[str, str, float]]:
    pairs: List[Tuple[str, str, float]] = []
    cols = list(corr.columns)
    for i in range(len(cols)):
        for j in range(i + 1, len(cols)):
            c = corr.iloc[i, j]
            pairs.append((cols[i], cols[j], float(c)))
    pairs.sort(key=lambda x: abs(x[2]), reverse=True)
    return pairs[:top_n]


def plot_corr_heatmap(corr: pd.DataFrame, title: str, out_path: str) -> None:
    plt.figure(figsize=(10, 8))
    sns.heatmap(corr, annot=True, center=0, cmap='RdYlGn', fmt='.2f', square=True, cbar_kws={"shrink": .8})
    plt.title(title)
    plt.tight_layout()
    plt.savefig(out_path, dpi=150)
    plt.close()


def plot_rolling_correlation(returns: pd.DataFrame, pair: Tuple[str, str, float], windows: List[int], title_prefix: str, out_path: str) -> None:
    s1, s2, _ = pair
    plt.figure(figsize=(11, 6))
    for w in windows:
        roll = returns[s1].rolling(window=w).corr(returns[s2])
        plt.plot(returns['Date'], roll, label=f"{w}D")
    plt.axhline(0.0, color='k', lw=1, alpha=0.6)
    plt.legend(title='Rolling Window')
    plt.title(f"{title_prefix}: Rolling correlation {s1} vs {s2}")
    plt.xlabel('Date')
    plt.ylabel('Correlation')
    plt.tight_layout()
    plt.savefig(out_path, dpi=150)
    plt.close()


def plot_pair_scatter(returns: pd.DataFrame, pair: Tuple[str, str, float], title_prefix: str, out_path: str) -> None:
    s1, s2, c = pair
    x = returns[s1].values
    y = returns[s2].values
    mask = np.isfinite(x) & np.isfinite(y)
    x, y = x[mask], y[mask]
    if len(x) < 5:
        return
    slope, intercept = np.polyfit(x, y, 1)
    y_fit = slope * x + intercept
    ss_res = np.sum((y - y_fit) ** 2)
    ss_tot = np.sum((y - np.mean(y)) ** 2)
    r2 = 1.0 - ss_res / ss_tot if ss_tot != 0 else np.nan

    plt.figure(figsize=(7, 6))
    sns.scatterplot(x=x, y=y, s=20, alpha=0.6)
    xs = np.linspace(x.min(), x.max(), 100)
    plt.plot(xs, slope * xs + intercept, color='red', lw=2, label=f"fit (R²={r2:.2f})")
    plt.title(f"{title_prefix}: {s1} vs {s2} (corr={c:.2f})")
    plt.xlabel(f"{s1} returns")
    plt.ylabel(f"{s2} returns")
    plt.legend()
    plt.tight_layout()
    plt.savefig(out_path, dpi=150)
    plt.close()


def plot_corr_network(corr: pd.DataFrame, threshold: float, title: str, out_path: str) -> None:
    G = nx.Graph()
    for node in corr.columns:
        G.add_node(node)
    for i, u in enumerate(corr.columns):
        for j, v in enumerate(corr.columns):
            if j <= i:
                continue
            w = float(corr.iloc[i, j])
            if abs(w) >= threshold:
                G.add_edge(u, v, weight=w)

    pos = nx.spring_layout(G, seed=42)

    plt.figure(figsize=(9, 7))
    edge_colors = ['green' if corr[u][v] > 0 else 'red' for u, v in G.edges()]
    edge_widths = [2.0 * abs(corr[u][v]) for u, v in G.edges()]
    nx.draw_networkx_nodes(G, pos, node_size=700, node_color='#87ceeb')
    nx.draw_networkx_labels(G, pos, font_size=9)
    nx.draw_networkx_edges(G, pos, edge_color=edge_colors, width=edge_widths, alpha=0.8)
    plt.title(title)
    plt.axis('off')
    plt.tight_layout()
    plt.savefig(out_path, dpi=150)
    plt.close()


def plot_dendrogram(corr: pd.DataFrame, title: str, out_path: str) -> None:
    dist = 1.0 - np.abs(corr.values)
    condensed = squareform(dist, checks=False)
    linkage = hierarchy.linkage(condensed, method='average')

    plt.figure(figsize=(10, 6))
    hierarchy.dendrogram(linkage, labels=list(corr.columns), leaf_rotation=90)
    plt.title(title)
    plt.tight_layout()
    plt.savefig(out_path, dpi=150)
    plt.close()


def run_for_metal(metal: str, csv_path: str, output_dir: str, rolling_windows: List[int], top_n_pairs: int, network_threshold: float, mapping_path: str) -> Dict[str, List[str]]:
    ensure_output_dir(output_dir)

    df = load_and_prepare(csv_path)
    price_cols_all = detect_price_columns(df)

    # Map only columns with explicit tickers for this metal
    col_to_ticker = apply_ticker_mapping(price_cols_all, mapping_path, metal)
    price_cols = list(col_to_ticker.keys())
    if len(price_cols) < 2:
        raise ValueError(f"Not enough mapped price columns for {metal} in {mapping_path}")

    rets = compute_returns(df, price_cols)
    rename_map = {col: col_to_ticker[col] for col in price_cols}
    rets = rets.rename(columns=rename_map)

    tickers = list(rename_map.values())
    corr = correlation_matrix(rets, tickers)

    written: Dict[str, List[str]] = {'heatmap': [], 'rolling': [], 'scatter': [], 'network': [], 'dendrogram': []}

    heatmap_path = os.path.join(output_dir, f"{metal}_corr_heatmap.png")
    plot_corr_heatmap(corr, f"{metal.capitalize()} - Correlation Matrix (Returns)", heatmap_path)
    written['heatmap'].append(heatmap_path)

    top_pairs = rank_top_pairs(corr, top_n_pairs)

    for idx, pair in enumerate(top_pairs, start=1):
        roll_path = os.path.join(output_dir, f"{metal}_rolling_corr_pair{idx}.png")
        plot_rolling_correlation(rets, pair, rolling_windows, metal.capitalize(), roll_path)
        written['rolling'].append(roll_path)

        scatter_path = os.path.join(output_dir, f"{metal}_scatter_pair{idx}.png")
        plot_pair_scatter(rets, pair, metal.capitalize(), scatter_path)
        written['scatter'].append(scatter_path)

    network_path = os.path.join(output_dir, f"{metal}_corr_network.png")
    plot_corr_network(corr, network_threshold, f"{metal.capitalize()} - Correlation Network (|corr|≥{network_threshold})", network_path)
    written['network'].append(network_path)

    dendro_path = os.path.join(output_dir, f"{metal}_corr_dendrogram.png")
    plot_dendrogram(corr, f"{metal.capitalize()} - Correlation Dendrogram", dendro_path)
    written['dendrogram'].append(dendro_path)

    return written


def build_default_inputs(use_interpolated: bool) -> Dict[str, str]:
    base = os.getcwd()
    data_dir = os.path.join(base, 'data')

    def pick(name_outer: str) -> str:
        if use_interpolated:
            return os.path.join(data_dir, f"ALL_{name_outer}_prices_interpolated.csv")
        return os.path.join(data_dir, f"ALL_{name_outer}_prices_outer.csv")

    return {
        'lithium': pick('lithium'),
        'nickel': pick('nickel'),
        'cobalt': pick('cobalt'),
        'copper': pick('copper'),
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Generate PNG correlation visualizations for metals.')
    parser.add_argument('--use-interpolated', action='store_true')
    parser.add_argument('--output-dir', type=str, default=os.path.join('barb_experiments', 'output'))
    parser.add_argument('--rolling-windows', type=str, default='30,60')
    parser.add_argument('--top-n', type=int, default=5)
    parser.add_argument('--threshold', type=float, default=0.6)
    parser.add_argument('--mapping-file', type=str, default=os.path.join('config', 'ticker_mapping.json'))
    parser.add_argument('--lithium', type=str)
    parser.add_argument('--nickel', type=str)
    parser.add_argument('--cobalt', type=str)
    parser.add_argument('--copper', type=str)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    rolling_windows = [int(x) for x in args.rolling_windows.split(',') if x.strip()]
    inputs = build_default_inputs(args.use_interpolated)
    if args.lithium:
        inputs['lithium'] = args.lithium
    if args.nickel:
        inputs['nickel'] = args.nickel
    if args.cobalt:
        inputs['cobalt'] = args.cobalt
    if args.copper:
        inputs['copper'] = args.copper

    out_base = args.output_dir
    ensure_output_dir(out_base)

    for metal, path in inputs.items():
        metal_out = os.path.join(out_base, metal)
        ensure_output_dir(metal_out)
        run_for_metal(
            metal=metal,
            csv_path=path,
            output_dir=metal_out,
            rolling_windows=rolling_windows,
            top_n_pairs=args.top_n,
            network_threshold=args.threshold,
            mapping_path=args.mapping_file,
        )


if __name__ == '__main__':
    main()
