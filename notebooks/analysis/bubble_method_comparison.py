"""
Compare bubble detection methods across Cobalt, Copper, Lithium, and Nickel.

- GSADF: binary labels from R (`exuber`). This script prefers **`df_master_gsadf.csv`**
  (from `gsadf_exuber_master_prep.R`); otherwise it uses `*_BD` columns from `df_master.csv`.
  There is no drop-in Python equivalent to `exuber`.

- LPPLS: labels from `lppls_data_prep.ipynb` output (`df_master_lppls.csv` or
  `R/data_R/df_master_lppls.csv`).

- CUSUM: labels from `cusum_data_prep.ipynb` output (`df_master_cusum.csv`).

Paths are resolved relative to this file; several common locations are tried.

**Notebook:** `notebooks/analysis/bubble_method_comparison.ipynb` loads this module and calls
`run_comparison()` (Jupyter cannot use ``argparse`` the same way as ``python bubble_method_comparison.py``).

Usage (from repo root or this directory):

    python notebooks/analysis/bubble_method_comparison.py

Optional: recompute CUSUM from prices in the GSADF master if the CUSUM file is
missing (same logic as `cusum_data_prep.ipynb`):

    python notebooks/analysis/bubble_method_comparison.py --recompute-cusum
"""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

try:
    from sklearn.metrics import cohen_kappa_score
except ImportError:
    cohen_kappa_score = None

# Representative DALY series — same mapping as lppls_data_prep / cusum_data_prep
METAL_PRICE_COL = {
    "Cobalt": "CODALY",
    "Copper": "CUDALY",
    "Lithium": "LIDALY",
    "Nickel": "NIDALY",
}



def _script_dir() -> Path:
    return Path(__file__).resolve().parent


def _repo_candidates() -> list[Path]:
    """Prefer metals repo root (`.../metals`), then `notebooks` parent."""
    d = _script_dir()
    return [d.parents[2], d.parents[1]]


def _first_existing(rel_paths: list[str]) -> Path | None:
    for root in _repo_candidates():
        for rel in rel_paths:
            p = (root / rel).resolve()
            if p.is_file():
                return p
    return None


def read_master_csv(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path, sep=";", decimal=",", index_col=0)
    df["Date"] = pd.to_datetime(df["Date"])
    df = df.sort_values("Date").reset_index(drop=True)
    return df


def jaccard_binary(a: np.ndarray, b: np.ndarray) -> float:
    a = a.astype(bool)
    b = b.astype(bool)
    inter = np.logical_and(a, b).sum()
    union = np.logical_or(a, b).sum()
    return float(inter / union) if union > 0 else 1.0


def bubble_precision_recall(pred: np.ndarray, ref: np.ndarray) -> tuple[float, float]:
    """Treat `ref` as GSADF (reference bubble set). pred = other method."""
    pred = pred.astype(bool)
    ref = ref.astype(bool)
    tp = np.logical_and(pred, ref).sum()
    prec = float(tp / pred.sum()) if pred.sum() > 0 else np.nan
    rec = float(tp / ref.sum()) if ref.sum() > 0 else np.nan
    return prec, rec



def cusum_bubble_detector(
    log_prices: np.ndarray, window: int = 120, threshold_sigma: float = 2.0
) -> np.ndarray:
    """Rolling CUSUM on log-price first differences (same as cusum_data_prep.ipynb)."""
    T = len(log_prices)
    dy = np.diff(log_prices)
    bubble = np.zeros(T, dtype=int)
    for end in range(window, T):
        start = end - window
        segment = dy[start:end]
        local_sigma = float(np.sqrt(np.mean(segment**2)))
        if local_sigma < 1e-10:
            continue
        cusum = np.cumsum(segment) / (local_sigma * np.sqrt(window))
        if np.max(cusum) > threshold_sigma:
            bubble[end] = 1
    return bubble


def smooth_bubble_labels(
    bubble: np.ndarray, min_gap: int = 5, min_duration: int = 10
) -> np.ndarray:
    """Post-process labels: fill gaps ≤ min_gap, then drop episodes < min_duration (notebook order)."""
    result = bubble.astype(int).copy()
    in_gap, gap_start = False, 0
    for i in range(len(result)):
        if result[i] == 0 and (i == 0 or result[i - 1] == 1):
            gap_start = i
            in_gap = True
        elif result[i] == 1 and in_gap:
            if i - gap_start <= min_gap:
                result[gap_start:i] = 1
            in_gap = False
    in_ep, ep_start = False, 0
    for i in range(len(result)):
        if result[i] == 1 and not in_ep:
            ep_start = i
            in_ep = True
        elif (result[i] == 0 or i == len(result) - 1) and in_ep:
            end_i = i if result[i] == 0 else i + 1
            if end_i - ep_start < min_duration:
                result[ep_start:end_i] = 0
            in_ep = False
    return result



def trend_filter_ols_window(raw: np.ndarray, prices: np.ndarray, lag: int = 5) -> np.ndarray:
    """Same as lppls_data_prep / cusum_data_prep: OLS slope > 0 on price levels over [t-lag, …, t]."""
    raw = np.asarray(raw, dtype=int).copy()
    prices = np.asarray(prices, dtype=float)
    for i in range(len(raw)):
        if raw[i] != 1:
            continue
        if i < lag:
            raw[i] = 0
            continue
        seg = prices[i - lag : i + 1]
        if len(seg) < 3 or np.nanstd(seg) == 0:
            raw[i] = 0
            continue
        x = np.arange(len(seg), dtype=float)
        slope = np.polyfit(x, seg, 1)[0]
        if not np.isfinite(slope) or slope <= 0:
            raw[i] = 0
    return raw


def compute_cusum_flags(df: pd.DataFrame) -> dict[str, np.ndarray]:
    """Multi-scale CUSUM + smoothing + trend filter (cusum_data_prep defaults)."""
    WINDOW_SHORT = 60
    WINDOW_LONG = 120
    THRESHOLD_SIGMA = 2.0
    MIN_GAP = 5
    MIN_DURATION = 10
    out: dict[str, np.ndarray] = {}
    for metal, col in METAL_PRICE_COL.items():
        prices = df[col].values.astype(float)
        log_p = np.log(np.maximum(prices, 0.001))
        cs_short = cusum_bubble_detector(log_p, window=WINDOW_SHORT, threshold_sigma=THRESHOLD_SIGMA)
        cs_long = cusum_bubble_detector(log_p, window=WINDOW_LONG, threshold_sigma=THRESHOLD_SIGMA)
        union = ((cs_short == 1) | (cs_long == 1)).astype(int)
        smoothed = smooth_bubble_labels(union, min_gap=MIN_GAP, min_duration=MIN_DURATION)
        out[metal] = trend_filter_ols_window(smoothed, prices, lag=5)
    return out


def align_three_masters(
    df_gsadf: pd.DataFrame, df_lppls: pd.DataFrame, df_cusum: pd.DataFrame
) -> pd.DataFrame:
    """Inner-join on Date so all three methods share identical rows."""
    a = df_gsadf.rename(
        columns={f"{c}_BD": f"{c}_BD_gsadf" for c in METAL_PRICE_COL.values()}
    )
    b = df_lppls.rename(
        columns={f"{c}_BD": f"{c}_BD_lppls" for c in METAL_PRICE_COL.values()}
    )[["Date"] + [f"{c}_BD_lppls" for c in METAL_PRICE_COL.values()]]
    c = df_cusum.rename(
        columns={f"{c}_BD": f"{c}_BD_cusum" for c in METAL_PRICE_COL.values()}
    )[["Date"] + [f"{c}_BD_cusum" for c in METAL_PRICE_COL.values()]]
    m = a.merge(b, on="Date", how="inner").merge(c, on="Date", how="inner")
    return m.sort_values("Date").reset_index(drop=True)



def plot_timeline_raster(df: pd.DataFrame, out_path: Path) -> None:
    """Three-row stripe per metal: GSADF, LPPLS, CUSUM."""
    dates = df["Date"].values
    fig, axes = plt.subplots(4, 1, figsize=(14, 10), sharex=True)
    methods = ["gsadf", "lppls", "cusum"]
    for ax, (metal, col) in zip(axes, METAL_PRICE_COL.items()):
        mats = np.vstack(
            [
                df[f"{col}_BD_{m}"].values.astype(float)
                for m in methods
            ]
        )
        ax.imshow(mats, aspect="auto", interpolation="nearest", cmap="Purples", vmin=0, vmax=1)
        ax.set_yticks([0, 1, 2])
        ax.set_yticklabels(["GSADF", "LPPLS", "CUSUM"])
        ax.set_ylabel(metal, rotation=0, labelpad=40, fontsize=10, va="center")
    axes[-1].set_xlabel("Trading day index")
    fig.suptitle("Bubble flags (white=calm, purple=bubble) — aligned sample", fontsize=12)
    fig.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    plt.close(fig)


def plot_jaccard_heatmap(pairwise: pd.DataFrame, out_path: Path) -> None:
    """Rows = metals, cols = method pairs."""
    pivot = pairwise.pivot(index="metal", columns="pair", values="jaccard")
    fig, ax = plt.subplots(figsize=(8, 4))
    im = ax.imshow(pivot.values.astype(float), aspect="auto", cmap="YlOrRd", vmin=0, vmax=1)
    ax.set_xticks(range(len(pivot.columns)))
    ax.set_xticklabels(pivot.columns, rotation=25, ha="right")
    ax.set_yticks(range(len(pivot.index)))
    ax.set_yticklabels(pivot.index)
    for i in range(pivot.shape[0]):
        for j in range(pivot.shape[1]):
            ax.text(j, i, f"{pivot.values[i, j]:.2f}", ha="center", va="center", color="black", fontsize=9)
    fig.colorbar(im, ax=ax, label="Jaccard")
    ax.set_title("Pairwise Jaccard index on bubble-day sets")
    fig.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    plt.close(fig)


def run_comparison(
    recompute_cusum: bool = False,
    output_dir: str | Path | None = None,
) -> dict:
    """
    Load GSADF / LPPLS / CUSUM masters, align on Date, write CSVs + figures under `outputs/`.

    Returns a dict with keys: ``prevalence``, ``pairwise``, ``kappa`` (optional), ``merged``, ``out_dir``, ``paths``.
    Use from a Jupyter notebook instead of ``main()`` (argparse breaks in notebooks).
    """
    p_gsadf = _first_existing(
        [
            "R/df_master_gsadf.csv",
            "R/data_R/df_master_gsadf.csv",
            "R/data_R/df_master.csv",
            "R/df_master.csv",
            "metals/R/df_master.csv",
        ]
    )
    if p_gsadf is None:
        raise FileNotFoundError(
            "Could not find a GSADF master. Tried df_master_gsadf.csv then df_master.csv under R/ or R/data_R/."
        )

    p_lppls = _first_existing(
        [
            "R/df_master_lppls.csv",
            "R/data_R/df_master_lppls.csv",
        ]
    )
    p_cusum = _first_existing(["R/df_master_cusum.csv"])

    df_g = read_master_csv(p_gsadf)
    if p_lppls is None:
        raise FileNotFoundError(
            "Could not find df_master_lppls.csv. Run notebooks/analysis/lppls_data_prep.ipynb first."
        )
    df_l = read_master_csv(p_lppls)

    if recompute_cusum or p_cusum is None:
        cusum_by_metal = compute_cusum_flags(df_g)
        df_c = df_g.copy()
        for metal, col in METAL_PRICE_COL.items():
            df_c[f"{col}_BD"] = cusum_by_metal[metal]
        if p_cusum is None and not recompute_cusum:
            print("Note: df_master_cusum.csv not found — recomputing CUSUM from prices.")
    else:
        df_c = read_master_csv(p_cusum)

    df = align_three_masters(df_g, df_l, df_c)
    print(f"Aligned panel: {len(df)} rows, {df['Date'].min().date()} — {df['Date'].max().date()}")
    print(f"GSADF master: {p_gsadf}")
    print(f"LPPLS master: {p_lppls}")
    print(f"CUSUM source: {'recomputed' if recompute_cusum or p_cusum is None else p_cusum}")

    out_dir = Path(output_dir) if output_dir else _script_dir().parents[2] / "outputs"
    out_dir.mkdir(parents=True, exist_ok=True)

    rows_prev = []
    rows_pairs = []
    rows_kappa = []

    pairs = [("gsadf", "lppls"), ("gsadf", "cusum"), ("lppls", "cusum")]

    for metal, col in METAL_PRICE_COL.items():
        g = df[f"{col}_BD_gsadf"].values.astype(int)
        l = df[f"{col}_BD_lppls"].values.astype(int)
        c = df[f"{col}_BD_cusum"].values.astype(int)
        for name, x in [("GSADF", g), ("LPPLS", l), ("CUSUM", c)]:
            rows_prev.append(
                {"metal": metal, "method": name, "bubble_days": int(x.sum()), "pct": 100.0 * x.mean()}
            )
        for m1, m2 in pairs:
            a = {"gsadf": g, "lppls": l, "cusum": c}[m1]
            b = {"gsadf": g, "lppls": l, "cusum": c}[m2]
            j = jaccard_binary(a, b)
            rows_pairs.append({"metal": metal, "pair": f"{m1}–{m2}", "jaccard": j})
            if cohen_kappa_score is not None:
                k = cohen_kappa_score(a, b)
                rows_kappa.append({"metal": metal, "pair": f"{m1}–{m2}", "cohen_kappa": k})
        for mname, x in [("LPPLS", l), ("CUSUM", c)]:
            prec, rec = bubble_precision_recall(x, g)
            rows_pairs.append(
                {
                    "metal": metal,
                    "pair": f"{mname}_vs_GSADF_PR",
                    "precision_vs_gsadf": prec,
                    "recall_vs_gsadf": rec,
                }
            )

    prev_df = pd.DataFrame(rows_prev)
    pair_df = pd.DataFrame(rows_pairs)
    prev_path = out_dir / "bubble_methods_prevalence.csv"
    pair_path = out_dir / "bubble_methods_pairwise_metrics.csv"
    prev_df.to_csv(prev_path, index=False)
    pair_df.to_csv(pair_path, index=False)
    print(f"Wrote {prev_path}")
    print(f"Wrote {pair_path}")

    kdf = None
    if cohen_kappa_score is None:
        print("Install scikit-learn for Cohen's kappa in pairwise export.")
    else:
        kdf = pd.DataFrame(rows_kappa)
        kp = out_dir / "bubble_methods_cohen_kappa.csv"
        kdf.to_csv(kp, index=False)
        print(f"Wrote {kp}")

    for metal, col in METAL_PRICE_COL.items():
        s = (
            df[f"{col}_BD_gsadf"].values.astype(int)
            + df[f"{col}_BD_lppls"].values.astype(int)
            + df[f"{col}_BD_cusum"].values.astype(int)
        )
        counts = pd.Series(s).value_counts().sort_index()
        print(f"\n{metal} — count of methods flagging bubble (0..3):\n{counts.to_string()}")

    j_only = pair_df[pair_df["pair"].str.contains("–")].copy()
    heatmap_path = out_dir / "bubble_methods_jaccard_heatmap.png"
    raster_path = out_dir / "bubble_methods_timeline_raster.png"
    plot_jaccard_heatmap(j_only, heatmap_path)
    plot_timeline_raster(df, raster_path)
    print(f"Figures saved under {out_dir}")

    print(
        "\nR (GSADF via exuber): use `gsadf_exuber_master_prep.R` / `df_master_gsadf.csv` so this comparison "
        "stays consistent with lppls_data_prep.ipynb and cusum_data_prep.ipynb."
    )

    return {
        "prevalence": prev_df,
        "pairwise": pair_df,
        "kappa": kdf,
        "merged": df,
        "out_dir": out_dir,
        "paths": {
            "prevalence_csv": prev_path,
            "pairwise_csv": pair_path,
            "heatmap_png": heatmap_path,
            "raster_png": raster_path,
        },
    }


def main() -> None:
    parser = argparse.ArgumentParser(description="Compare GSADF vs LPPLS vs CUSUM bubble labels.")
    parser.add_argument(
        "--recompute-cusum",
        action="store_true",
        help="Ignore CUSUM CSV and recompute from prices in the GSADF master.",
    )
    parser.add_argument(
        "--output-dir",
        type=str,
        default="",
        help="Directory for figures/CSVs (default: ../../outputs next to R/).",
    )
    args = parser.parse_args()
    run_comparison(recompute_cusum=args.recompute_cusum, output_dir=args.output_dir or None)


if __name__ == "__main__":
    main()

