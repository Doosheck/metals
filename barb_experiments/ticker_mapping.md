# Ticker Mapping Overview

This document describes the mapping from original price series column names to compact 6-letter tickers used in figures and tables.

- Tick format: 6 uppercase letters
- Prefix: first two letters denote the metal family
  - LI = Lithium, NI = Nickel, CO = Cobalt, CU = Copper (fallback MX)
- Body: derived from source/exchange keywords; unique per series
- Mapping is stored in `barb_experiments/ticker_mapping.json` and auto-extended for new columns

## LaTeX Table (for inclusion in the paper)
Copy-paste the following into your LaTeX document:

```latex
\begin{table}[ht]
\centering
\begin{tabular}{ll}
\toprule
Original series name & Ticker \\
\midrule
Price\_Lithium & LILITM \\
Price\_SouthAmericaLOB & LISALB \\
Price\_Lithium\_Hydroxide & LILIOH \\
Price\_Lithium\_Americas & LILAMC \\
Price\_EastAsia\_Lithium\_Carbonate & LIEALC \\
Price\_EastAsia\_Lithium\_Carbonate\_Battery\_Grade & LIEABG \\
Price\_LME\_Lithium & LILMEL \\
Price\_Nickel & NINIKL \\
Price\_LME & NILMEN \\
Price\_ETF & NIETFN \\
Price\_SHFE & NISHFN \\
Price\_WUXI & NIWUXI \\
Price\_India & NIINDA \\
Price\_Cobalt & COCOBL \\
Price\_LME\_All\_Location\_Stock & COLMAS \\
Price\_COMEX & COCMXC \\
Price\_Dailymetal & CUDMET \\
Price\_COMEX\_Copper & CUCMXC \\
Price\_LME\_3M & CULME3 \\
Price\_SMM\_Guixi & CUSMMG \\
Price\_SHFE & CUSHFE \\
Price\_ETF\_Copper & CUETFC \\
\bottomrule
\end{tabular}
\caption{Mapping from original series names to 6-letter, metal-prefixed tickers.}
\label{tab:ticker_mapping}
\end{table}
```

## Notes
- If a new series appears, the script generates a new unique ticker and updates `ticker_mapping.json`.
- To force a specific code, edit `barb_experiments/ticker_mapping.json` and re-run the script.
