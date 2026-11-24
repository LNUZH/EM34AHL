# Replication of Pawar & Munuswamy (2024)

Replication package for Problem Set 3, Empirical Methods (University of Zurich, HS 2026).

Pawar, D. S., & Munuswamy, J. (2024). Does environmental reporting of banks affect their
financial performance? Evidence from India. *International Journal of Bank Marketing*, 42(4),
745–767. https://doi.org/10.1108/IJBM-12-2022-0545

The authors did not publish replication code. This repository contains only our own code and
the authors' public data.

## Data

Munuswamy, J., & Pawar, D. S. (2024). *Dataset on Does environmental reporting of banks affect
their financial performance? Evidence from India* (Version 1) [Data set]. Mendeley Data.
https://doi.org/10.17632/vtxmwt2958.1 (CC BY 4.0, accessed 6 November 2026)

| File | Content |
|---|---|
| `Banks financial performance and Env score.xlsx` | Panel of 10 banks × fiscal years 2017–2021 (Sheet1 used; Sheet2 is an identical copy) |
| `Environmenmental Reporting of Banks in India Coding and Results.xlsx` | Content-analysis coding; sheet `Results` gives the ENV totals (Table 5) |

The raw files are never modified; all cleaning is done in the script. File names are kept as
published by the authors (including the typo "Environmenmental").

## Folder structure

```
├── PS3_Replication.Rproj   RStudio project (open this first)
├── PS3_Solution.R          master script, runs the whole analysis from the raw data
├── README.md               this file
├── <two raw Excel files>   see Data
└── output/                 created by the script
    ├── tables/             CSV files (see below)
    └── figures/            PDF figures (see below)
```

## Software

R 4.4.1; packages readxl 1.4.5, plm 2.6-7, sandwich 3.1-3, lmtest 0.9-40
(versions from `sessionInfo()` at the end of the script). Install once:

```r
install.packages(c("readxl", "plm", "sandwich", "lmtest"))
```

## How to run

1. Open `PS3_Replication.Rproj` (sets the working directory; all paths are relative).
2. Run `PS3_Solution.R` from top to bottom (Source). It creates `output/` and all files below.
   No step depends on random numbers; `set.seed(12345)` is set anyway.

## Which section produces which result

| Script section | Content | Output | In the report |
|---|---|---|---|
| (a)–(b) | Setup, import and renaming | – | – |
| (c) | Data checks: balance, duplicates, copied series, sign conflicts, ENV vs. coding file, within-bank variance | console | Section 2.1 |
| (d1)–(d2) | Table 5 (ranking) and Table 6 (descriptives, EViews conventions) | `table5_ours.csv`, `table6_ours.csv` | Sections 2.1, 3.2 |
| (d3)–(d4) | ENV by bank over time; ROA/ROE against ENV | `fig1_env_by_bank.pdf`, `fig2_scatter_env.pdf` | Figure 1 = `fig2_scatter_env.pdf` |
| (d5) | ENV slope by set of controls | `sign_by_controls.csv` | Sections 3.2, 4.1 |
| (d6) | Omitted variable bias formula for net NPA | `ovb_npa.csv` | Section 4.1 |
| (e) | Pooled OLS, random effects (Swamy–Arora), fixed effects; LM and Hausman tests; SE types | `regressions_ours.csv` | Table 1 |
| (f) | Comparison with the published Tables 8–10; deviations | `comparison_full.csv`, `deviations.csv` | Section 3.1, Appendix A.3 |
| (g) | Extension A–G: bank + year FE, without IDFC First, lagged ENV, first differences, lag without FE; clustered and conventional SE | `extension.csv` | Table 2, Section 5.1 |
