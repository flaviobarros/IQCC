# IQCC Replication Package

This directory contains the replication materials for the IQCC R package,
intended to support a future submission to the Journal of Statistical Software (JSS).

## Structure

```
paper/
├── article.Rnw          # JSS article source (future)
├── references.bib       # Bibliography
├── replication/
│   ├── README.md        # This file
│   ├── fast/            # Quick replication (minutes)
│   │   └── code.R       # Key numerical results
│   └── full/            # Comprehensive replication (future)
├── figures/             # Generated figures
└── tables/              # Generated tables
```

## Requirements

- R >= 3.5.0
- IQCC package (install from source or GitHub)
- Packages: qcc, MASS, miscTools

## How to Run

### Fast replication

```r
source("replication/fast/code.R")
```

This reproduces the key numerical results from the IQCC base papers:
- Cornish-Fisher corrected p-chart limits (Joekes & Barbosa, 2013)
- Exact R-chart limits (Barbosa, Gneri & Meneguetti, 2013)
- DS-np double-sampling design (Joekes, Smrekar & Barbosa, 2015)
- Hotelling T² asymptotic robustness
- Generalized variance |S| limits
- Trace statistic tr(V) limits

Expected runtime: under 5 minutes.

### Full replication (future)

Will include exhaustive reproduction of all tables and figures
from the IQCC base papers, with full Monte Carlo simulations.

## Version

IQCC 0.8.0 (2026)
