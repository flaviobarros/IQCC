# IQCC

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/IQCC)](https://cran.r-project.org/package=IQCC)
[![Codecov test coverage](https://codecov.io/gh/flaviobarros/IQCC/branch/master/graph/badge.svg)](https://app.codecov.io/gh/flaviobarros/IQCC)
[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![packageversion](https://img.shields.io/badge/Package%20version-0.7-orange.svg?style=flat-square)](commits/master)
[![DOI](https://zenodo.org/badge/18469916.svg)](https://zenodo.org/badge/latestdoi/18469916)
[![](https://cranlogs.r-pkg.org/badges/IQCC)](https://cran.r-project.org/package=IQCC)

**IQCC** implements *Improved Quality Control Charts*: statistical process control charts with exact, corrected, or standardized limits for univariate and multivariate monitoring.

The package is motivated by a recurring practical problem in classical Shewhart-type control charts: when the statistic being monitored is discrete, skewed, bounded, or strongly non-normal, the usual normal-based three-sigma limits can be badly misplaced. In those cases, the nominal false alarm risk may be very different from the actual one. IQCC provides tools that keep the familiar control-chart workflow while using more appropriate probability limits whenever possible.

## Main features

- **Univariate control charts**: X-bar, R, S, p, and u charts.
- **Improved probability limits**: exact limits, Cornish-Fisher corrected limits, and standardized variants.
- **Multivariate control charts**: Hotelling T² charts for Phase I and Phase II monitoring.
- **False alarm diagnostics**: tools for evaluating the actual false alarm risk of selected classical charts.
- **Phase I and Phase II support**: retrospective analysis and prospective process monitoring.

## Implemented methods

| Monitoring problem | Function(s) | Implemented methods | Notes |
|---|---|---|---|
| Mean of a univariate process | `cchart.Xbar()`, `cchart.Xbar1()`, `cchart.Xbar2()`, `cchart.Xbar_R()` | Shewhart-type X-bar charts | Includes support for X-bar/R workflows. |
| Range / process dispersion | `cchart.R()` | Shewhart R chart; exact Tukey-based R chart | The exact chart uses the relative range distribution through the Tukey distribution. |
| Standard deviation | `cchart.S()` | Normalized S chart; exact chi-square-based S chart | Exact limits are based on the chi-square distribution of the sample variance. |
| Nonconforming proportion | `cchart.p()` | Shewhart p chart; Cornish-Fisher p chart; standardized p chart | The Cornish-Fisher option is designed for low nonconforming proportions where normal approximation is poor. |
| Double-sampling np chart | `dsnp_prob_accept()`, `dsnp_arl()`, `dsnp_ass()`, `dsnp_limits()`, `cchart.DSnp()` | DS-np numerical core, limit search, and control chart | Two-stage sampling for high-quality processes with small samples. |
| Nonconformities per unit | `cchart.u()` | Shewhart u chart; standardized u chart | Attribute chart for counts per inspection unit. |
| Multivariate mean vector | `T2.1()`, `T2.2()`, `cchart.T2.1()`, `cchart.T2.2()` | Hotelling T² charts for Phase I and Phase II | Supports individual and subgroup observations. |
| Relative range constants | `d2()`, `d3()` | Numerical integration using Tukey distribution functions | Used by exact R-chart calculations and false alarm diagnostics. |
| False alarm risk | `alpha.risk()` | Exact false alarm probability for the classical three-sigma R chart | Useful for diagnosing inflated false alarm risk. |

## Installation

Install the CRAN version:

```r
install.packages("IQCC", dependencies = TRUE)
```

Or install the development version from GitHub:

```r
devtools::install_github("flaviobarros/IQCC")
```

## Quick start

```r
library(IQCC)

# X-bar and R charts
data(pistonrings)
cchart.Xbar_R(pistonrings[1:25, ], 5)

# Exact R chart using Phase I data to estimate sigma
cchart.R(pistonrings[26:40, ], 5, type = "tukey", y = pistonrings[1:25, ])

# p chart with Cornish-Fisher limits
data(binomdata)
attach(binomdata)
cchart.p(
  x1 = Di[1:12], n1 = ni[1:12],
  type = "CF",
  x2 = Di[13:25], n2 = ni[13:25]
)

# Hotelling T² chart (Phase I)
mu <- c(5.682, 88.22)
Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
datum <- data.1(20, 10, mu, Sigma)
estat <- stats(datum, 20, 10, 2)
T2 <- T2.1(estat, 20, 10)
cchart.T2.1(T2, 20, 10, 2)
```
