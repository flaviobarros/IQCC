# IQCC

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/IQCC)](https://cran.r-project.org/package=IQCC)
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

## Learning more

The package includes vignettes that explain where IQCC fits and how to approach selected monitoring problems:

```r
vignette("iqcc-positioning", package = "IQCC")
vignette("high-quality-processes", package = "IQCC")
```

- `iqcc-positioning` explains when IQCC is a better fit than a classical normal-approximation chart and how it complements broader SPC packages.
- `high-quality-processes` focuses on rare nonconformities, Cornish-Fisher p charts, and the DS-np numerical core.

## Research background

IQCC was developed from research on improved statistical quality control charts, especially work associated with Emanuel Pimentel Barbosa and collaborators. The package emphasizes cases where classical Shewhart-type limits are simple and familiar but statistically inaccurate.

Important methodological themes include:

- **Cornish-Fisher quantile correction** for attribute charts in high-quality processes, where the nonconforming proportion is very small and the binomial distribution is highly skewed.
- **Exact range-chart limits** using the relative range distribution, implemented through the Tukey distribution, to avoid the false alarm inflation of normal-based R charts.
- **Exact probability limits** for S charts based on the chi-square distribution.
- **Hotelling T² monitoring** for multivariate process mean vectors.

## Development roadmap

The package currently implements several core ideas from the research program, but some published methods are not yet implemented. Planned or candidate extensions include:

| Candidate extension | Statistical target | Possible function names | Status |
|---|---|---|---|
| Double-sampling np chart | Nonconforming proportion in high-quality processes with small samples | `dsnp_limits()`, `cchart.DSnp()` | Implemented |
| Generalized variance chart | Multivariate process variability using `|S|` | `gv_limits()`, `cchart.GV()` | Planned |
| Cornish-Fisher corrected generalized variance chart | Corrected limits for `|S|` under non-normal sampling distribution | `gv_cf_limits()` | Planned |
| Auxiliary trace chart | Complementary monitoring using `tr(V)` | `trv_limits()`, `cchart.trV()` | Planned |
| Numerical validation tables | Reproduce selected values from the underlying papers | `tests/testthat/` fixtures | Planned |

These extensions should be implemented incrementally, with pure numerical functions separated from plotting functions and with validation against published examples whenever possible.

## References

- Montgomery, D.C. (2008). *Introduction to Statistical Quality Control*. 6th ed. Wiley.
- Barros, F. et al. (2017). IQCC: An R Package for Improved Quality Control Charts. *Journal of Statistical Software*.
- Joekes, S. and Barbosa, E.P. (2013). An improved attribute control chart for monitoring non-conforming proportion in high quality processes. *Control Engineering Practice*.
- Barbosa, E.P., Gneri, M.A. and Meneguetti, A. (2013). Range Control Charts Revisited: Simpler Tippett-like Formulae, Its Practical Implementation, and the Study of False Alarm. *Communications in Statistics - Simulation and Computation*.
- Joekes, S., Smrekar, M. and Barbosa, E.P. (2015). Extending a double sampling control chart for non-conforming proportion in high quality processes to the case of small samples. *Statistical Methodology*.
