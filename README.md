# IQCC

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/IQCC)](https://cran.r-project.org/package=IQCC)
[![Codecov test coverage](https://codecov.io/gh/flaviobarros/IQCC/branch/master/graph/badge.svg)](https://app.codecov.io/gh/flaviobarros/IQCC)
![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![packageversion](https://img.shields.io/badge/Package%20version-0.8.0-orange.svg?style=flat-square)](commits/master)
[![DOI](https://zenodo.org/badge/18469916.svg)](https://zenodo.org/badge/latestdoi/18469916)
[![](https://cranlogs.r-pkg.org/badges/IQCC)](https://cran.r-project.org/package=IQCC)

**IQCC** implements *Improved Quality Control Charts*: statistical process control charts with exact, corrected, standardized, or simulation-based limits for univariate and multivariate monitoring.

The package is motivated by a recurring practical problem in classical Shewhart-type control charts: when the statistic being monitored is discrete, skewed, bounded, or strongly non-normal, usual normal-based three-sigma limits can be badly misplaced. In those cases, the nominal false-alarm risk may differ substantially from the actual one. IQCC keeps the familiar control-chart workflow while exposing numerical functions that make limits, risk, ARL, and sample-size calculations reproducible and testable.

## Main features

- **Univariate control charts**: X-bar, R, S, p, and u charts.
- **Improved probability limits**: exact, Cornish-Fisher corrected, standardized, and simulation-based limits.
- **High-quality processes**: corrected p charts and double-sampling np charts for rare nonconformities.
- **Multivariate monitoring**: Hotelling T² charts, generalized variance
  charts, and auxiliary `tr(V)` variability charts.
- **False-alarm diagnostics**: exact binomial, Poisson, range-chart, and generalized variance risk calculations where available.
- **Phase I and Phase II support**: retrospective estimation and prospective monitoring.
- **Research-oriented numerical layer**: pure functions separated from plotting interfaces for validation and simulation studies.

## Implemented methods

| Monitoring problem | Function(s) | Implemented methods | Notes |
|---|---|---|---|
| Mean of a univariate process | `cchart.Xbar()`, `cchart.Xbar1()`, `cchart.Xbar2()`, `cchart.Xbar_R()` | Shewhart-type X-bar charts | Includes X-bar/R workflows. |
| Range / process dispersion | `cchart.R()` | Shewhart R chart; exact Tukey-based R chart | Exact limits use the relative range distribution. |
| Standard deviation | `cchart.S()` | Normalized S chart; exact chi-square-based S chart | Exact limits use the sample-variance distribution. |
| Nonconforming proportion | `pchart_limits()`, `pchart_alpha_risk()`, `cchart.p()` | Normal, CF1, CF2, and standardized p charts | Includes exact binomial false-alarm evaluation and pooled estimation. |
| Double-sampling np chart | `dsnp_prob_accept()`, `dsnp_arl()`, `dsnp_ass()`, `dsnp_limits()`, `cchart.DSnp()` | Exact-binomial DS-np performance, limit search, and chart | Two-stage sampling for high-quality processes with small samples. |
| Nonconformities per unit | `uchart_limits()`, `uchart_alpha_risk()`, `cchart.u()` | Normal, CF1, CF2, and standardized u charts | Includes exact Poisson risk and pooled rate estimation. |
| Multivariate mean vector | `T2.1()`, `T2.2()`, `cchart.T2.1()`, `cchart.T2.2()` | Hotelling T² charts for Phase I and Phase II | Supports individual and subgroup observations. |
| Multivariate variability | `gv_stat()`, `gv_limits()`, `gv_alpha_risk()`, `cchart.GV()` | Normal, Cornish-Fisher, selected exact, and simulation-based generalized variance charts | Exact dimension-two limits and selected published dimension-three quantiles. |
| Multivariate variability structure | `trv_stat()`, `trv_limits()`, `trv_alpha_risk()`, `cchart.trV()` | Exact chi-square and simulation-based trace-statistic charts | Complements `|S|` by detecting standardized trace changes that may preserve determinant. |
| Relative range constants | `d2()`, `d3()` | Numerical integration using Tukey distribution functions | Used by exact R-chart calculations. |
| False-alarm risk for R charts | `alpha.risk()` | Exact false-alarm probability for the classical three-sigma R chart | Diagnoses inflated false-alarm risk. |

## Installation

Install the CRAN version:

```r
install.packages("IQCC", dependencies = TRUE)
```

Install the development version from GitHub:

```r
remotes::install_github("flaviobarros/IQCC")
```

## Quick start

```r
library(IQCC)

# X-bar and R charts
data(pistonrings)
cchart.Xbar_R(pistonrings[1:25, ], 5)

# Exact R chart using Phase I data to estimate sigma
cchart.R(
  pistonrings[26:40, ],
  5,
  type = "tukey",
  y = pistonrings[1:25, ]
)

# p-chart limits and exact false-alarm risk
p_limits <- pchart_limits(p = 0.015, n = 20, type = "cf2")
pchart_alpha_risk(
  p = 0.015,
  n = 20,
  lcl = p_limits$lcl,
  ucl = p_limits$ucl
)

# u-chart with pooled Phase I rate and CF2 limits
data(moonroof)
cchart.u(
  x1 = moonroof$yi[1:17],
  n1 = moonroof$ni[1:17],
  type = "cf2",
  x2 = moonroof$yi[18:34],
  n2 = moonroof$ni[18:34]
)

# DS-np performance for a published high-quality-process design
dsnp_arl(
  p = c(0.005, 0.0075),
  n1 = 34,
  n2 = 162,
  wl = 1.5,
  ucl1 = 2.5,
  ucl2 = 4.5
)

# Generalized variance limits for dimension two
gv_limits(
  n = 10,
  p = 2,
  det_sigma = 0.5320,
  type = "exact"
)

# Auxiliary trace chart for covariance-structure changes
set.seed(123)
phase1 <- array(rnorm(6 * 8 * 2), dim = c(6, 8, 2))
cchart.trV(phase1, Sigma0 = diag(2), plot = FALSE)
```

## Learning more

The package includes three vignettes:

```r
vignette("iqcc-positioning", package = "IQCC")
vignette("high-quality-processes", package = "IQCC")
vignette("statistical-foundations", package = "IQCC")
```

- `iqcc-positioning` explains where IQCC fits in the R/SPC ecosystem.
- `high-quality-processes` focuses on rare nonconformities, Cornish-Fisher p charts, and DS-np monitoring.
- `statistical-foundations` records the probability models, derivations, and validation strategy behind the audited methods.

A longer article-oriented technical document is available at `paper/statistical-foundations.md`.

## Research background

IQCC was developed from research on improved statistical quality control charts, especially work associated with Emanuel Pimentel Barbosa and collaborators. The package emphasizes cases where classical Shewhart-type limits are simple and familiar but statistically inaccurate.

Important methodological themes include:

- Cornish-Fisher quantile correction for highly skewed attribute statistics;
- exact discrete false-alarm evaluation for binomial and Poisson charts;
- exact range-chart limits through the relative range distribution;
- double-sampling designs for rare nonconformities;
- Hotelling T² monitoring for multivariate process means;
- generalized variance monitoring through products of chi-square variables and Bartlett decomposition;
- auxiliary `tr(V)` monitoring through the trace of a standardized Wishart matrix.

## Development roadmap

| Candidate extension | Statistical target | Possible function names | Status |
|---|---|---|---|
| Double-sampling np chart | Nonconforming proportion in high-quality processes | `dsnp_limits()`, `cchart.DSnp()` | Implemented and validated |
| Generalized variance chart | Multivariate process variability using `|S|` | `gv_limits()`, `cchart.GV()` | Implemented and validated |
| Cornish-Fisher generalized variance limits | Corrected limits for skewed `|S|` distribution | `gv_limits(type = "cf")` | Implemented |
| Auxiliary trace chart | Complementary monitoring using `tr(V)` | `trv_limits()`, `cchart.trV()` | Implemented |
| Full DS-np sample-size optimization | Joint design over sample sizes and limits | future API | Planned |
| Generic exact generalized variance quantiles | Meijer-G or another validated numerical approach | future API | Research stage |
| Numerical validation catalogue | Centralized published fixtures and metadata | `tests/testthat/` and documentation | In progress |

## References

- Montgomery, D. C. (2008). *Introduction to Statistical Quality Control*. 6th ed. Wiley.
- Joekes, S. and Barbosa, E. P. (2013). An improved attribute control chart for monitoring non-conforming proportion in high quality processes. *Control Engineering Practice*.
- Barbosa, E. P., Gneri, M. A. and Meneguetti, A. (2013). Range Control Charts Revisited: Simpler Tippett-like Formulae, Its Practical Implementation, and the Study of False Alarm. *Communications in Statistics - Simulation and Computation*.
- Joekes, S., Smrekar, M. and Barbosa, E. P. (2015). Extending a double sampling control chart for non-conforming proportion in high quality processes to the case of small samples. *Statistical Methodology*.
