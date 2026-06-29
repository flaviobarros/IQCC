# IQCC

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/IQCC)](https://cran.r-project.org/package=IQCC)
[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![packageversion](https://img.shields.io/badge/Package%20version-0.7-orange.svg?style=flat-square)](commits/master)
[![DOI](https://zenodo.org/badge/18469916.svg)](https://zenodo.org/badge/latestdoi/18469916)
[![](https://cranlogs.r-pkg.org/badges/IQCC)](https://cran.r-project.org/package=IQCC)

Builds statistical control charts with exact limits for univariate and
multivariate cases.

## Features

- **Univariate charts**: X-bar, R, S, p, u charts with Shewhart, Cornish-Fisher, and standardized variants
- **Multivariate charts**: Hotelling T² control charts for Phase I and Phase II
- **Exact limits**: Tukey-based exact R chart limits, chi-square-based S chart limits
- **Phase I & II**: Full support for both retrospective (Phase I) and monitoring (Phase II) analysis

## Installation

You can install from CRAN:

```r
install.packages('IQCC', dependencies = TRUE)
```

or from GitHub:

```r
devtools::install_github('flaviobarros/IQCC')
```

## Quick Start

```r
library(IQCC)

# X-bar and R charts
data(pistonrings)
cchart.Xbar_R(pistonrings[1:25, ], 5)

# Hotelling T² chart (Phase I)
mu <- c(5.682, 88.22)
Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
datum <- data.1(20, 10, mu, Sigma)
estat <- stats(datum, 20, 10, 2)
T2 <- T2.1(estat, 20, 10)
cchart.T2.1(T2, 20, 10, 2)
```

## References

- Montgomery, D.C. (2008). *Introduction to Statistical Quality Control*. 6th ed. Wiley.
- Barros, F. et al. (2017). IQCC: An R Package for Improved Quality Control Charts. *Journal of Statistical Software*.
