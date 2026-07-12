#' IQCC: Improved Quality Control Charts
#'
#' IQCC implements statistical process control charts with exact,
#' Cornish-Fisher corrected, standardized, or simulation-based probability
#' limits. The package covers univariate variable and attribute charts,
#' double-sampling np charts for rare nonconformities, Hotelling T-squared
#' charts, and generalized variance charts for multivariate variability.
#'
#' Numerical functions for limits, false-alarm risk, average run length, and
#' average sample size are exposed separately from plotting functions so that
#' calculations can be inspected, tested, and reused in simulation studies.
#'
#' @name IQCC-package
#' @aliases IQCC
#' @docType package
#' @seealso
#' \code{\link{pchart_limits}}, \code{\link{uchart_limits}},
#' \code{\link{dsnp_limits}}, \code{\link{gv_limits}},
#' \code{\link{cchart.R}}, \code{\link{cchart.DSnp}},
#' \code{\link{cchart.GV}}
"_PACKAGE"

# Alpha risk for 3-sigma control limits
ALPHA <- 0.0027

# Sigma multiplier for Shewhart control limits
SIGMA_MULT <- 3

# Quantile probabilities for control limits (1 - ALPHA/2 and ALPHA/2)
Q_UPPER <- 1 - ALPHA / 2   # 0.99865
Q_LOWER <- ALPHA / 2       # 0.00135
