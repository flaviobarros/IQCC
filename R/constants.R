#' Package constants for control chart computations.
#'
#' @name IQCC-package
#' @docType package
#' @keywords internal
"_PACKAGE"

# Alpha risk for 3-sigma control limits
ALPHA <- 0.0027

# Sigma multiplier for Shewhart control limits
SIGMA_MULT <- 3

# Quantile probabilities for control limits (1 - ALPHA/2 and ALPHA/2)
Q_UPPER <- 1 - ALPHA / 2   # 0.99865
Q_LOWER <- ALPHA / 2       # 0.00135
