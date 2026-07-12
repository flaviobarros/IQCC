#' Exact False-Alarm Risk for p-Chart Limits
#'
#' Compute the actual false-alarm probability of p-chart limits under the
#' in-control binomial model. The calculation respects the discreteness of
#' \eqn{X \sim Binomial(n, p)} and treats a signal as
#' \eqn{X/n < LCL} or \eqn{X/n > UCL}.
#'
#' @param p In-control nonconforming proportion.
#' @param n Positive integer sample size or vector of sample sizes.
#' @param lcl Lower control limit or vector of limits.
#' @param ucl Upper control limit or vector of limits.
#'
#' @return A numeric vector with the exact false-alarm probability for each
#'   sample size and pair of limits.
#'
#' @export
#' @importFrom stats pbinom
#' @examples
#' limits <- pchart_limits(0.015, 20, type = "cf2")
#' pchart_alpha_risk(0.015, 20, limits$lcl, limits$ucl)
#'
pchart_alpha_risk <- function(p, n, lcl, ucl)
{
    if(!is.numeric(p) || length(p) != 1 || !is.finite(p) || p <= 0 || p >= 1)
        stop("p must be a finite scalar strictly between 0 and 1")
    if(!is.numeric(n) || length(n) < 1 || any(!is.finite(n)) ||
       any(n < 1) || any(n != floor(n)))
        stop("n must contain positive integers")
    if(!is.numeric(lcl) || !is.numeric(ucl) ||
       any(!is.finite(lcl)) || any(!is.finite(ucl)))
        stop("lcl and ucl must contain finite numeric values")

    target_length <- max(length(n), length(lcl), length(ucl))
    if(any(!c(length(n), length(lcl), length(ucl)) %in% c(1, target_length)))
        stop("n, lcl, and ucl must have compatible lengths")

    n <- rep(n, length.out = target_length)
    lcl <- rep(lcl, length.out = target_length)
    ucl <- rep(ucl, length.out = target_length)

    if(any(lcl < 0 | lcl > 1) || any(ucl < 0 | ucl > 1) || any(lcl > ucl))
        stop("limits must satisfy 0 <= lcl <= ucl <= 1")

    lower_count <- ceiling(n * lcl) - 1
    upper_count <- floor(n * ucl)

    lower_risk <- ifelse(
        lcl <= 0,
        0,
        pbinom(lower_count, size = n, prob = p)
    )
    upper_risk <- ifelse(
        ucl >= 1,
        0,
        1 - pbinom(upper_count, size = n, prob = p)
    )

    lower_risk + upper_risk
}
