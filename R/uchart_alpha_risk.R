#' Exact False-Alarm Risk for u-Chart Limits
#'
#' Compute the actual false-alarm probability under
#' \eqn{X \sim Poisson(lambda n)}, with a signal when
#' \eqn{X/n < LCL} or \eqn{X/n > UCL}.
#'
#' @param lambda In-control defect rate per inspection unit.
#' @param n Positive inspection size or vector of inspection sizes.
#' @param lcl Lower control limit or vector of limits.
#' @param ucl Upper control limit or vector of limits.
#'
#' @return A numeric vector of exact false-alarm probabilities.
#' @export
#' @importFrom stats ppois
#' @examples
#' limits <- uchart_limits(1.4, 10, type = "cf2")
#' uchart_alpha_risk(1.4, 10, limits$lcl, limits$ucl)
#'
uchart_alpha_risk <- function(lambda, n, lcl, ucl)
{
    if(!is.numeric(lambda) || length(lambda) != 1 || !is.finite(lambda) ||
       lambda <= 0)
        stop("lambda must be a finite positive scalar")
    if(!is.numeric(n) || length(n) < 1 || any(!is.finite(n)) || any(n <= 0))
        stop("n must contain finite positive values")
    if(!is.numeric(lcl) || !is.numeric(ucl) || any(!is.finite(lcl)) ||
       any(!is.finite(ucl)))
        stop("lcl and ucl must contain finite numeric values")

    target_length <- max(length(n), length(lcl), length(ucl))
    if(any(!c(length(n), length(lcl), length(ucl)) %in% c(1, target_length)))
        stop("n, lcl, and ucl must have compatible lengths")

    n <- rep(n, length.out = target_length)
    lcl <- rep(lcl, length.out = target_length)
    ucl <- rep(ucl, length.out = target_length)

    if(any(lcl < 0) || any(lcl > ucl))
        stop("limits must satisfy 0 <= lcl <= ucl")

    lower_count <- ceiling(n * lcl) - 1
    upper_count <- floor(n * ucl)
    mu <- lambda * n

    lower_risk <- ifelse(lcl <= 0, 0, ppois(lower_count, lambda = mu))
    upper_risk <- 1 - ppois(upper_count, lambda = mu)

    lower_risk + upper_risk
}
