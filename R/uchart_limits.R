#' Probability Limits for u Charts
#'
#' Compute normal or Cornish-Fisher probability limits for a u chart without
#' constructing a plot. The model is \eqn{X \sim Poisson(lambda n)} and the
#' monitored rate is \eqn{U = X/n}.
#'
#' For \eqn{U}, the standardized third and fourth cumulants are
#' \eqn{gamma_1 = 1/sqrt(lambda n)} and \eqn{gamma_2 = 1/(lambda n)}.
#' Consequently, the first Cornish-Fisher correction is
#' \eqn{(z^2 - 1)/(6n)} and the second-order correction reduces to
#' \eqn{z(1-z^2)/(72 n sqrt(lambda n))}.
#'
#' With \eqn{z=3}, the CF2 upper limit recovers the historical IQCC formula
#' \deqn{lambda + 3 sqrt(lambda/n) + 4/(3n) -
#'       1/(3 n sqrt(lambda n)).}
#'
#' For two-sided CF2 limits, the operational convention used here evaluates
#' the second adjustment with the positive upper-tail quantile and applies it
#' with the same sign to both limits. This is consistent with the historical
#' IQCC formula and with the operational convention used for the corrected
#' p chart.
#'
#' @param lambda In-control defect rate per inspection unit. A positive scalar.
#' @param n Positive inspection size or vector of inspection sizes.
#' @param alpha Nominal false-alarm probability. Defaults to 0.0027.
#' @param type Limit method: \code{"normal"}, \code{"cf1"}, or \code{"cf2"}.
#' @param sides Either \code{"two.sided"} or \code{"upper"}.
#' @param truncate Logical. If \code{TRUE}, negative lower limits are set to 0.
#'
#' @return A list containing \code{center}, \code{lcl}, \code{ucl},
#'   \code{n}, \code{alpha}, \code{type}, \code{sides}, and \code{mean_count}.
#'
#' @export
#' @importFrom stats qnorm
#' @examples
#' uchart_limits(1.4, 10, type = "normal")
#' uchart_limits(1.4, 10, type = "cf1")
#' uchart_limits(1.4, 10, type = "cf2")
#'
uchart_limits <- function(lambda, n, alpha = ALPHA,
                          type = c("normal", "cf1", "cf2"),
                          sides = c("two.sided", "upper"),
                          truncate = TRUE)
{
    type <- match.arg(type)
    sides <- match.arg(sides)

    if(!is.numeric(lambda) || length(lambda) != 1 || !is.finite(lambda) ||
       lambda <= 0)
        stop("lambda must be a finite positive scalar")
    if(!is.numeric(n) || length(n) < 1 || any(!is.finite(n)) || any(n <= 0))
        stop("n must contain finite positive values")
    if(!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) ||
       alpha <= 0 || alpha >= 1)
        stop("alpha must be a finite scalar between 0 and 1")
    if(!is.logical(truncate) || length(truncate) != 1 || is.na(truncate))
        stop("truncate must be TRUE or FALSE")

    sd_u <- sqrt(lambda / n)

    if(sides == "two.sided")
    {
        z_lower <- qnorm(alpha / 2)
        z_upper <- qnorm(1 - alpha / 2)
    }
    else
    {
        z_lower <- NA_real_
        z_upper <- qnorm(1 - alpha)
    }

    cf1_quantile <- function(z)
        lambda + z * sd_u + (z^2 - 1) / (6 * n)

    cf2_adjustment <- function(z_positive)
        z_positive * (1 - z_positive^2) /
            (72 * n * sqrt(lambda * n))

    if(type == "normal")
    {
        ucl <- lambda + z_upper * sd_u
        lcl <- if(sides == "two.sided")
            lambda + z_lower * sd_u
        else
            rep(0, length(n))
    }
    else if(type == "cf1")
    {
        ucl <- cf1_quantile(z_upper)
        lcl <- if(sides == "two.sided")
            cf1_quantile(z_lower)
        else
            rep(0, length(n))
    }
    else
    {
        second_adjustment <- cf2_adjustment(z_upper)
        ucl <- cf1_quantile(z_upper) + second_adjustment
        lcl <- if(sides == "two.sided")
            cf1_quantile(z_lower) + second_adjustment
        else
            rep(0, length(n))
    }

    if(truncate)
        lcl <- pmax(0, lcl)

    if(any(!is.finite(lcl)) || any(!is.finite(ucl)) || any(lcl > ucl))
        stop("the requested method produced invalid limits")

    list(
        center = lambda,
        lcl = lcl,
        ucl = ucl,
        n = n,
        alpha = alpha,
        type = type,
        sides = sides,
        mean_count = lambda * n
    )
}
