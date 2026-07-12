#' Probability Limits for p Charts
#'
#' Compute normal or Cornish-Fisher probability limits for a p chart without
#' constructing a plot. The Cornish-Fisher formulas retain the original
#' proportion scale and correct the normal approximation using binomial
#' cumulants.
#'
#' \code{type = "cf1"} uses the first skewness correction proposed by
#' Winterbottom (1993). \code{type = "cf2"} uses the operational two-adjustment
#' limits in equation (8) of Joekes and Barbosa (2013). In particular, the
#' second adjustment is evaluated with the positive normal quantile and is
#' applied with the same sign to both limits. This convention reproduces the
#' published limits and false-alarm risks in Tables 2 and 3.
#'
#' The article recommends the normal approximation for approximately
#' \eqn{n p (1-p) >= 5}, CF1 for approximately \eqn{n p (1-p) >= 0.25}, and
#' CF2 for approximately \eqn{n p (1-p) >= 0.08}. The returned
#' \code{applicable} field reports whether each sample size meets the
#' corresponding recommendation.
#'
#' @param p In-control nonconforming proportion. A scalar strictly between
#'   zero and one.
#' @param n Positive integer sample size or vector of sample sizes.
#' @param alpha Nominal false alarm probability. Defaults to 0.0027.
#' @param type Limit method: \code{"normal"}, \code{"cf1"}, or \code{"cf2"}.
#' @param sides Either \code{"two.sided"} or \code{"upper"}.
#' @param truncate Logical. If \code{TRUE}, limits are restricted to [0, 1].
#'
#' @return A list containing \code{center}, \code{lcl}, \code{ucl},
#'   \code{n}, \code{alpha}, \code{type}, \code{sides}, \code{npq}, and
#'   \code{applicable}.
#'
#' @export
#' @importFrom stats qnorm
#' @references
#' Winterbottom, A. (1993). Simple adjustment to improve control limits on
#' attribute charts. \emph{Quality and Reliability Engineering International}.
#'
#' Joekes, S. and Barbosa, E. P. (2013). An improved attribute control chart
#' for monitoring non-conforming proportion in high quality processes.
#' \emph{Control Engineering Practice}, 21, 407--412.
#' \doi{10.1016/j.conengprac.2012.12.005}.
#' @examples
#' pchart_limits(0.015, n = 20, type = "normal")
#' pchart_limits(0.015, n = 20, type = "cf1")
#' pchart_limits(0.015, n = 20, type = "cf2")
#'
pchart_limits <- function(p, n, alpha = ALPHA,
                          type = c("normal", "cf1", "cf2"),
                          sides = c("two.sided", "upper"),
                          truncate = TRUE)
{
    type <- match.arg(type)
    sides <- match.arg(sides)

    if(!is.numeric(p) || length(p) != 1 || !is.finite(p) || p <= 0 || p >= 1)
        stop("p must be a finite scalar strictly between 0 and 1")
    if(!is.numeric(n) || length(n) < 1 || any(!is.finite(n)) ||
       any(n < 1) || any(n != floor(n)))
        stop("n must contain positive integers")
    if(!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) ||
       alpha <= 0 || alpha >= 1)
        stop("alpha must be a finite scalar between 0 and 1")
    if(!is.logical(truncate) || length(truncate) != 1 || is.na(truncate))
        stop("truncate must be TRUE or FALSE")

    sd_p <- sqrt(p * (1 - p) / n)

    cf1_quantile <- function(z)
    {
        p + z * sd_p + ((z^2 - 1) / (6 * n)) * (1 - 2 * p)
    }

    cf2_adjustment <- function(z_positive)
    {
        ((z_positive^3 - 3 * z_positive) / (24 * n^2)) *
            (1 - 6 * p * (1 - p)) / sd_p -
            ((2 * z_positive^3 - 5 * z_positive) / (36 * n^2)) *
            (1 - 2 * p)^2 / sd_p
    }

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

    if(type == "normal")
    {
        ucl <- p + z_upper * sd_p
        lcl <- if(sides == "two.sided") p + z_lower * sd_p else rep(0, length(n))
    }
    else if(type == "cf1")
    {
        ucl <- cf1_quantile(z_upper)
        lcl <- if(sides == "two.sided") cf1_quantile(z_lower) else rep(0, length(n))
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
    {
        lcl <- pmax(0, pmin(1, lcl))
        ucl <- pmax(0, pmin(1, ucl))
    }

    if(any(!is.finite(lcl)) || any(!is.finite(ucl)) || any(lcl > ucl))
        stop("the requested method produced invalid limits; check p, n, and method applicability")

    npq <- n * p * (1 - p)
    threshold <- switch(type, normal = 5, cf1 = 0.25, cf2 = 0.08)

    list(
        center = p,
        lcl = lcl,
        ucl = ucl,
        n = n,
        alpha = alpha,
        type = type,
        sides = sides,
        npq = npq,
        applicable = npq >= threshold
    )
}
