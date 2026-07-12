#' Probability Limits for p Charts
#'
#' Compute normal or Cornish-Fisher probability limits for a p chart without
#' constructing a plot. The Cornish-Fisher formulas retain the original
#' proportion scale and correct the normal approximation using binomial
#' cumulants.
#'
#' \code{type = "cf1"} uses the first skewness correction proposed by
#' Winterbottom (1993). \code{type = "cf2"} adds the fourth-cumulant and
#' squared-skewness terms used by Joekes and Barbosa (2013).
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
#'   \code{n}, \code{alpha}, \code{type}, and \code{sides}.
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

    cf_quantile <- function(z)
    {
        value <- p + z * sd_p

        if(type %in% c("cf1", "cf2"))
        {
            value <- value + ((z^2 - 1) / (6 * n)) * (1 - 2 * p)
        }

        if(type == "cf2")
        {
            value <- value +
                ((z^3 - 3 * z) / (24 * n^2)) *
                (1 - 6 * p * (1 - p)) / sd_p -
                ((2 * z^3 - 5 * z) / (36 * n^2)) *
                (1 - 2 * p)^2 / sd_p
        }

        value
    }

    if(sides == "two.sided")
    {
        z_lower <- qnorm(alpha / 2)
        z_upper <- qnorm(1 - alpha / 2)
        lcl <- cf_quantile(z_lower)
    }
    else
    {
        z_upper <- qnorm(1 - alpha)
        lcl <- rep(0, length(n))
    }

    ucl <- cf_quantile(z_upper)

    if(truncate)
    {
        lcl <- pmax(0, pmin(1, lcl))
        ucl <- pmax(0, pmin(1, ucl))
    }

    list(
        center = p,
        lcl = lcl,
        ucl = ucl,
        n = n,
        alpha = alpha,
        type = type,
        sides = sides
    )
}
