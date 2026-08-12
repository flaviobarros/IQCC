#' Exact Probability Limits for the S Chart
#'
#' Compute exact probability control limits for subgroup standard deviations
#' without constructing a plot. Under normal sampling,
#' \deqn{(n-1)S^2/\sigma^2 \sim \chi^2_{n-1}.}
#' For a two-sided chart, the nominal false-alarm probability is divided equally
#' between the two tails. For an upper chart, all of \code{alpha} is assigned to
#' the upper tail and the lower limit is zero.
#'
#' @param sigma Positive finite scalar. The in-control process standard
#'   deviation. When estimated from Phase I data, the returned limits are
#'   plug-in limits and do not incorporate Phase I estimation uncertainty.
#' @param n Integer subgroup size(s), each at least 2.
#' @param alpha Nominal false-alarm probability per subgroup, strictly between
#'   0 and 1. Defaults to 0.0027.
#' @param side Either \code{"two.sided"} or \code{"upper"}.
#'
#' @return A named list with components \code{lcl}, \code{ucl}, \code{center},
#'   \code{sigma}, \code{n}, \code{alpha}, \code{side}, and \code{method}.
#'   The theoretical center is \eqn{c_4(n)\sigma}.
#'
#' @section Phase convention:
#' These functions take \code{sigma} as known. If a Phase I estimate is
#' supplied instead, the resulting limits are plug-in limits. The historical
#' \code{cchart.S()} wrapper estimates \code{sigma} from the data using
#' \code{qcc::sd.S()} before calling this function for exact limits.
#'
#' @section Decision rule:
#' A subgroup standard deviation signals out of control when it is below
#' \code{lcl} or above \code{ucl}. Equality to a limit is treated as in control.
#'
#' @references
#' Montgomery, D. C. (2009). \emph{Introduction to Statistical Quality
#' Control}, 6th ed. Wiley.
#'
#' @seealso \code{\link{s_shewhart_limits}}, \code{\link{cchart.S}},
#'   \code{\link{c4}}
#' @export
#' @importFrom stats qchisq
#' @examples
#' s_exact_limits(sigma = 2, n = 5)
#' s_exact_limits(sigma = 1, n = 2:6, side = "upper")
#'
s_exact_limits <- function(sigma, n, alpha = ALPHA,
                           side = c("two.sided", "upper"))
{
    side <- match.arg(side)
    .s_validate_sigma(sigma)
    n <- .s_validate_n(n)
    .s_validate_alpha(alpha)

    lower_prob <- if(side == "upper") 0 else alpha / 2
    upper_prob <- if(side == "upper") 1 - alpha else 1 - alpha / 2

    lcl <- if(side == "upper") rep(0, length(n)) else
        sigma * sqrt(stats::qchisq(lower_prob, n - 1) / (n - 1))
    ucl <- sigma * sqrt(stats::qchisq(upper_prob, n - 1) / (n - 1))
    center <- c4(n) * sigma

    list(lcl = unname(lcl), ucl = unname(ucl), center = unname(center),
         sigma = sigma, n = n, alpha = alpha, side = side, method = "exact")
}

#' Conventional Shewhart Limits for the S Chart
#'
#' Compute conventional Shewhart limits for subgroup standard deviations
#' without constructing a plot. Under normality,
#' \eqn{E(S)=c_4(n)\sigma} and
#' \eqn{SD(S)=\sigma\sqrt{1-c_4(n)^2}}. With the default theoretical center,
#' the limits are therefore
#' \deqn{LCL=\max\{0,c_4(n)\sigma-k\sigma\sqrt{1-c_4(n)^2}\}}
#' and
#' \deqn{UCL=c_4(n)\sigma+k\sigma\sqrt{1-c_4(n)^2},}
#' where \eqn{k} is \code{nsigmas}.
#'
#' The optional \code{center} argument exists to reproduce the historical
#' \code{qcc} S-chart convention, which centers the limits on the observed
#' weighted mean of subgroup standard deviations while estimating \code{sigma}
#' separately. When \code{center = NULL}, the theoretical center
#' \eqn{c_4(n)\sigma} is used.
#'
#' @param sigma Positive finite scalar. The in-control process standard
#'   deviation, known or estimated separately.
#' @param n Integer subgroup size(s), each at least 2.
#' @param nsigmas Positive finite scalar giving the number of standard-error
#'   units used for the limits. Defaults to 3.
#' @param side Either \code{"two.sided"} or \code{"upper"}. For an upper chart
#'   the lower limit is zero.
#' @param center Optional finite numeric center. It may be a scalar or have the
#'   same length as \code{n}. If omitted, \eqn{c_4(n)\sigma} is used.
#'
#' @return A named list with components \code{lcl}, \code{ucl}, \code{center},
#'   \code{sigma}, \code{n}, \code{nsigmas}, \code{side}, and \code{method}.
#'
#' @section Legacy qcc convention:
#' The current \code{qcc::limits.S()} implementation computes
#' \code{std.dev * sqrt(1 - c4(sizes)^2)} as the standard error of the plotted
#' S statistic and centers conventional limits on the chart center supplied by
#' \code{qcc}. Consequently, \code{cchart.S(type = "n")} passes its Phase I
#' weighted S center explicitly to this function. This preserves the historical
#' IQCC/qcc numerical behavior rather than silently replacing the sample center
#' by \eqn{c_4(n)\hat\sigma}.
#'
#' @references
#' Montgomery, D. C. (2009). \emph{Introduction to Statistical Quality
#' Control}, 6th ed. Wiley.
#'
#' Scrucca, L. \emph{qcc: Quality Control Charts}. R package.
#'
#' @seealso \code{\link{s_exact_limits}}, \code{\link{cchart.S}},
#'   \code{\link{c4}}
#' @export
#' @examples
#' s_shewhart_limits(sigma = 2, n = 5)
#' s_shewhart_limits(sigma = 2, n = 5, center = 1.9)
#'
s_shewhart_limits <- function(sigma, n, nsigmas = SIGMA_MULT,
                               side = c("two.sided", "upper"), center = NULL)
{
    side <- match.arg(side)
    .s_validate_sigma(sigma)
    n <- .s_validate_n(n)
    if(!is.numeric(nsigmas) || length(nsigmas) != 1 ||
       !is.finite(nsigmas) || nsigmas <= 0)
        stop("nsigmas must be a finite positive scalar")

    theoretical_center <- c4(n) * sigma
    if(is.null(center))
    {
        center <- theoretical_center
    }
    else
    {
        if(!is.numeric(center) || length(center) < 1 ||
           length(center) > 1 && length(center) != length(n) ||
           any(!is.finite(center)))
            stop("center must be a finite numeric scalar or have the same length as n")
        if(length(center) == 1 && length(n) > 1)
            center <- rep(center, length(n))
    }

    se_s <- sigma * sqrt(pmax(0, 1 - c4(n)^2))
    lcl <- if(side == "upper") rep(0, length(n)) else
        pmax(0, center - nsigmas * se_s)
    ucl <- center + nsigmas * se_s

    list(lcl = unname(lcl), ucl = unname(ucl), center = unname(center),
         sigma = sigma, n = n, nsigmas = nsigmas, side = side,
         method = "shewhart")
}

.s_validate_sigma <- function(sigma)
{
    if(!is.numeric(sigma) || length(sigma) != 1 || !is.finite(sigma) ||
       sigma <= 0)
        stop("sigma must be a finite positive scalar")
    invisible(sigma)
}

.s_validate_n <- function(n)
{
    if(!is.numeric(n) || length(n) < 1 || any(!is.finite(n)) ||
       any(n < 2) || any(n != floor(n)))
        stop("n must contain integers greater than or equal to 2")
    as.integer(n)
}

.s_validate_alpha <- function(alpha)
{
    if(!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) ||
       alpha <= 0 || alpha >= 1)
        stop("alpha must be a finite scalar between 0 and 1")
    invisible(alpha)
}
