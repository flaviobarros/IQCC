#' Generalized Variance Control Limits
#'
#' Compute one-sided upper or equal-tail two-sided control limits for the
#' generalized variance statistic \eqn{|S|} under independent sampling from a
#' \eqn{p}-variate normal process with in-control covariance matrix \eqn{\Sigma}.
#'
#' If \eqn{S} is the usual covariance matrix from a subgroup of size \eqn{n},
#' then \eqn{(n-1)S} has a Wishart distribution and \eqn{|S|} can be
#' represented as a scaled product of independent chi-square variables. The
#' function uses that representation to obtain moments, approximations, exact
#' cases, or simulated quantiles.
#'
#' @param n Integer subgroup sample size. It must satisfy \eqn{n > p}.
#' @param p Integer process dimension, at least 2.
#' @param det_sigma Positive finite scalar equal to \eqn{|\Sigma|}, the
#' determinant of the in-control covariance matrix. Control limits scale
#' linearly with this value.
#' @param alpha Nominal probability of a false alarm per subgroup, strictly
#' between 0 and 1. For \code{side = "upper"}, all probability is placed in
#' the upper tail. For \code{side = "two.sided"}, \code{alpha / 2} is placed
#' in each tail.
#' @param type Character string selecting the limit calculation:
#' \describe{
#'   \item{\code{"normal"}}{Moment-matched Gaussian quantiles using the exact
#'     mean and variance of \eqn{|S|}.}
#'   \item{\code{"cf"}}{Cornish-Fisher corrected Gaussian quantiles using
#'     skewness, and optionally excess kurtosis.}
#'   \item{\code{"exact"}}{Closed-form chi-square quantiles for \eqn{p=2}, or
#'     the published upper-limit table for \eqn{p=3}, \eqn{n=4,\ldots,15}, and
#'     \code{alpha} equal to 0.0020 or 0.0027.}
#'   \item{\code{"simulation"}}{Monte Carlo quantiles from the Bartlett
#'     product-of-chi-squares representation.}
#' }
#' Partial matching is not used.
#' @param side Either \code{"upper"} or \code{"two.sided"}. Since generalized
#' variance is nonnegative, the lower limit for an upper chart is zero. Any
#' negative approximated lower limit for a two-sided chart is also truncated
#' to zero.
#' @param cf_order Integer 1 or 2. Order 1 uses the skewness correction. Order
#' 2 additionally uses excess kurtosis and the squared-skewness term. It is
#' used only when \code{type = "cf"}.
#' @param nsim Integer number of Monte Carlo draws, at least 1000. It is used
#' by \code{type = "simulation"} and stored in the returned object for all
#' methods.
#' @param seed \code{NULL} or a finite numeric scalar used as the Monte Carlo
#' seed. A supplied seed makes simulated limits reproducible and the caller's
#' existing \code{.Random.seed} is restored on exit.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{lcl}, \code{ucl}}{Lower and upper control limits.}
#'   \item{\code{center}}{The exact in-control mean of \eqn{|S|}; this is the
#'     center line used by \code{cchart.GV()}.}
#'   \item{\code{type}, \code{side}, \code{alpha}}{The selected method, chart
#'     sidedness, and nominal false-alarm probability.}
#'   \item{\code{n}, \code{p}, \code{det_sigma}}{The validated design
#'     parameters.}
#'   \item{\code{moments}}{A list containing the first four ordinary moments,
#'     mean, variance, standard deviation, skewness, excess kurtosis, the
#'     constants \code{b1} and \code{b2}, and the design parameters.}
#'   \item{\code{cf_order}, \code{nsim}, \code{seed}}{The requested numerical
#'     settings.}
#' }
#'
#' @section Exact-method scope:
#' The \eqn{p=2} exact distribution is available for both upper and two-sided
#' charts. The \eqn{p=3} implementation is deliberately limited to the
#' published one-sided upper quantiles described above. No generic Meijer-G
#' evaluator is claimed; use \code{type = "simulation"} outside those cases.
#'
#' @section Decision convention:
#' A plotted subgroup signals when \eqn{|S| < LCL} or \eqn{|S| > UCL}.
#' Equality to either limit is treated as in control.
#'
#' @section Errors:
#' Errors are raised for invalid dimensions, \eqn{n <= p}, a nonpositive
#' \code{det_sigma}, invalid \code{alpha}, unsupported exact cases, invalid
#' Cornish-Fisher order, fewer than 1000 simulations, or an invalid seed.
#'
#' @references
#' Barbosa, E. P., Gneri, M. A., and Meneguetti, A. \emph{Improving
#' Shewhart-type Generalized Variance Control Charts for Multivariate Process
#' Variability Monitoring using Cornish-Fisher Quantile Correction, Meijer-G
#' Function and Other Tools}. Research report.
#'
#' Anderson, T. W. (1984). \emph{An Introduction to Multivariate Statistical
#' Analysis}, 2nd ed. Wiley.
#'
#' Cornish, E. A. and Fisher, R. A. (1960). The percentage points of
#' distributions having known cumulants. \emph{Technometrics}, 2, 209--225.
#'
#' @seealso \code{\link{gv_stat}}, \code{\link{gv_alpha_risk}},
#' \code{\link{cchart.GV}}
#' @export
#' @examples
#' # Published dimension-two illustration: |Sigma| = 0.5320, n = 10.
#' gv_limits(10, 2, det_sigma = 0.5320, type = "normal")$ucl
#' gv_limits(10, 2, det_sigma = 0.5320, type = "cf")$ucl
#' gv_limits(10, 2, det_sigma = 0.5320, type = "exact")$ucl
#'
#' # Equal-tail exact limits are also available in dimension two.
#' gv_limits(10, 2, side = "two.sided", type = "exact")
#'
#' \donttest{
#' # A reproducible simulation-based limit for higher dimension.
#' gv_limits(8, 3, type = "simulation", nsim = 5000, seed = 2026)
#' }
gv_limits <- function(n, p, det_sigma = 1, alpha = 0.0027,
                       type = c("normal", "cf", "exact", "simulation"),
                       side = c("upper", "two.sided"), cf_order = 1,
                       nsim = 100000, seed = NULL)
{
    type <- match.arg(type)
    side <- match.arg(side)
    p <- .gv_validate_dimension(p)
    n <- .gv_validate_sample_size(n, p)
    det_sigma <- .gv_validate_det_sigma(det_sigma)
    alpha <- .gv_validate_alpha(alpha)
    moments <- .gv_moments(n, p, det_sigma)

    probs <- if(side == "upper") c(NA_real_, 1 - alpha) else c(alpha / 2, 1 - alpha / 2)

    if(type == "normal")
    {
        lower <- if(side == "upper") 0 else moments$mean + stats::qnorm(probs[1]) * moments$sd
        upper <- moments$mean + stats::qnorm(probs[2]) * moments$sd
    }
    else if(type == "cf")
    {
        q_upper <- .gv_cf_standard_quantile(probs[2], moments, cf_order)
        upper <- moments$mean + q_upper * moments$sd
        lower <- if(side == "upper") 0 else
            moments$mean + .gv_cf_standard_quantile(probs[1], moments, cf_order) * moments$sd
    }
    else if(type == "exact" && p == 2)
    {
        df <- 2 * n - 4
        qfun <- function(prob) stats::qchisq(prob, df)^2 / (4 * (n - 1)^2)
        lower <- if(side == "upper") 0 else det_sigma * qfun(probs[1])
        upper <- det_sigma * qfun(probs[2])
    }
    else if(type == "exact" && p == 3)
    {
        if(side != "upper")
            stop("exact p = 3 limits are available only for an upper chart")
        lower <- 0
        upper <- det_sigma * .gv_exact_d3_upper(n, alpha)
    }
    else if(type == "exact")
    {
        stop("exact limits are implemented only for p = 2 and tabulated p = 3 cases")
    }
    else
    {
        sim <- .gv_simulate_multiplier(n, p, nsim, seed)
        lower <- if(side == "upper") 0 else det_sigma * unname(stats::quantile(sim, probs[1], type = 8))
        upper <- det_sigma * unname(stats::quantile(sim, probs[2], type = 8))
    }

    lower <- max(0, lower)
    list(lcl = lower, ucl = upper, center = moments$mean, type = type,
         side = side, alpha = alpha, n = n, p = p, det_sigma = det_sigma,
         moments = moments, cf_order = cf_order, nsim = nsim, seed = seed)
}
