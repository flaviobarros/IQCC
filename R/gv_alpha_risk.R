#' False-Alarm Risk for Generalized Variance Charts
#'
#' Evaluate the actual in-control probability that the generalized variance
#' statistic crosses limits produced by \code{gv_limits()}, and report the
#' corresponding zero-state in-control average run length.
#'
#' The diagnostic is useful because Gaussian and Cornish-Fisher limits are
#' approximations and therefore need not attain the requested nominal risk
#' exactly. For dimension \eqn{p=2}, the crossing probability is evaluated
#' from the exact chi-square representation of \eqn{|S|}. For \eqn{p>2}, it is
#' estimated independently by Monte Carlo simulation from Bartlett's
#' product-of-chi-squares representation.
#'
#' @inheritParams gv_limits
#'
#' @details
#' The function first calls \code{gv_limits()} using the requested
#' \code{type}. It then evaluates
#' \deqn{P(|S| > UCL)}
#' for an upper chart, or
#' \deqn{P(|S| < LCL) + P(|S| > UCL)}
#' for a two-sided chart. The inequalities are strict, matching the signaling
#' rule used by \code{cchart.GV()}.
#'
#' For \eqn{p>2}, \code{nsim} controls both any simulation used to construct
#' the limits and the independent simulation used to evaluate their risk. If
#' \code{seed} is supplied, the limits use that seed and the risk evaluation
#' uses \code{seed + 1}; each seeded simulation restores the caller's previous
#' random-number state on exit. With \code{seed = NULL}, the global RNG state
#' advances normally.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{alpha}}{Actual or Monte Carlo estimated false-alarm
#'     probability.}
#'   \item{\code{nominal_alpha}}{The requested nominal value.}
#'   \item{\code{arl0}}{Zero-state in-control average run length,
#'     \code{1 / alpha}; \code{Inf} if no simulated crossing is observed.}
#'   \item{\code{limits}}{The complete object returned by
#'     \code{gv_limits()}.}
#'   \item{\code{method}}{Either \code{"exact evaluation"} for \eqn{p=2} or
#'     \code{"simulation"} for higher dimensions.}
#' }
#'
#' @section Monte Carlo uncertainty:
#' For \eqn{p>2}, the reported risk is a binomial proportion based on
#' \code{nsim} draws. Its approximate Monte Carlo standard error is
#' \eqn{\sqrt{\hat\alpha(1-\hat\alpha)/nsim}}. Very small false-alarm risks
#' therefore require substantially more simulations than routine examples.
#'
#' @section Errors:
#' Input and exact-method restrictions are inherited from \code{gv_limits()}.
#' In addition, simulation requires an integer \code{nsim} of at least 1000
#' and a valid optional seed.
#'
#' @references
#' Barbosa, E. P., Gneri, M. A., and Meneguetti, A. \emph{Improving
#' Shewhart-type Generalized Variance Control Charts for Multivariate Process
#' Variability Monitoring using Cornish-Fisher Quantile Correction, Meijer-G
#' Function and Other Tools}. Research report.
#'
#' @seealso \code{\link{gv_limits}}, \code{\link{gv_stat}},
#' \code{\link{cchart.GV}}
#' @export
#' @examples
#' # Exact evaluation shows that exact p = 2 limits attain the nominal risk.
#' exact_risk <- gv_alpha_risk(10, 2, type = "exact")
#' exact_risk$alpha
#' exact_risk$arl0
#'
#' # Diagnose the false-alarm inflation of moment-matched normal limits.
#' gv_alpha_risk(10, 2, type = "normal")
#'
#' \donttest{
#' # Reproducible risk evaluation for a higher-dimensional chart.
#' gv_alpha_risk(8, 3, type = "cf", nsim = 10000, seed = 2026)
#' }
gv_alpha_risk <- function(n, p, det_sigma = 1, alpha = 0.0027,
                           type = c("normal", "cf", "exact", "simulation"),
                           side = c("upper", "two.sided"), cf_order = 1,
                           nsim = 200000, seed = NULL)
{
    type <- match.arg(type)
    side <- match.arg(side)
    limits <- gv_limits(n, p, det_sigma, alpha, type, side,
                        cf_order, nsim, seed)

    if(p == 2)
    {
        df <- 2 * n - 4
        cdf <- function(x)
        {
            if(x <= 0)
                return(0)
            stats::pchisq(2 * (n - 1) * sqrt(x / det_sigma), df)
        }
        actual <- if(side == "upper")
            1 - cdf(limits$ucl)
        else
            cdf(limits$lcl) + 1 - cdf(limits$ucl)
    }
    else
    {
        sim <- det_sigma * .gv_simulate_multiplier(n, p, nsim,
                                                   if(is.null(seed)) NULL else seed + 1)
        actual <- if(side == "upper")
            mean(sim > limits$ucl)
        else
            mean(sim < limits$lcl | sim > limits$ucl)
    }

    list(alpha = actual, nominal_alpha = alpha,
         arl0 = if(actual == 0) Inf else 1 / actual,
         limits = limits, method = if(p == 2) "exact evaluation" else "simulation")
}
