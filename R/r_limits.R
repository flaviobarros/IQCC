#' Exact Probability Limits for the R Chart
#'
#' Compute exact equal-tail probability control limits for the R chart without
#' constructing a plot. The limits are based on the distribution of the relative
#' range \eqn{W = R / \sigma} under normality, using the Studentized range
#' distribution implemented by \code{stats::qtukey()}.
#'
#' The exact limits are
#' \deqn{LCL = \sigma \, F_W^{-1}(\alpha/2; n)}
#' and
#' \deqn{UCL = \sigma \, F_W^{-1}(1 - \alpha/2; n),}
#' where \eqn{F_W^{-1}} is the quantile function of \eqn{W = R / \sigma}.
#'
#' @param sigma Positive finite scalar. The in-control process standard
#'   deviation. When \code{sigma} is estimated from Phase I data, the returned
#'   limits are plug-in estimates and do not incorporate the additional
#'   uncertainty of Phase I estimation.
#' @param n Integer subgroup size, at least 2.
#' @param alpha Nominal false-alarm probability per subgroup. Defaults to
#'   0.0027. One half is placed in each tail.
#'
#' @return A named list with components:
#' \describe{
#'   \item{lcl}{Lower control limit.}
#'   \item{ucl}{Upper control limit.}
#'   \item{center}{\eqn{d_2(n) \sigma}, the expected value of the range under
#'     normality.}
#'   \item{sigma}{The supplied \code{sigma}.}
#'   \item{n}{The supplied subgroup size.}
#'   \item{alpha}{The supplied nominal false-alarm probability.}
#' }
#'
#' @section Phase convention:
#' These limits are computed for a known or separately estimated \code{sigma}.
#' When \code{sigma} is estimated from a Phase I reference sample, the limits
#' are plug-in limits and do not account for Phase I sampling variability.
#'
#' @section Decision rule:
#' A subgroup range \eqn{R} signals out of control when \eqn{R < LCL} or
#' \eqn{R > UCL}. Equality to a limit is treated as in control.
#'
#' @section Errors:
#' An error is raised when \code{sigma} is \code{NA}, \code{NaN}, or
#' non-positive; when \code{n} is smaller than 2, non-integer, or non-finite;
#' or when \code{alpha} is not between 0 and 1.
#'
#' @references
#' Barbosa, E. P., Gneri, M. A., and Meneguetti, A. (2013). Range control
#' charts revisited: Simpler Tippett-like formulae, its practical
#' implementation, and the study of false alarm. \emph{Communications in
#' Statistics - Simulation and Computation}, 42(2), 247--262.
#' \doi{10.1080/03610918.2011.639967}.
#'
#' @seealso \code{\link{r_shewhart_limits}}, \code{\link{cchart.R}},
#'   \code{\link{alpha.risk}}, \code{\link{d2}}
#' @export
#' @importFrom stats qtukey
#' @examples
#' # Known-sigma limits for subgroup size n = 5, sigma = 2, alpha = 0.0027
#' r_exact_limits(sigma = 2, n = 5)
#'
#' # Custom alpha
#' r_exact_limits(sigma = 1, n = 10, alpha = 0.01)
#'
r_exact_limits <- function(sigma, n, alpha = ALPHA)
{
    if(!is.numeric(sigma) || length(sigma) != 1 || !is.finite(sigma) ||
       sigma <= 0)
        stop("sigma must be a finite positive scalar")
    if(length(n) != 1 || !is.numeric(n) || !is.finite(n) ||
       n < 2 || n != floor(n))
        stop("n must be an integer greater than or equal to 2")
    if(!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) ||
       alpha <= 0 || alpha >= 1)
        stop("alpha must be a finite scalar between 0 and 1")

    lcl <- sigma * stats::qtukey(alpha / 2, n, Inf)
    ucl <- sigma * stats::qtukey(1 - alpha / 2, n, Inf)
    center <- d2(n) * sigma

    list(lcl = unname(lcl), ucl = unname(ucl), center = unname(center),
         sigma = sigma, n = n, alpha = alpha)
}


#' Conventional Three-Sigma Limits for the R Chart
#'
#' Compute the conventional three-sigma (Shewhart) control limits for the R
#' chart without constructing a plot. The limits are based on the constants
#' \eqn{d_2(n)} and \eqn{d_3(n)}, the mean and standard deviation of the
#' relative range \eqn{W = R / \sigma} under normality.
#'
#' The conventional limits are
#' \deqn{LCL = \max\{0, d_2(n) - k \, d_3(n)\} \, \sigma}
#' and
#' \deqn{UCL = \{d_2(n) + k \, d_3(n)\} \, \sigma,}
#' where \eqn{k} is the number of sigma units (default 3). Because the
#' distribution of the relative range is not normal, the actual false-alarm
#' probability of these limits can be substantially larger than the nominal
#' 0.0027 associated with three-sigma limits for a normal variate. Use
#' \code{\link{alpha.risk}} to evaluate the exact false-alarm probability.
#'
#' @param sigma Positive finite scalar. The in-control process standard
#'   deviation. When estimated from Phase I data, the returned limits are
#'   plug-in estimates.
#' @param n Integer subgroup size, at least 2.
#' @param nsigmas Positive numeric scalar. The number of sigma units for the
#'   control limits. Defaults to 3.
#'
#' @return A named list with components:
#' \describe{
#'   \item{lcl}{Lower control limit (zero-truncated).}
#'   \item{ucl}{Upper control limit.}
#'   \item{center}{\eqn{d_2(n) \sigma}, the expected value of the range under
#'     normality.}
#'   \item{nsigmas}{The supplied multiplier.}
#'   \item{sigma}{The supplied \code{sigma}.}
#'   \item{n}{The supplied subgroup size.}
#' }
#'
#' @section Phase convention:
#' These limits are computed for a known or separately estimated \code{sigma}.
#' When \code{sigma} is estimated from a Phase I reference sample, the limits
#' are plug-in limits.
#'
#' @section Decision rule:
#' A subgroup range \eqn{R} signals out of control when \eqn{R < LCL} or
#' \eqn{R > UCL}. Equality to a limit is treated as in control.
#'
#' @section Errors:
#' An error is raised when \code{sigma} is \code{NA}, \code{NaN}, or
#' non-positive; when \code{n} is smaller than 2, non-integer, or non-finite;
#' or when \code{nsigmas} is non-positive.
#'
#' @references
#' Montgomery, D. C. (2009). \emph{Introduction to Statistical Quality
#' Control}, 6th ed. Wiley.
#'
#' Barbosa, E. P., Gneri, M. A., and Meneguetti, A. (2013). Range control
#' charts revisited: Simpler Tippett-like formulae, its practical
#' implementation, and the study of false alarm. \emph{Communications in
#' Statistics - Simulation and Computation}, 42(2), 247--262.
#' \doi{10.1080/03610918.2011.639967}.
#'
#' @seealso \code{\link{r_exact_limits}}, \code{\link{cchart.R}},
#'   \code{\link{alpha.risk}}, \code{\link{d2}}, \code{\link{d3}}
#' @export
#' @importFrom stats ptukey
#' @examples
#' # Conventional three-sigma limits for n = 5, sigma = 2
#' r_shewhart_limits(sigma = 2, n = 5)
#'
#' # Custom multiplier
#' r_shewhart_limits(sigma = 1, n = 10, nsigmas = 2)
#'
r_shewhart_limits <- function(sigma, n, nsigmas = SIGMA_MULT)
{
    if(!is.numeric(sigma) || length(sigma) != 1 || !is.finite(sigma) ||
       sigma <= 0)
        stop("sigma must be a finite positive scalar")
    if(length(n) != 1 || !is.numeric(n) || !is.finite(n) ||
       n < 2 || n != floor(n))
        stop("n must be an integer greater than or equal to 2")
    if(!is.numeric(nsigmas) || length(nsigmas) != 1 || !is.finite(nsigmas) ||
       nsigmas <= 0)
        stop("nsigmas must be a finite positive scalar")

    lcl <- max(0, d2(n) - nsigmas * d3(n)) * sigma
    ucl <- (d2(n) + nsigmas * d3(n)) * sigma
    center <- d2(n) * sigma

    list(lcl = unname(lcl), ucl = unname(ucl), center = unname(center),
         nsigmas = nsigmas, sigma = sigma, n = n)
}
