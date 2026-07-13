#' Generalized Variance Control Chart
#'
#' Construct and optionally plot a Shewhart-type chart for multivariate process
#' variability based on the generalized variance statistic \eqn{|S|}, the
#' determinant of each subgroup sample covariance matrix.
#'
#' Phase I subgroups define the chart design and, unless \code{Sigma} is
#' supplied, estimate the in-control generalized variance. Optional Phase II
#' subgroups are then evaluated using the same subgroup size, process
#' dimension, and fixed control limits.
#'
#' @param x1 Phase I subgroup data in any format accepted by
#' \code{gv_stat()}: a list of equal-sized numeric matrices, a
#' subgroup-by-observation-by-variable array, or a numeric matrix containing
#' consecutive subgroups.
#' @param size Positive integer subgroup size when \code{x1} or \code{x2} is a
#' stacked matrix. It is ignored for list and array input.
#' @param x2 Optional Phase II subgroup data in a format accepted by
#' \code{gv_stat()}. Phase II subgroups must have the same number of
#' observations and variables as the Phase I subgroups.
#' @param Sigma Optional finite symmetric positive-definite \eqn{p \times p}
#' in-control covariance matrix. If supplied, its determinant defines the
#' chart limits. If omitted, the function averages the Phase I covariance
#' matrices and estimates
#' \deqn{|\Sigma| = |\bar S| / b_3,}
#' where \eqn{b_3} is the finite-Phase-I determinant bias correction based on
#' the number of Phase I subgroups, subgroup size, and dimension.
#' @inheritParams gv_limits
#' @param plot Logical scalar. If \code{TRUE}, draw the chart before returning
#' the result. If \code{FALSE}, construct the object without plotting.
#' @param ... Additional graphical arguments passed to
#' \code{plot.cchart.GV()} and then to \code{graphics::plot()}.
#'
#' @return An object of class \code{"cchart.GV"}, a list with components:
#' \describe{
#'   \item{\code{statistics}}{Generalized variances for all Phase I followed
#'     by all Phase II subgroups.}
#'   \item{\code{phase1}, \code{phase2}}{Integer indices identifying the two
#'     phases in \code{statistics}.}
#'   \item{\code{limits}}{The complete list returned by \code{gv_limits()}.}
#'   \item{\code{out.of.control}}{Indices for which
#'     \code{statistics < lcl} or \code{statistics > ucl}. Equality to a limit
#'     does not signal.}
#'   \item{\code{Sbar}}{The average Phase I covariance matrix when
#'     \code{Sigma} is estimated, otherwise \code{NULL}.}
#'   \item{\code{Sigma}}{The supplied in-control covariance matrix, or
#'     \code{NULL} when the determinant is estimated from Phase I.}
#'   \item{\code{b3}}{The determinant bias-correction factor when Phase I
#'     estimation is used, otherwise \code{NA_real_}.}
#'   \item{\code{n}, \code{p}}{Subgroup size and process dimension.}
#'   \item{\code{call}}{The matched function call.}
#' }
#'
#' @details
#' Each subgroup must contain finite observations on at least two variables,
#' and the subgroup size must exceed the process dimension. When
#' \code{Sigma = NULL}, the average Phase I covariance matrix must itself be
#' positive definite. The control-line center is the exact in-control mean of
#' \eqn{|S|}, not generally \eqn{|\Sigma|}.
#'
#' \code{plot.cchart.GV()} displays subgroup generalized variances, the center
#' line, lower and upper limits, a vertical separator before Phase II, and
#' solid points at signaled subgroups. The plot method returns its input
#' invisibly.
#'
#' @section Phase I and Phase II convention:
#' Phase I data are used both as historical plotted points and, when
#' \code{Sigma} is absent, to estimate the in-control determinant. Phase II
#' data do not alter the estimate or the limits. For a strict prospective
#' chart with known parameters, supply \code{Sigma}.
#'
#' @section Simulated limits:
#' When \code{type = "simulation"}, \code{nsim} and \code{seed} are passed to
#' \code{gv_limits()}. A supplied seed makes the result reproducible and the
#' caller's existing random-number state is restored.
#'
#' @section Errors:
#' Errors are raised for invalid subgroup representations, unequal subgroup
#' dimensions, non-finite observations, \eqn{n <= p}, an invalid or
#' non-positive-definite \code{Sigma}, a singular Phase I average covariance
#' matrix, and any unsupported limit configuration documented in
#' \code{gv_limits()}.
#'
#' @references
#' Barbosa, E. P., Gneri, M. A., and Meneguetti, A. \emph{Improving
#' Shewhart-type Generalized Variance Control Charts for Multivariate Process
#' Variability Monitoring using Cornish-Fisher Quantile Correction, Meijer-G
#' Function and Other Tools}. Research report.
#'
#' Alt, F. B. (1984). Multivariate quality control. In Johnson, N. L. and
#' Kotz, S. (eds.), \emph{Encyclopedia of Statistical Sciences}, Vol. 6,
#' 110--122. Wiley.
#'
#' @seealso \code{\link{gv_stat}}, \code{\link{gv_limits}},
#' \code{\link{gv_alpha_risk}}, \code{\link{print.cchart.GV}}
#' @export
#' @examples
#' set.seed(123)
#' phase1 <- array(rnorm(6 * 8 * 2), dim = c(6, 8, 2))
#'
#' # Known in-control covariance, exact dimension-two limits.
#' chart <- cchart.GV(
#'     phase1,
#'     Sigma = diag(2),
#'     type = "exact",
#'     plot = FALSE
#' )
#' chart$limits[c("lcl", "center", "ucl")]
#' summary(chart)
#'
#' # Estimate |Sigma| from Phase I and evaluate two Phase II subgroups.
#' phase2 <- array(rnorm(2 * 8 * 2), dim = c(2, 8, 2))
#' estimated <- cchart.GV(
#'     phase1,
#'     x2 = phase2,
#'     type = "cf",
#'     plot = FALSE
#' )
#' estimated$phase2
cchart.GV <- function(x1, size = NULL, x2 = NULL, Sigma = NULL,
                      alpha = 0.0027,
                      type = c("normal", "cf", "exact", "simulation"),
                      side = c("upper", "two.sided"), cf_order = 1,
                      nsim = 100000, seed = NULL, plot = TRUE, ...)
{
    type <- match.arg(type)
    side <- match.arg(side)
    groups1 <- .gv_groups(x1, size)
    n <- nrow(groups1[[1]])
    p <- ncol(groups1[[1]])
    stat1 <- gv_stat(groups1)

    if(is.null(Sigma))
    {
        covariances <- lapply(groups1, stats::cov)
        Sbar <- Reduce(`+`, covariances) / length(covariances)
        if(any(!is.finite(Sbar)) ||
           inherits(try(chol(Sbar), silent = TRUE), "try-error"))
            stop("The Phase I average covariance matrix must be positive definite")

        det_sbar <- determinant(Sbar, logarithm = FALSE)
        if(det_sbar$sign <= 0 || !is.finite(as.numeric(det_sbar$modulus)))
            stop("The Phase I average covariance matrix must have a positive finite determinant")

        b3 <- .gv_b3(length(groups1), n, p)
        det_sigma <- as.numeric(det_sbar$modulus) / b3
        Sigma_used <- NULL
    }
    else
    {
        Sigma <- as.matrix(Sigma)
        if(!is.numeric(Sigma) || any(!is.finite(Sigma)) ||
           nrow(Sigma) != p || ncol(Sigma) != p ||
           max(abs(Sigma - t(Sigma))) > 1e-8)
            stop("Sigma must be a finite symmetric p by p matrix")
        if(inherits(try(chol(Sigma), silent = TRUE), "try-error"))
            stop("Sigma must be positive definite")

        det_sigma_obj <- determinant(Sigma, logarithm = FALSE)
        if(det_sigma_obj$sign <= 0 ||
           !is.finite(as.numeric(det_sigma_obj$modulus)))
            stop("Sigma must be positive definite")
        det_sigma <- as.numeric(det_sigma_obj$modulus)
        Sigma_used <- Sigma
        b3 <- NA_real_
        Sbar <- NULL
    }

    limits <- gv_limits(n, p, det_sigma, alpha, type, side,
                        cf_order, nsim, seed)

    if(is.null(x2))
    {
        stat2 <- numeric(0)
    }
    else
    {
        groups2 <- .gv_groups(x2, size)
        if(nrow(groups2[[1]]) != n || ncol(groups2[[1]]) != p)
            stop("Phase II subgroups must match Phase I dimensions")
        stat2 <- gv_stat(groups2)
    }

    values <- c(stat1, stat2)
    out_of_control <- which(values < limits$lcl | values > limits$ucl)
    object <- list(statistics = values, phase1 = seq_along(stat1),
                   phase2 = if(length(stat2)) length(stat1) + seq_along(stat2) else integer(0),
                   limits = limits, out.of.control = out_of_control,
                   Sbar = Sbar, Sigma = Sigma_used, b3 = b3,
                   n = n, p = p, call = match.call())
    class(object) <- "cchart.GV"

    if(isTRUE(plot))
        plot(object, ...)
    object
}

#' @rdname cchart.GV
#' @param x An object of class \code{"cchart.GV"}.
#' @return For \code{plot.cchart.GV()}, \code{x} invisibly.
#' @export
plot.cchart.GV <- function(x, ...)
{
    values <- x$statistics
    idx <- seq_along(values)
    ylim <- range(c(values, x$limits$lcl, x$limits$ucl), finite = TRUE)
    graphics::plot(idx, values, type = "b", xlab = "Subgroup",
                   ylab = expression("Generalized variance " * "|S|"),
                   main = "Generalized Variance Control Chart", ylim = ylim, ...)
    graphics::abline(h = x$limits$center, lty = 2)
    graphics::abline(h = x$limits$lcl, lty = 3)
    graphics::abline(h = x$limits$ucl, lty = 3)
    if(length(x$phase2))
        graphics::abline(v = max(x$phase1) + 0.5, lty = 2)
    if(length(x$out.of.control))
        graphics::points(x$out.of.control, values[x$out.of.control], pch = 19)
    invisible(x)
}
