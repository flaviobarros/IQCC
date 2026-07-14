#' Trace Statistic for Multivariate Variability
#'
#' Compute the auxiliary trace statistic
#' \deqn{T = (n - 1) \operatorname{tr}(\Sigma_0^{-1} S),}
#' where \eqn{S} is the unbiased subgroup covariance matrix and
#' \eqn{\Sigma_0} is the in-control covariance matrix.
#'
#' The statistic complements the generalized variance \eqn{|S|}. Two
#' covariance matrices can have the same determinant but different trace after
#' standardization by \eqn{\Sigma_0}; \code{trv_stat()} is designed to make
#' that structural change visible.
#'
#' @param x Multivariate subgroup data in any format accepted by
#'   \code{\link{gv_stat}}: a list of numeric matrices, a
#'   subgroup-by-observation-by-variable array, or a stacked numeric matrix.
#' @param size Positive integer subgroup size when \code{x} is a stacked
#'   matrix. It is ignored for list and array input.
#' @param Sigma0 Finite symmetric positive-definite in-control covariance
#'   matrix.
#'
#' @return A numeric vector with one trace statistic per subgroup.
#'
#' @details
#' Under independent sampling from a multivariate normal process with
#' covariance \eqn{\Sigma_0},
#' \deqn{(n - 1)S \sim W_p(n - 1, \Sigma_0)}
#' and therefore
#' \deqn{(n - 1)\operatorname{tr}(\Sigma_0^{-1}S) \sim
#'       \chi^2_{p(n - 1)}.}
#' The implementation evaluates the trace as
#' \code{(n - 1) * sum(Sigma0_inverse * S)}, avoiding an explicit matrix
#' square root while preserving the same value.
#'
#' @section Errors:
#' Errors are raised for invalid subgroup data, non-finite observations,
#' subgroup sizes smaller than two, a non-square or non-symmetric
#' \code{Sigma0}, a dimension mismatch, or a covariance matrix that is not
#' positive definite.
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
#' @seealso \code{\link{trv_limits}}, \code{\link{trv_alpha_risk}},
#' \code{\link{cchart.trV}}, \code{\link{gv_stat}}
#' @export
#' @examples
#' g1 <- cbind(c(0, 1, 2, 3), c(0, 2, 1, 3))
#' g2 <- cbind(c(1, 2, 4, 7), c(3, 1, 5, 2))
#' trv_stat(list(g1, g2), Sigma0 = diag(2))
trv_stat <- function(x, size = NULL, Sigma0)
{
    groups <- .gv_groups(x, size)
    p <- ncol(groups[[1]])
    sigma <- .trv_validate_sigma0(Sigma0, p)

    vapply(
        groups,
        function(group)
        {
            if(!is.numeric(group) || any(!is.finite(group)))
                stop("all subgroup observations must be finite numeric values")
            if(nrow(group) < 2)
                stop("each subgroup must contain at least two observations")

            S <- stats::cov(group)
            value <- (nrow(group) - 1) * sum(sigma$inverse * S)
            if(value < -sqrt(.Machine$double.eps))
                stop("trace statistic must be nonnegative")
            max(0, value)
        },
        numeric(1)
    )
}

#' Trace-Statistic Control Limits
#'
#' Compute one-sided upper control limits for the auxiliary trace statistic
#' \eqn{T = (n - 1)\operatorname{tr}(\Sigma_0^{-1}S)}.
#'
#' @param n Integer subgroup sample size, at least 2.
#' @param p Integer process dimension, at least 2.
#' @param alpha Nominal upper-tail false-alarm probability, strictly between
#'   0 and 1.
#' @param type Character string selecting the calculation:
#' \describe{
#'   \item{\code{"chisq"}}{Exact chi-square quantile with
#'     \eqn{p(n - 1)} degrees of freedom.}
#'   \item{\code{"simulation"}}{Monte Carlo quantile from the same
#'     chi-square distribution. This is mainly a diagnostic and teaching
#'     option.}
#' }
#' @param nsim Integer number of Monte Carlo draws, at least 1000. Used only
#'   when \code{type = "simulation"}.
#' @param seed \code{NULL} or a finite numeric scalar used as the Monte Carlo
#'   seed. A supplied seed makes simulated limits reproducible and restores
#'   the caller's existing random-number state on exit.
#'
#' @return A list with components \code{lcl}, \code{ucl}, \code{center},
#'   \code{type}, \code{alpha}, \code{n}, \code{p}, \code{df}, \code{nsim},
#'   and \code{seed}. The lower control limit is zero because the chart is
#'   one-sided upper.
#'
#' @export
#' @examples
#' trv_limits(n = 8, p = 2)
#' trv_limits(n = 8, p = 2, type = "simulation", nsim = 5000, seed = 2026)
trv_limits <- function(n, p, alpha = 0.0027,
                       type = c("chisq", "simulation"),
                       nsim = 100000, seed = NULL)
{
    type <- match.arg(type)
    n <- .trv_validate_sample_size(n)
    p <- .gv_validate_dimension(p)
    alpha <- .gv_validate_alpha(alpha)
    df <- p * (n - 1L)

    if(type == "chisq")
    {
        upper <- stats::qchisq(1 - alpha, df = df)
    }
    else
    {
        nsim <- .trv_validate_nsim(nsim)
        sim <- .gv_with_seed(seed, stats::rchisq(nsim, df = df))
        upper <- unname(stats::quantile(sim, 1 - alpha, type = 8))
    }

    list(lcl = 0, ucl = upper, center = df, type = type, alpha = alpha,
         n = n, p = p, df = df, nsim = nsim, seed = seed)
}

#' False-Alarm Risk for Trace-Statistic Charts
#'
#' Evaluate the actual in-control upper-tail probability of a supplied
#' \code{tr(V)} upper control limit.
#'
#' @inheritParams trv_limits
#' @param ucl Positive finite upper control limit on the trace statistic.
#' @param Sigma0 Optional in-control covariance matrix. It is accepted for API
#'   symmetry with \code{\link{trv_stat}} and validated when supplied, but the
#'   null distribution of the standardized trace statistic depends only on
#'   \code{n} and \code{p}.
#'
#' @return A list with components \code{alpha}, \code{arl0}, \code{ucl},
#'   \code{n}, \code{p}, \code{df}, and \code{method}.
#'
#' @export
#' @examples
#' lim <- trv_limits(8, 2)
#' trv_alpha_risk(8, 2, lim$ucl)
trv_alpha_risk <- function(n, p, ucl, Sigma0 = NULL,
                           type = c("chisq", "simulation"),
                           nsim = 100000, seed = NULL)
{
    type <- match.arg(type)
    n <- .trv_validate_sample_size(n)
    p <- .gv_validate_dimension(p)
    if(!is.null(Sigma0))
        .trv_validate_sigma0(Sigma0, p)
    if(!is.numeric(ucl) || length(ucl) != 1 || !is.finite(ucl) || ucl < 0)
        stop("ucl must be a nonnegative finite scalar")

    df <- p * (n - 1L)
    actual <- if(type == "chisq")
    {
        1 - stats::pchisq(ucl, df = df)
    }
    else
    {
        nsim <- .trv_validate_nsim(nsim)
        sim <- .gv_with_seed(seed, stats::rchisq(nsim, df = df))
        mean(sim > ucl)
    }

    list(alpha = actual,
         arl0 = if(actual == 0) Inf else 1 / actual,
         ucl = ucl, n = n, p = p, df = df,
         method = if(type == "chisq") "exact chi-square" else "simulation")
}

#' Trace-Statistic Control Chart
#'
#' Construct and optionally plot a Shewhart-type upper control chart for
#' multivariate process variability using the standardized trace statistic
#' \eqn{tr(V)}.
#'
#' @param x Phase I subgroup data in any format accepted by
#'   \code{\link{trv_stat}}.
#' @param size Positive integer subgroup size when \code{x} or \code{newdata}
#'   is a stacked matrix.
#' @param Sigma0 Optional finite symmetric positive-definite in-control
#'   covariance matrix. If omitted, the average Phase I covariance matrix is
#'   used as a plug-in estimate.
#' @inheritParams trv_limits
#' @param newdata Optional Phase II subgroup data in any format accepted by
#'   \code{\link{trv_stat}}. Phase II subgroups must have the same subgroup
#'   size and dimension as Phase I.
#' @param plot Logical scalar. If \code{TRUE}, draw the chart before returning
#'   the result.
#' @param ... Additional graphical arguments passed to
#'   \code{plot.cchart.trV()} and then to \code{graphics::plot()}.
#'
#' @return An object of class \code{"cchart.trV"}, a list with components:
#' \describe{
#'   \item{\code{statistics}}{Trace statistics for all Phase I followed by
#'     all Phase II subgroups.}
#'   \item{\code{phase1}, \code{phase2}}{Integer indices identifying the two
#'     phases in \code{statistics}.}
#'   \item{\code{limits}}{The complete list returned by
#'     \code{trv_limits()}.}
#'   \item{\code{out.of.control}}{Indices for which
#'     \code{statistics > ucl}. Equality to the limit does not signal.}
#'   \item{\code{Sigma0}}{The supplied or estimated in-control covariance
#'     matrix.}
#'   \item{\code{Sbar}}{The average Phase I covariance matrix when
#'     \code{Sigma0} is estimated, otherwise \code{NULL}.}
#'   \item{\code{n}, \code{p}}{Subgroup size and process dimension.}
#'   \item{\code{call}}{The matched function call.}
#' }
#'
#' @details
#' The \code{tr(V)} chart complements \code{\link{cchart.GV}}. The generalized
#' variance chart monitors determinant changes, while the trace chart monitors
#' the sum of standardized covariance eigenvalues. When \code{Sigma0 = NULL},
#' limits are plug-in limits conditional on the Phase I estimate and do not
#' include additional Phase I estimation uncertainty.
#'
#' @export
#' @examples
#' set.seed(123)
#' phase1 <- array(rnorm(6 * 8 * 2), dim = c(6, 8, 2))
#' chart <- cchart.trV(phase1, Sigma0 = diag(2), plot = FALSE)
#' chart$limits[c("center", "ucl")]
cchart.trV <- function(x, size = NULL, Sigma0 = NULL, alpha = 0.0027,
                       type = c("chisq", "simulation"), nsim = 100000,
                       seed = NULL, newdata = NULL, plot = TRUE, ...)
{
    type <- match.arg(type)
    groups1 <- .gv_groups(x, size)
    n <- nrow(groups1[[1]])
    p <- ncol(groups1[[1]])
    n <- .trv_validate_sample_size(n)

    if(is.null(Sigma0))
    {
        covariances <- lapply(groups1, stats::cov)
        Sbar <- Reduce(`+`, covariances) / length(covariances)
        sigma <- .trv_validate_sigma0(Sbar, p)
        Sigma_used <- sigma$matrix
        covariance_source <- "estimated from Phase I subgroups"
    }
    else
    {
        sigma <- .trv_validate_sigma0(Sigma0, p)
        Sigma_used <- sigma$matrix
        Sbar <- NULL
        covariance_source <- "supplied Sigma0"
    }

    stat1 <- trv_stat(groups1, Sigma0 = Sigma_used)
    limits <- trv_limits(n, p, alpha, type, nsim, seed)

    if(is.null(newdata))
    {
        stat2 <- numeric(0)
    }
    else
    {
        groups2 <- .gv_groups(newdata, size)
        if(nrow(groups2[[1]]) != n || ncol(groups2[[1]]) != p)
            stop("Phase II subgroups must match Phase I dimensions")
        stat2 <- trv_stat(groups2, Sigma0 = Sigma_used)
    }

    values <- c(stat1, stat2)
    out_of_control <- which(values > limits$ucl)
    object <- list(statistics = values, phase1 = seq_along(stat1),
                   phase2 = if(length(stat2)) length(stat1) + seq_along(stat2) else integer(0),
                   limits = limits, out.of.control = out_of_control,
                   Sigma0 = Sigma_used, Sbar = Sbar,
                   covariance_source = covariance_source,
                   n = n, p = p, call = match.call())
    class(object) <- "cchart.trV"

    if(isTRUE(plot))
        plot(object, ...)
    object
}

#' @rdname cchart.trV
#' @param x An object of class \code{"cchart.trV"}.
#' @return For \code{plot.cchart.trV()}, \code{x} invisibly.
#' @export
plot.cchart.trV <- function(x, ...)
{
    values <- x$statistics
    idx <- seq_along(values)
    ylim <- range(c(values, x$limits$lcl, x$limits$ucl), finite = TRUE)
    graphics::plot(idx, values, type = "b", xlab = "Subgroup",
                   ylab = "Trace statistic tr(V)",
                   main = "Trace-Statistic Control Chart", ylim = ylim, ...)
    graphics::abline(h = x$limits$center, lty = 2)
    graphics::abline(h = x$limits$ucl, lty = 3)
    if(length(x$phase2))
        graphics::abline(v = max(x$phase1) + 0.5, lty = 2)
    if(length(x$out.of.control))
        graphics::points(x$out.of.control, values[x$out.of.control], pch = 19)
    invisible(x)
}

#' @rdname cchart.trV
#' @param x An object of class \code{"cchart.trV"}.
#' @param digits Number of significant digits used for numerical output.
#' @param ... Additional arguments passed to the summary print method.
#'
#' @return \code{x}, invisibly.
#' @method print cchart.trV
#' @export
print.cchart.trV <- function(x,
                             digits = max(3L, getOption("digits") - 3L),
                             ...)
{
    print(summary(x), digits = digits, ...)
    invisible(x)
}

#' @rdname cchart.trV
#' @param object An object of class \code{"cchart.trV"}.
#' @param ... Currently unused.
#'
#' @return An object of class \code{"summary.cchart.trV"}.
#' @method summary cchart.trV
#' @export
summary.cchart.trV <- function(object, ...)
{
    phase <- rep("I", length(object$statistics))
    if(length(object$phase2))
        phase[object$phase2] <- "II"

    signal_index <- object$out.of.control
    signals <- data.frame(
        index = signal_index,
        phase = phase[signal_index],
        statistic = object$statistics[signal_index],
        row.names = NULL,
        stringsAsFactors = FALSE
    )

    result <- list(
        call = object$call,
        dimensions = c(n = object$n, p = object$p),
        subgroups = c(
            phase1 = length(object$phase1),
            phase2 = length(object$phase2),
            total = length(object$statistics)
        ),
        method = list(
            type = object$limits$type,
            alpha = object$limits$alpha,
            df = object$limits$df
        ),
        limits = c(
            lcl = object$limits$lcl,
            center = object$limits$center,
            ucl = object$limits$ucl
        ),
        covariance = list(
            source = object$covariance_source,
            Sigma0 = object$Sigma0,
            Sbar = object$Sbar
        ),
        statistics = summary(object$statistics),
        signals = signals
    )
    class(result) <- "summary.cchart.trV"
    result
}

#' @rdname cchart.trV
#' @param x An object of class \code{"summary.cchart.trV"}.
#' @param digits Number of significant digits used for numerical output.
#' @param ... Currently unused.
#' @return \code{x}, invisibly.
#' @method print summary.cchart.trV
#' @export
print.summary.cchart.trV <- function(x,
                                     digits = max(3L, getOption("digits") - 3L),
                                     ...)
{
    cat("Trace-Statistic Control Chart\n")
    cat("  Dimension: p =", x$dimensions[["p"]],
        "; subgroup size n =", x$dimensions[["n"]], "\n")
    cat("  Subgroups:", x$subgroups[["total"]],
        "(Phase I:", x$subgroups[["phase1"]],
        "; Phase II:", x$subgroups[["phase2"]], ")\n")
    cat("  Limits:", x$method$type,
        "; nominal alpha =", format(x$method$alpha, digits = digits),
        "; df =", x$method$df, "\n")
    cat("  Covariance:", x$covariance$source, "\n")
    cat("  Center =", format(x$limits[["center"]], digits = digits),
        "; UCL =", format(x$limits[["ucl"]], digits = digits), "\n")
    cat("  Signals:", nrow(x$signals), "\n")
    if(nrow(x$signals))
        print(x$signals, row.names = FALSE, digits = digits)
    invisible(x)
}

.trv_validate_sample_size <- function(n)
{
    if(!is.numeric(n) || length(n) != 1 || !is.finite(n) ||
       n < 2 || n != floor(n))
        stop("n must be an integer greater than or equal to 2")
    as.integer(n)
}

.trv_validate_nsim <- function(nsim)
{
    if(!is.numeric(nsim) || length(nsim) != 1 || !is.finite(nsim) ||
       nsim < 1000 || nsim != floor(nsim))
        stop("nsim must be an integer of at least 1000")
    as.integer(nsim)
}

.trv_validate_sigma0 <- function(Sigma0, p = NULL)
{
    Sigma0 <- as.matrix(Sigma0)
    if(!is.numeric(Sigma0) || any(!is.finite(Sigma0)) ||
       nrow(Sigma0) != ncol(Sigma0))
        stop("Sigma0 must be a finite square matrix")

    if(!is.null(p) && (nrow(Sigma0) != p || ncol(Sigma0) != p))
        stop("Sigma0 dimension must match the subgroup dimension")
    if(is.null(p))
        p <- .gv_validate_dimension(nrow(Sigma0))

    if(max(abs(Sigma0 - t(Sigma0))) > 1e-8)
        stop("Sigma0 must be symmetric")

    chol_sigma <- try(chol(Sigma0), silent = TRUE)
    if(inherits(chol_sigma, "try-error"))
        stop("Sigma0 must be positive definite")

    list(matrix = Sigma0, inverse = chol2inv(chol_sigma), p = p)
}
