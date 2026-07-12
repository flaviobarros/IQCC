#' Generalized Variance Control Chart
#'
#' @param x1 Phase I subgroups accepted by \code{gv_stat}.
#' @param size Subgroup size when \code{x1} or \code{x2} is a matrix.
#' @param x2 Optional Phase II subgroups.
#' @param Sigma Optional in-control covariance matrix. If omitted, it is estimated
#' from the average Phase I covariance matrix with the determinant bias correction.
#' @inheritParams gv_limits
#' @param plot Logical; draw the chart.
#' @param ... Additional arguments passed to \code{plot}.
#' @return An object of class \code{cchart.GV}.
#' @export
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
        b3 <- .gv_b3(length(groups1), n, p)
        det_sigma <- as.numeric(determinant(Sbar, logarithm = FALSE)$modulus) / b3
        Sigma_used <- NULL
    }
    else
    {
        Sigma <- as.matrix(Sigma)
        if(!is.numeric(Sigma) || any(!is.finite(Sigma)) ||
           nrow(Sigma) != p || ncol(Sigma) != p ||
           max(abs(Sigma - t(Sigma))) > 1e-8)
            stop("Sigma must be a finite symmetric p by p matrix")
        det_sigma <- determinant(Sigma, logarithm = FALSE)
        if(det_sigma$sign <= 0)
            stop("Sigma must be positive definite")
        det_sigma <- as.numeric(det_sigma$modulus)
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
