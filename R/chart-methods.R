#' Print and Summarize a Generalized Variance Control Chart
#'
#' Display the chart design, Phase I and Phase II subgroup counts, control
#' limits, and the number of signaled subgroups.
#'
#' @param x An object of class \code{"cchart.GV"} or
#' \code{"summary.cchart.GV"}.
#' @param digits Number of significant digits used for numerical output.
#' @param ... Additional arguments passed to the summary print method;
#' otherwise currently unused.
#'
#' @return \code{x}, invisibly.
#' @method print cchart.GV
#' @export
print.cchart.GV <- function(x,
                            digits = max(3L, getOption("digits") - 3L),
                            ...)
{
    print(summary(x), digits = digits, ...)
    invisible(x)
}

#' Summarize a Generalized Variance Control Chart
#'
#' Construct a compact statistical summary of a generalized variance chart,
#' including its design, limit method, covariance source, descriptive summary
#' of the plotted statistics, and signaled subgroups.
#'
#' @param object An object of class \code{"cchart.GV"}.
#' @param ... Currently unused.
#'
#' @return An object of class \code{"summary.cchart.GV"}. The returned list
#' contains \code{call}, \code{dimensions}, \code{subgroups}, \code{method},
#' \code{limits}, \code{covariance}, \code{statistics}, and \code{signals}.
#' The \code{signals} component is a data frame with subgroup index, phase, and
#' generalized variance for every point outside the control limits.
#' @rdname print.cchart.GV
#' @method summary cchart.GV
#' @export
summary.cchart.GV <- function(object, ...)
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
            side = object$limits$side,
            alpha = object$limits$alpha,
            det_sigma = object$limits$det_sigma
        ),
        limits = c(
            lcl = object$limits$lcl,
            center = object$limits$center,
            ucl = object$limits$ucl
        ),
        covariance = list(
            source = if(is.null(object$Sigma))
                "estimated from Phase I subgroups"
            else
                "supplied Sigma",
            Sigma = object$Sigma,
            Sbar = object$Sbar,
            b3 = object$b3
        ),
        statistics = summary(object$statistics),
        signals = signals
    )
    class(result) <- "summary.cchart.GV"
    result
}

#' @return \code{x}, invisibly.
#' @rdname print.cchart.GV
#' @method print summary.cchart.GV
#' @export
print.summary.cchart.GV <- function(x,
                                    digits = max(3L, getOption("digits") - 3L),
                                    ...)
{
    cat("Generalized Variance Control Chart\n")
    cat("  Dimension: p =", x$dimensions[["p"]],
        "; subgroup size n =", x$dimensions[["n"]], "\n")
    cat("  Subgroups:", x$subgroups[["total"]],
        "(Phase I:", x$subgroups[["phase1"]],
        "; Phase II:", x$subgroups[["phase2"]], ")\n")
    cat("  Limits:", x$method$type, "/", x$method$side,
        "; nominal alpha =", format(x$method$alpha, digits = digits), "\n")
    cat("  Covariance:", x$covariance$source, "\n")
    cat("  LCL =", format(x$limits[["lcl"]], digits = digits),
        "; center =", format(x$limits[["center"]], digits = digits),
        "; UCL =", format(x$limits[["ucl"]], digits = digits), "\n")
    cat("  Signals:", nrow(x$signals), "\n")
    if(nrow(x$signals))
        print(x$signals, row.names = FALSE, digits = digits)
    invisible(x)
}

#' Print and Summarize a Double-Sampling np Control Chart
#'
#' Display the sampling plan, integer decision thresholds, operating
#' characteristics, stage decisions, and number of signaled observations.
#'
#' @param x An object of class \code{"cchart.DSnp"} or
#' \code{"summary.cchart.DSnp"}.
#' @param digits Number of significant digits used for numerical output.
#' @param ... Additional arguments passed to the summary print method;
#' otherwise currently unused.
#'
#' @return \code{x}, invisibly.
#' @method print cchart.DSnp
#' @export
print.cchart.DSnp <- function(x,
                              digits = max(3L, getOption("digits") - 3L),
                              ...)
{
    print(summary(x), digits = digits, ...)
    invisible(x)
}

#' Summarize a Double-Sampling np Control Chart
#'
#' Construct a compact summary of the sampling plan, decision thresholds,
#' in-control and optional out-of-control performance, stage outcomes, and
#' signaled observations.
#'
#' @param object An object of class \code{"cchart.DSnp"}.
#' @param ... Currently unused.
#'
#' @return An object of class \code{"summary.cchart.DSnp"}. The returned list
#' contains \code{call}, \code{observations}, \code{parameters},
#' \code{limits}, \code{performance}, \code{stage_counts}, and
#' \code{signals}. The \code{signals} component contains the observations
#' that signaled at either sampling stage.
#' @rdname print.cchart.DSnp
#' @method summary cchart.DSnp
#' @export
summary.cchart.DSnp <- function(object, ...)
{
    stage_levels <- c(
        "accept_first", "accept_second", "signal_first", "signal_second"
    )
    stage_counts <- table(factor(object$data$stage, levels = stage_levels))
    names(stage_counts) <- stage_levels

    signal_columns <- c("index", "x1", "x2", "total", "stage")
    signals <- object$data[object$data$signal, signal_columns, drop = FALSE]
    row.names(signals) <- NULL

    result <- list(
        call = object$call,
        observations = nrow(object$data),
        parameters = object$parameters,
        limits = object$limits,
        performance = object$performance,
        stage_counts = stage_counts,
        signals = signals
    )
    class(result) <- "summary.cchart.DSnp"
    result
}

#' @return \code{x}, invisibly.
#' @rdname print.cchart.DSnp
#' @method print summary.cchart.DSnp
#' @export
print.summary.cchart.DSnp <- function(x,
                                      digits = max(3L, getOption("digits") - 3L),
                                      ...)
{
    cat("Double-Sampling np Control Chart\n")
    cat("  Observations:", x$observations, "\n")
    cat("  Sample sizes: n1 =", x$parameters$n1,
        "; n2 =", x$parameters$n2, "\n")
    cat("  In-control proportion p0 =",
        format(x$parameters$p0, digits = digits))
    if(!is.null(x$parameters$p1))
        cat("; alternative p1 =", format(x$parameters$p1, digits = digits))
    cat("\n")
    cat("  First-stage accept if D1 <=", x$limits$wl_accept,
        "; signal if D1 >=", x$limits$ucl1_reject, "\n")
    cat("  Second-stage accept if D1 + D2 <=", x$limits$ucl2_accept,
        "; signal otherwise\n")

    performance <- unlist(x$performance, use.names = TRUE)
    cat("  Performance:",
        paste(names(performance),
              format(performance, digits = digits),
              sep = " = ", collapse = "; "), "\n")
    cat("  Stage outcomes:",
        paste(names(x$stage_counts), as.integer(x$stage_counts),
              sep = " = ", collapse = "; "), "\n")
    cat("  Signals:", nrow(x$signals), "\n")
    if(nrow(x$signals))
        print(x$signals, row.names = FALSE, digits = digits)
    invisible(x)
}
