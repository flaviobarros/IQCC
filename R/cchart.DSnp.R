#' Double-Sampling np Control Chart
#'
#' Build and optionally plot a double-sampling np (DS-np) control chart for
#' monitoring the nonconforming proportion in high-quality processes.
#'
#' The DS-np chart uses two sampling stages. At the first stage, a sample of
#' size \code{n1} is inspected. If the count \code{x1} is at or below the
#' warning limit, the process is accepted. If \code{x1} exceeds the first
#' upper control limit, the process signals out-of-control immediately.
#' Otherwise, a second sample of size \code{n2} is inspected and the combined
#' count \code{x1 + x2} is compared to the second upper control limit.
#'
#' Limits can be supplied manually via \code{wl}, \code{ucl1}, and
#' \code{ucl2}, obtained from a pre-computed \code{dsnp_limits()} object via
#' the \code{limits} argument, or computed automatically inside the function
#' when neither is provided.
#'
#' The fractional limits are converted to integer thresholds using the
#' convention from the numerical core functions:
#' \itemize{
#'   \item \code{wl_accept = floor(wl)}: accept at first stage if
#'         \code{x1 <= wl_accept}.
#'   \item \code{ucl1_reject = floor(ucl1) + 1}: signal at first stage if
#'         \code{x1 >= ucl1_reject}.
#'   \item \code{ucl2_accept = floor(ucl2)}: accept at second stage if
#'         \code{x1 + x2 <= ucl2_accept}.
#' }
#'
#' @param x1 Integer vector of nonconforming counts from the first sample.
#' @param n1 First-stage sample size (positive integer).
#' @param n2 Second-stage sample size (positive integer).
#' @param p0 In-control nonconforming proportion.
#' @param x2 Optional integer vector of nonconforming counts from the second
#'   sample. Must have the same length as \code{x1} when provided. Use
#'   \code{NA} for observations where no second sample was taken. Required
#'   for observations where \code{x1} falls in the intermediate (warning)
#'   zone.
#' @param wl Fractional warning limit. Must be less than \code{ucl1}.
#' @param ucl1 Fractional upper control limit for the first stage. Must be
#'   greater than \code{wl}.
#' @param ucl2 Fractional upper control limit for the combined samples.
#' @param limits Optional object returned by \code{dsnp_limits()}. When
#'   provided, \code{wl}, \code{ucl1}, and \code{ucl2} are taken from
#'   \code{limits$best}.
#' @param alpha Maximum desired false alarm probability at \code{p0}. Used
#'   only when limits need to be computed via \code{dsnp_limits()}.
#' @param p1 Optional out-of-control proportion. When provided, performance
#'   metrics at \code{p1} are included in the returned object.
#' @param plot Logical. If \code{TRUE} (default), draws the control chart.
#'   If \code{FALSE}, only returns the result object.
#' @param ... Additional arguments passed to \code{plot()} (currently
#'   unused).
#'
#' @return An object of class \code{"cchart.DSnp"}, which is a list with the
#' following elements:
#' \describe{
#'   \item{call}{The matched call.}
#'   \item{data}{A data.frame with columns \code{index}, \code{x1},
#'     \code{x2}, \code{total}, \code{stage}, and \code{signal}.}
#'   \item{limits}{A list with the fractional and integer thresholds:
#'     \code{wl}, \code{ucl1}, \code{ucl2}, \code{wl_accept},
#'     \code{ucl1_reject}, \code{ucl2_accept}.}
#'   \item{parameters}{A list with \code{n1}, \code{n2}, \code{p0},
#'     \code{alpha}, and \code{p1}.}
#'   \item{performance}{A list with in-control performance metrics
#'     (\code{arl0}, \code{ass0}, \code{p_signal0}), and optionally
#'     out-of-control metrics (\code{arl1}, \code{ass1},
#'     \code{p_signal1}) when \code{p1} is provided.}
#' }
#'
#' @export
#' @importFrom graphics legend abline plot points
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \code{\link{dsnp_limits}}, \code{\link{dsnp_prob_accept}},
#'   \code{\link{dsnp_arl}}, \code{\link{dsnp_ass}}
#' @references Joekes, S., Smrekar, M. and Barbosa, E. (2015). Extending a
#' double sampling control chart for non-conforming proportion in high quality
#' processes to the case of small samples. \emph{Statistical Methodology}.
#' @examples
#'
#' # Small example with manual limits
#' x1 <- c(0, 1, 2, 3, 1, 0, 2, 4, 1, 0)
#' x2 <- c(NA, NA, 2, NA, NA, NA, 3, NA, NA, NA)
#' res <- cchart.DSnp(x1, n1 = 10, n2 = 20, p0 = 0.05,
#'                    x2 = x2, wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
#'                    plot = FALSE)
#' res$limits
#' res$performance
#'
cchart.DSnp <- function(x1, n1, n2, p0,
                        x2 = NULL,
                        wl = NULL,
                        ucl1 = NULL,
                        ucl2 = NULL,
                        limits = NULL,
                        alpha = 0.0027,
                        p1 = NULL,
                        plot = TRUE,
                        ...)
{
    cl <- match.call()

    # --- Validate x1 ---
    if(!is.numeric(x1) || length(x1) < 1)
        stop("x1 must be a non-empty numeric vector")
    if(any(x1 < 0))
        stop("x1 must not contain negative values")
    if(any(x1 > n1))
        stop("x1 values must not exceed n1")
    if(any(x1 != floor(x1)))
        stop("x1 must contain integer values")

    m <- length(x1)

    # --- Resolve limits ---
    have_limits_obj <- !is.null(limits)
    have_manual     <- !is.null(wl) || !is.null(ucl1) || !is.null(ucl2)
    have_none       <- !have_limits_obj && !have_manual

    if(have_limits_obj && have_manual)
        stop("Cannot specify both 'limits' and manual 'wl'/'ucl1'/'ucl2'")

    if(have_manual && (is.null(wl) || is.null(ucl1) || is.null(ucl2)))
        stop("All of 'wl', 'ucl1', and 'ucl2' must be provided together")

    if(have_limits_obj)
    {
        if(!is.null(limits$best))
        {
            wl   <- limits$best$wl
            ucl1 <- limits$best$ucl1
            ucl2 <- limits$best$ucl2
        }
        else
        {
            stop("'limits' object does not contain a 'best' element")
        }
    }

    if(have_none)
    {
        lim <- dsnp_limits(p0, n1, n2, alpha = alpha, p1 = p1,
                           max_results = 1)
        wl   <- lim$best$wl
        ucl1 <- lim$best$ucl1
        ucl2 <- lim$best$ucl2
    }

    # --- Validate limits ---
    if(!is.numeric(wl) || !is.numeric(ucl1) || !is.numeric(ucl2))
        stop("wl, ucl1, and ucl2 must be numeric")
    if(length(wl) != 1 || length(ucl1) != 1 || length(ucl2) != 1)
        stop("wl, ucl1, and ucl2 must be scalar")
    if(wl >= ucl1)
        stop("wl must be less than ucl1")
    if(ucl2 <= wl)
        stop("ucl2 must be greater than wl")

    # --- Integer thresholds ---
    wl_accept   <- floor(wl)
    ucl1_reject <- floor(ucl1) + 1
    ucl2_accept <- floor(ucl2)

    # --- Validate x2 ---
    if(!is.null(x2))
    {
        if(length(x2) != m)
            stop("x2 must have the same length as x1")
        if(any(!is.na(x2) & x2 < 0))
            stop("x2 must not contain negative values")
        if(any(!is.na(x2) & x2 > n2))
            stop("x2 values must not exceed n2")
        if(any(!is.na(x2) & x2 != floor(x2)))
            stop("x2 must contain integer values")
    }

    # --- Classify each observation ---
    index <- seq_len(m)
    stage <- character(m)
    signal <- logical(m)
    total <- rep(NA_real_, m)
    x2_used <- if(!is.null(x2)) x2 else rep(NA_real_, m)

    for(i in seq_len(m))
    {
        d1 <- x1[i]

        if(d1 <= wl_accept)
        {
            stage[i]  <- "accept_first"
            signal[i] <- FALSE
        }
        else if(d1 >= ucl1_reject)
        {
            stage[i]  <- "signal_first"
            signal[i] <- TRUE
        }
        else
        {
            # Intermediate zone: second sample required
            if(is.null(x2) || is.na(x2_used[i]))
                stop(paste0("Second sample (x2) is required for observation ",
                            i, " where x1 = ", d1,
                            " falls in the intermediate zone"))
            d2 <- x2_used[i]
            total[i] <- d1 + d2
            if(total[i] > ucl2_accept)
            {
                stage[i]  <- "signal_second"
                signal[i] <- TRUE
            }
            else
            {
                stage[i]  <- "accept_second"
                signal[i] <- FALSE
            }
        }
    }

    # --- Build data frame ---
    data_df <- data.frame(
        index  = index,
        x1     = x1,
        x2     = x2_used,
        total  = total,
        stage  = stage,
        signal = signal,
        stringsAsFactors = FALSE
    )

    # --- Performance metrics ---
    arl0_res  <- dsnp_arl(p0, n1, n2, wl, ucl1, ucl2)
    ass0_res  <- dsnp_ass(p0, n1, n2, wl, ucl1)
    pa0_res   <- dsnp_prob_accept(p0, n1, n2, wl, ucl1, ucl2)

    performance <- list(
        arl0       = arl0_res$arl,
        ass0       = ass0_res$ass,
        p_signal0  = pa0_res$p_signal
    )

    if(!is.null(p1))
    {
        arl1_res <- dsnp_arl(p1, n1, n2, wl, ucl1, ucl2)
        ass1_res <- dsnp_ass(p1, n1, n2, wl, ucl1)
        pa1_res  <- dsnp_prob_accept(p1, n1, n2, wl, ucl1, ucl2)

        performance$arl1      <- arl1_res$arl
        performance$ass1      <- ass1_res$ass
        performance$p_signal1 <- pa1_res$p_signal
    }

    # --- Assemble result ---
    result <- list(
        call        = cl,
        data        = data_df,
        limits      = list(
            wl           = wl,
            ucl1         = ucl1,
            ucl2         = ucl2,
            wl_accept    = wl_accept,
            ucl1_reject  = ucl1_reject,
            ucl2_accept  = ucl2_accept
        ),
        parameters  = list(
            n1    = n1,
            n2    = n2,
            p0    = p0,
            alpha = alpha,
            p1    = p1
        ),
        performance = performance
    )
    class(result) <- "cchart.DSnp"

    # --- Plot ---
    if(plot)
        plot.cchart.DSnp(result, ...)

    result
}

#' Plot a DS-np Control Chart
#'
#' S3 method for plotting objects of class \code{"cchart.DSnp"}.
#'
#' The plot shows the first-stage counts \code{x1} against the sample index.
#' Points requiring a second sample are shown as open circles. Points that
#' signal (at either stage) are shown in red. Horizontal lines mark the
#' fractional limits.
#'
#' Note that \code{ucl2} is on the scale of the combined count
#' \code{x1 + x2}. The plot is a simple operational visualization; the
#' full decision logic is in the returned object.
#'
#' @param x An object of class \code{"cchart.DSnp"}.
#' @param ... Additional graphical parameters (currently unused).
#' @return Invisible \code{x}.
#' @export
#' @method plot cchart.DSnp
plot.cchart.DSnp <- function(x, ...)
{
    d    <- x$data
    lim  <- x$limits
    n1   <- x$parameters$n1

    # Determine y-axis range
    y_max <- max(d$x1, na.rm = TRUE)
    y_max <- max(y_max, lim$ucl1 + 1, lim$ucl2, na.rm = TRUE)
    y_max <- ceiling(y_max * 1.1)
    if(y_max < 5) y_max <- 5

    # Base plot: x1 vs index
    plot(d$index, d$x1, type = "n",
         xlab = "Sample index", ylab = "Count (x1)",
         xlim = c(1, nrow(d)),
         ylim = c(0, y_max),
         main = "DS-np Control Chart",
         las = 1)

    # Points that stayed at first stage (no second sample needed)
    first_only <- d$stage %in% c("accept_first", "signal_first")
    points(d$index[first_only], d$x1[first_only],
           pch = 16, cex = 1.2)

    # Points that needed a second sample
    second <- d$stage %in% c("accept_second", "signal_second")
    if(any(second))
        points(d$index[second], d$x1[second],
               pch = 1, cex = 1.2)

    # Highlight signals in red
    if(any(d$signal))
        points(d$index[d$signal], d$x1[d$signal],
               pch = 4, cex = 1.4, col = "red", lwd = 2)

    # Limit lines
    abline(h = lim$wl,   lty = 2, col = "blue")
    abline(h = lim$ucl1, lty = 2, col = "red")
    abline(h = lim$ucl2, lty = 3, col = "darkgreen")

    # Legend
    legend("topleft",
           legend = c(
               "First stage only",
               "Second sample needed",
               "Signal",
               paste0("wl = ", lim$wl),
               paste0("ucl1 = ", lim$ucl1),
               paste0("ucl2 = ", lim$ucl2)
           ),
           pch   = c(16, 1, 4, NA, NA, NA),
           lty   = c(NA, NA, NA, 2, 2, 3),
           col   = c("black", "black", "red", "blue", "red", "darkgreen"),
           cex   = 0.8,
           bg    = "white",
           bty   = "n")

    invisible(x)
}
