#' Plot a DS-np Control Chart
#'
#' Plot first-stage and second-stage DS-np decisions on separate panels so
#' that the first-stage count and combined count are shown on their proper
#' scales.
#'
#' @param x An object of class \code{"cchart.DSnp"}.
#' @param ... Additional graphical parameters passed to the first panel.
#' @return Invisible \code{x}.
#' @export
#' @method plot cchart.DSnp
plot.cchart.DSnp <- function(x, ...)
{
    d <- x$data
    lim <- x$limits

    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mfrow = c(2, 1), mar = c(3.2, 4.2, 2.4, 1.2))

    y1_max <- max(c(d$x1, lim$ucl1), na.rm = TRUE)
    y1_max <- max(1, ceiling(1.1 * y1_max))

    graphics::plot(
        d$index, d$x1,
        type = "n",
        xlab = "",
        ylab = "First-stage count (x1)",
        xlim = c(1, nrow(d)),
        ylim = c(0, y1_max),
        main = "DS-np Control Chart: Stage 1",
        las = 1,
        ...
    )

    accept_first <- d$stage == "accept_first"
    signal_first <- d$stage == "signal_first"
    second <- d$stage %in% c("accept_second", "signal_second")

    if(any(accept_first))
        graphics::points(d$index[accept_first], d$x1[accept_first], pch = 16)
    if(any(second))
        graphics::points(d$index[second], d$x1[second], pch = 1, cex = 1.2)
    if(any(signal_first))
        graphics::points(d$index[signal_first], d$x1[signal_first],
                         pch = 4, col = "red", lwd = 2, cex = 1.3)

    graphics::abline(h = lim$wl, lty = 2, col = "blue")
    graphics::abline(h = lim$ucl1, lty = 2, col = "red")
    graphics::legend(
        "topleft",
        legend = c("Accept at stage 1", "Proceed to stage 2",
                   "Signal at stage 1", "WL", "UCL1"),
        pch = c(16, 1, 4, NA, NA),
        lty = c(NA, NA, NA, 2, 2),
        col = c("black", "black", "red", "blue", "red"),
        bty = "n",
        cex = 0.8
    )

    stage2 <- !is.na(d$total)
    y2_max <- max(c(d$total[stage2], lim$ucl2), na.rm = TRUE)
    if(!is.finite(y2_max))
        y2_max <- lim$ucl2
    y2_max <- max(1, ceiling(1.1 * y2_max))

    graphics::plot(
        d$index, d$total,
        type = "n",
        xlab = "Sample index",
        ylab = "Combined count (x1 + x2)",
        xlim = c(1, nrow(d)),
        ylim = c(0, y2_max),
        main = "DS-np Control Chart: Stage 2",
        las = 1
    )

    accept_second <- d$stage == "accept_second"
    signal_second <- d$stage == "signal_second"
    if(any(accept_second))
        graphics::points(d$index[accept_second], d$total[accept_second], pch = 16)
    if(any(signal_second))
        graphics::points(d$index[signal_second], d$total[signal_second],
                         pch = 4, col = "red", lwd = 2, cex = 1.3)

    graphics::abline(h = lim$ucl2, lty = 2, col = "red")
    graphics::legend(
        "topleft",
        legend = c("Accept at stage 2", "Signal at stage 2", "UCL2"),
        pch = c(16, 4, NA),
        lty = c(NA, NA, 2),
        col = c("black", "red", "red"),
        bty = "n",
        cex = 0.8
    )

    invisible(x)
}
