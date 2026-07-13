#' Range Control Chart
#'
#' Build a control chart for subgroup ranges using either the conventional
#' three-sigma approximation or exact probability limits from the distribution
#' of the relative range \eqn{W = R / \sigma}.
#'
#' @param x Phase II subgroup data accepted by \code{qcc::qcc()} for an
#' \code{"R"} chart. Rows represent subgroups and columns observations within
#' subgroups.
#' @param n Integer subgroup size, at least 2. It is used to evaluate the actual
#' false-alarm probability of the conventional chart and to obtain Tukey
#' relative-range quantiles for the exact chart.
#' @param type Either \code{"norm"} for the conventional three-sigma chart or
#' \code{"tukey"} for exact equal-tail probability limits.
#' @param y Phase I subgroup data used to estimate \eqn{\sigma} through
#' \code{qcc::sd.R()} when \code{type = "tukey"}. It must be supplied for that
#' method and have the same subgroup structure as \code{x}.
#'
#' @return Invisibly, the \code{"qcc"} object returned by \code{qcc::qcc()}.
#' The function also draws the chart. For \code{type = "norm"}, a message is
#' added below the plot showing the actual false-alarm probability returned by
#' \code{alpha.risk(n)}.
#'
#' @details
#' For \code{type = "norm"}, limits are delegated to the standard
#' \code{qcc} range-chart implementation. For \code{type = "tukey"}, the
#' lower and upper limits are
#' \deqn{\hat\sigma F_W^{-1}(0.00135;n)}
#' and
#' \deqn{\hat\sigma F_W^{-1}(0.99865;n),}
#' where \eqn{\hat\sigma} is estimated from \code{y} and the quantiles are
#' obtained with \code{stats::qtukey()}.
#'
#' @section Phase convention:
#' The exact chart treats \code{y} as Phase I reference data and \code{x} as
#' the plotted monitoring data. The conventional chart uses the standard
#' estimation behavior of \code{qcc::qcc()} on \code{x}.
#'
#' @section Errors:
#' An error is raised for an unsupported \code{type}, for \code{n < 2}, or
#' when \code{y} is omitted for the exact chart. Additional data validation is
#' performed by \code{qcc::qcc()} and \code{qcc::sd.R()}.
#'
#' @references
#' Barbosa, E. P., Gneri, M. A., and Meneguetti, A. (2013). Range control
#' charts revisited: Simpler Tippett-like formulae, its practical
#' implementation, and the study of false alarm. \emph{Communications in
#' Statistics - Simulation and Computation}, 42(2), 247--262.
#' \doi{10.1080/03610918.2011.639967}.
#'
#' @seealso \code{\link{alpha.risk}}, \code{\link{cchart.S}},
#' \code{\link{table.qtukey}}
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @importFrom qcc qcc sd.R
#' @importFrom stats qtukey
#' @importFrom graphics mtext
#' @examples
#' data(pistonrings)
#' conventional <- cchart.R(pistonrings[1:25, ], 5)
#' exact <- cchart.R(
#'     pistonrings[26:40, ], 5,
#'     type = "tukey",
#'     y = pistonrings[1:25, ]
#' )
cchart.R <- function(x, n, type = c("norm", "tukey"), y = NULL)
{
    type <- match.arg(type)
    if(length(n) != 1 || !is.numeric(n) || !is.finite(n) ||
       n < 2 || n != floor(n))
        stop("n must be an integer greater than or equal to 2")

    if(type == "norm")
    {
        chart <- qcc(x, type = "R", xlab = "")
        result <- signif(alpha.risk(n), 3)
        mtext(paste("Warning: actual false-alarm probability alpha =", result,
                    "is inflated relative to 0.0027; use type = 'tukey' for exact probability limits."),
              side = 1, font = 2)
    }
    else
    {
        if(is.null(y))
            stop("y must be supplied when type = 'tukey'")
        chart <- qcc(
            x,
            type = "R",
            limits = c(qtukey(Q_LOWER, n, Inf) * sd.R(y),
                       qtukey(Q_UPPER, n, Inf) * sd.R(y))
        )
    }

    invisible(chart)
}
