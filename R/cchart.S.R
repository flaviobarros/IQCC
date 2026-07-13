#' Standard-Deviation Control Chart
#'
#' Build a control chart for subgroup standard deviations using either the
#' normalized limits supplied by \code{qcc::qcc()} or exact probability limits
#' derived from the chi-square distribution of the sample variance.
#'
#' @param x Subgroup data accepted by \code{qcc::qcc()} for an \code{"S"}
#' chart. Rows represent subgroups and columns observations within subgroups.
#' @param type Either \code{"n"} for the normalized qcc limits or \code{"e"}
#' for exact equal-tail probability limits.
#' @param m Integer subgroup size, at least 2. It is required when
#' \code{type = "e"}. If omitted, a warning is issued and the normalized chart
#' is drawn instead.
#'
#' @return Invisibly, the \code{"qcc"} object returned by \code{qcc::qcc()}.
#' The function also draws the chart.
#'
#' @details
#' Under multivariate normal sampling reduced to a univariate subgroup,
#' \eqn{(m-1)S^2/\sigma^2} follows a chi-square distribution with
#' \eqn{m-1} degrees of freedom. The exact limits are therefore
#' \deqn{\hat\sigma\sqrt{\chi^2_{0.00135,m-1}/(m-1)}}
#' and
#' \deqn{\hat\sigma\sqrt{\chi^2_{0.99865,m-1}/(m-1)},}
#' where \eqn{\hat\sigma} is obtained with \code{qcc::sd.S(x)}.
#'
#' @section Errors and warnings:
#' An error is raised for an unsupported \code{type} or an invalid supplied
#' \code{m}. If exact limits are requested without \code{m}, the function
#' warns and falls back to the normalized chart. Additional data validation is
#' performed by \code{qcc::qcc()} and \code{qcc::sd.S()}.
#'
#' @references
#' Montgomery, D. C. (2009). \emph{Introduction to Statistical Quality
#' Control}, 6th ed. Wiley.
#'
#' @seealso \code{\link{cchart.R}}, \code{\link{c4}}
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @importFrom qcc qcc sd.S
#' @importFrom stats qchisq
#' @examples
#' data(softdrink)
#' normalized <- cchart.S(softdrink, type = "n")
#' exact <- cchart.S(softdrink, type = "e", m = 10)
cchart.S <- function(x, type = c("n", "e"), m = NULL)
{
    type <- match.arg(type)

    if(type == "n")
    {
        chart <- qcc(x, type = "S")
    }
    else if(is.null(m))
    {
        warning("The sample size m wasn't specified, so a normalized S control chart was plotted instead.")
        chart <- qcc(x, type = "S")
    }
    else
    {
        if(length(m) != 1 || !is.numeric(m) || !is.finite(m) ||
           m < 2 || m != floor(m))
            stop("m must be an integer greater than or equal to 2")
        limits <- c(
            sqrt(qchisq(Q_LOWER, m - 1) / (m - 1)) * sd.S(x),
            sqrt(qchisq(Q_UPPER, m - 1) / (m - 1)) * sd.S(x)
        )
        chart <- qcc(x, type = "S", limits = limits)
    }

    invisible(chart)
}
