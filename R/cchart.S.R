#' Standard-Deviation Control Chart
#'
#' Build a control chart for subgroup standard deviations using either the
#' conventional Shewhart limits used historically by \code{qcc} or exact
#' probability limits derived from the chi-square distribution of the sample
#' variance. Numerical limits are delegated to \code{\link{s_shewhart_limits}}
#' and \code{\link{s_exact_limits}}.
#'
#' @param x Subgroup data accepted by \code{qcc::qcc()} for an \code{"S"}
#' chart. Rows represent subgroups and columns observations within subgroups.
#' @param type Either \code{"n"} for the normalized qcc-compatible limits or
#' \code{"e"} for exact equal-tail probability limits.
#' @param m Integer subgroup size, at least 2. It is required when
#' \code{type = "e"}. If omitted, a warning is issued and the normalized chart
#' is drawn instead. This argument is retained unchanged for compatibility with
#' historical positional calls.
#'
#' @return Invisibly, the \code{"qcc"} object returned by \code{qcc::qcc()}.
#' The function also draws the chart.
#'
#' @details
#' For exact limits, \eqn{(m-1)S^2/\sigma^2} follows a chi-square distribution
#' with \eqn{m-1} degrees of freedom under normality. The historical wrapper
#' estimates \eqn{\sigma} from \code{x} using \code{qcc::sd.S()} and supplies
#' that estimate to \code{s_exact_limits()}; these are therefore plug-in
#' probability limits.
#'
#' For \code{type = "n"}, the legacy qcc convention is deliberately preserved:
#' the center is the weighted mean of the observed subgroup standard deviations,
#' while \eqn{\sigma} is estimated separately by \code{qcc::sd.S()}. The
#' resulting standard error is \eqn{\hat\sigma\sqrt{1-c_4(n)^2}}. This differs
#' in finite Phase I samples from replacing the observed center by the
#' theoretical \eqn{c_4(n)\hat\sigma}; \code{cchart.S()} does not make that
#' replacement.
#'
#' @section Phase convention:
#' The existing API has no separate Phase I argument. Both the chart center and
#' \eqn{\sigma} estimate are obtained from \code{x}; exact limits additionally
#' use the supplied historical design size \code{m}. Thus both normalized and
#' exact limits are plug-in limits when used through this wrapper.
#'
#' @section Errors and warnings:
#' An error is raised for an unsupported \code{type} or an invalid supplied
#' \code{m}. If exact limits are requested without \code{m}, the function
#' warns and falls back to the normalized chart. Additional data validation is
#' performed by \code{qcc::qcc()}, \code{qcc::stats.S()}, and
#' \code{qcc::sd.S()}.
#'
#' @references
#' Montgomery, D. C. (2009). \emph{Introduction to Statistical Quality
#' Control}, 6th ed. Wiley.
#'
#' @seealso \code{\link{s_shewhart_limits}}, \code{\link{s_exact_limits}},
#'   \code{\link{cchart.R}}, \code{\link{c4}}
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @importFrom qcc qcc sd.S stats.S
#' @examples
#' data(softdrink)
#' normalized <- cchart.S(softdrink, type = "n")
#' exact <- cchart.S(softdrink, type = "e", m = 10)
cchart.S <- function(x, type = c("n", "e"), m = NULL)
{
    type <- match.arg(type)

    normalized_chart <- function(x)
    {
        data <- as.matrix(x)
        sizes <- as.integer(rowSums(!is.na(data)))
        s_stats <- stats.S(data, sizes)
        sigma_hat <- sd.S(data, sizes)
        lims <- s_shewhart_limits(
            sigma = sigma_hat,
            n = sizes,
            center = s_stats$center
        )
        qcc_limits <- if(length(unique(sizes)) == 1)
            c(lims$lcl[1], lims$ucl[1])
        else
            cbind(lims$lcl, lims$ucl)
        chart <- qcc(x, type = "S", limits = qcc_limits)
        colnames(chart$limits) <- c("LCL", "UCL")
        chart
    }

    if(type == "n")
    {
        chart <- normalized_chart(x)
    }
    else if(is.null(m))
    {
        warning("The sample size m wasn't specified, so a normalized S control chart was plotted instead.")
        chart <- normalized_chart(x)
    }
    else
    {
        if(length(m) != 1 || !is.numeric(m) || !is.finite(m) ||
           m < 2 || m != floor(m))
            stop("m must be an integer greater than or equal to 2")
        sigma_hat <- sd.S(x)
        lims <- s_exact_limits(sigma = sigma_hat, n = m)
        chart <- qcc(x, type = "S", limits = c(lims$lcl, lims$ucl))
    }

    invisible(chart)
}
