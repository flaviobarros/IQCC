#' Double-Sampling np Chart: Average Run Length
#'
#' Compute the Average Run Length (ARL) for the double-sampling np chart
#' at a given nonconforming proportion p.
#'
#' The ARL is defined as the expected number of samples before the chart
#' signals. For a given proportion p, ARL = 1 / (1 - PT(p)), where PT(p)
#' is the total acceptance probability computed by \code{dsnp_prob_accept}.
#'
#' @param p The nonconforming proportion to evaluate. Can be a scalar or
#' numeric vector, with values in [0, 1].
#' @param n1 The first-stage sample size (positive integer).
#' @param n2 The second-stage sample size (positive integer).
#' @param wl The fractional warning limit. Must be less than ucl1.
#' @param ucl1 The fractional upper control limit for the first stage.
#' @param ucl2 The fractional upper control limit for the combined samples.
#' @return A list with the following elements:
#' \describe{
#'   \item{arl}{The Average Run Length: 1 / p_signal.}
#'   \item{pt}{Total acceptance probability from \code{dsnp_prob_accept}.}
#'   \item{p_signal}{Total signaling probability.}
#'   \item{n1, n2, wl, ucl1, ucl2}{The input chart parameters.}
#' }
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{dsnp_prob_accept}, \link{dsnp_ass}
#' @references Joekes, S., Smrekar, M. and Barbosa, E. (2015). Extending a
#' double sampling control chart for non-conforming proportion in high quality
#' processes to the case of small samples.
#' @examples
#'
#' # Published example from Joekes et al. (2015)
#' res0 <- dsnp_arl(0.005, n1 = 34, n2 = 162,
#'                  wl = 1.5, ucl1 = 2.5, ucl2 = 4.5)
#' res0$arl  # approximately 803.41
#'
#' res1 <- dsnp_arl(0.0075, n1 = 34, n2 = 162,
#'                  wl = 1.5, ucl1 = 2.5, ucl2 = 4.5)
#' res1$arl  # approximately 193.22
#'
dsnp_arl <- function(p, n1, n2, wl, ucl1, ucl2)
{
    res <- dsnp_prob_accept(p, n1, n2, wl, ucl1, ucl2)

    arl <- ifelse(res$p_signal == 0, Inf, 1 / res$p_signal)

    list(
        arl = arl,
        pt = res$pt,
        p_signal = res$p_signal,
        n1 = n1,
        n2 = n2,
        wl = wl,
        ucl1 = ucl1,
        ucl2 = ucl2
    )
}
