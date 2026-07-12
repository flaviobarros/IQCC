#' Double-Sampling np Chart: Average Run Length
#'
#' Compute the Average Run Length for the double-sampling np chart.
#'
#' @param p Nonconforming proportion, scalar or vector in [0, 1].
#' @param n1 First-stage sample size.
#' @param n2 Second-stage sample size.
#' @param wl Fractional warning limit.
#' @param ucl1 Fractional first-stage upper control limit.
#' @param ucl2 Fractional combined upper control limit.
#' @return A list containing ARL, acceptance and signal probabilities, and
#' chart parameters.
#' @export
#' @seealso \link{dsnp_prob_accept}, \link{dsnp_ass}
#' @examples
#' dsnp_arl(0.005, 34, 162, 1.5, 2.5, 4.5)$arl
#'
dsnp_arl <- function(p, n1, n2, wl, ucl1, ucl2)
{
    res <- .dsnp_performance(p, n1, n2, wl, ucl1, ucl2)
    list(
        arl = res$arl,
        pt = res$pt,
        p_signal = res$p_signal,
        n1 = res$n1,
        n2 = res$n2,
        wl = res$wl,
        ucl1 = res$ucl1,
        ucl2 = res$ucl2
    )
}
