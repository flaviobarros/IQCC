#' Double-Sampling np Chart: Average Run Length
#'
#' Compute the average run length for the double-sampling np chart as the
#' reciprocal of the signal probability at each supplied nonconforming
#' proportion.
#'
#' @param p Nonconforming proportion, scalar or vector in \eqn{[0, 1]}.
#' @param n1 First-stage positive integer sample size.
#' @param n2 Second-stage positive integer sample size.
#' @param wl Finite fractional warning limit.
#' @param ucl1 Finite fractional first-stage upper control limit.
#' @param ucl2 Finite fractional combined upper control limit.
#' @return A list containing ARL, acceptance and signal probabilities, and
#' chart parameters.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @references Joekes, S., Smrekar, M. and Barbosa, E. P. (2015). Extending a
#' double sampling control chart for non-conforming proportion in high quality
#' processes to the case of small samples. \emph{Statistical Methodology}, 23,
#' 35--49.
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
