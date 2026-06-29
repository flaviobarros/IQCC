#' Double-Sampling np Chart: Average Sample Size
#'
#' Compute the Average Sample Size (ASS) for the double-sampling np chart
#' at a given nonconforming proportion p.
#'
#' The ASS is the expected total number of items inspected per signal
#' decision. If the process is accepted or signals at the first stage,
#' n1 items are inspected. If the second stage is required, n1 + n2 items
#' are inspected. ASS = n1 + n2 * P(second stage is required).
#'
#' @param p The nonconforming proportion to evaluate. Can be a scalar or
#' numeric vector, with values in [0, 1].
#' @param n1 The first-stage sample size (positive integer).
#' @param n2 The second-stage sample size (positive integer).
#' @param wl The fractional warning limit. Must be less than ucl1.
#' @param ucl1 The fractional upper control limit for the first stage.
#' @return A list with the following elements:
#' \describe{
#'   \item{ass}{The Average Sample Size: n1 + n2 * p_second.}
#'   \item{p_second}{Probability of requiring the second sample.}
#'   \item{n1, n2, wl, ucl1}{The input chart parameters.}
#' }
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{dsnp_prob_accept}, \link{dsnp_arl}
#' @references Joekes, S., Smrekar, M. and Barbosa, E. (2015). Extending a
#' double sampling control chart for non-conforming proportion in high quality
#' processes to the case of small samples.
#' @importFrom stats pbinom
#' @examples
#'
#' # Published example from Joekes et al. (2015)
#' res <- dsnp_ass(0.005, n1 = 34, n2 = 162,
#'                 wl = 1.5, ucl1 = 2.5)
#' res$ass  # approximately 35.94
#'
dsnp_ass <- function(p, n1, n2, wl, ucl1)
{
    if(any(p < 0 | p > 1))
        stop("p must be between 0 and 1")
    if(n1 < 1 || n1 != floor(n1))
        stop("n1 must be a positive integer")
    if(n2 < 1 || n2 != floor(n2))
        stop("n2 must be a positive integer")
    if(wl >= ucl1)
        stop("wl must be less than ucl1")

    wl_accept   <- floor(wl)
    ucl1_reject <- floor(ucl1) + 1

    p_second <- pbinom(ucl1_reject - 1, n1, p) - pbinom(wl_accept, n1, p)
    ass <- n1 + n2 * p_second

    list(
        ass = ass,
        p_second = p_second,
        n1 = n1,
        n2 = n2,
        wl = wl,
        ucl1 = ucl1
    )
}
