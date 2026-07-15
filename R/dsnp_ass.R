#' Double-Sampling np Chart: Average Sample Size
#'
#' Compute the average sample size for the double-sampling np chart under
#' complete inspection of every second-stage sample that is requested.
#'
#' The returned value is \eqn{n_1 + n_2 P(second stage)}. It does not use
#' curtailed inspection within the second-stage sample.
#'
#' @param p Nonconforming proportion, scalar or vector in \eqn{[0, 1]}.
#' @param n1 First-stage positive integer sample size.
#' @param n2 Second-stage positive integer sample size.
#' @param wl Finite fractional warning limit.
#' @param ucl1 Finite fractional first-stage upper control limit.
#' @return A list containing ASS, second-stage probability, and chart
#' parameters.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @references Joekes, S., Smrekar, M. and Barbosa, E. P. (2015). Extending a
#' double sampling control chart for non-conforming proportion in high quality
#' processes to the case of small samples. \emph{Statistical Methodology}, 23,
#' 35--49.
#' @seealso \link{dsnp_prob_accept}, \link{dsnp_arl}
#' @examples
#' dsnp_ass(0.005, 34, 162, 1.5, 2.5)$ass
#'
dsnp_ass <- function(p, n1, n2, wl, ucl1)
{
    p <- .dsnp_validate_probability(p)
    n1 <- .dsnp_validate_size(n1, "n1")
    n2 <- .dsnp_validate_size(n2, "n2")
    th <- .dsnp_thresholds(wl, ucl1)

    p_second <- if(th$d1_lower > th$d1_upper)
        rep(0, length(p))
    else
        stats::pbinom(th$d1_upper, n1, p) -
            stats::pbinom(th$wl_accept, n1, p)

    p_second <- pmin(1, pmax(0, p_second))
    ass <- n1 + n2 * p_second

    list(
        ass = ass,
        p_second = p_second,
        n1 = n1,
        n2 = n2,
        wl = th$wl,
        ucl1 = th$ucl1
    )
}
