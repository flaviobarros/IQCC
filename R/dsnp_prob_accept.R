#' Double-Sampling np Chart: Acceptance Probability
#'
#' Compute the total probability that the double-sampling np chart accepts
#' (does not signal) at a given nonconforming proportion.
#'
#' @param p Nonconforming proportion, scalar or vector in [0, 1].
#' @param n1 First-stage sample size.
#' @param n2 Second-stage sample size.
#' @param wl Fractional warning limit.
#' @param ucl1 Fractional first-stage upper control limit.
#' @param ucl2 Fractional combined upper control limit.
#' @return A list with first- and second-stage acceptance probabilities,
#' total acceptance and signal probabilities, second-stage probability,
#' and integer decision thresholds.
#' @export
#' @references Joekes, S., Smrekar, M. and Barbosa, E. P. (2015).
#' Extending a double sampling control chart for non-conforming proportion in
#' high quality processes to the case of small samples. Statistical Methodology,
#' 23, 35-49.
#' @examples
#' dsnp_prob_accept(0.005, 34, 162, 1.5, 2.5, 4.5)
#'
dsnp_prob_accept <- function(p, n1, n2, wl, ucl1, ucl2)
{
    res <- .dsnp_performance(p, n1, n2, wl, ucl1, ucl2)
    res[c(
        "pa1", "pa2", "pt", "p_signal", "p_decision_first", "p_second",
        "n1", "n2", "wl", "ucl1", "ucl2", "wl_accept",
        "ucl1_reject", "ucl2_accept"
    )]
}
