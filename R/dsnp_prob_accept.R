#' Double-Sampling np Chart: Acceptance Probability
#'
#' Compute the total probability that the double-sampling np chart accepts
#' (does not signal) at a given nonconforming proportion.
#'
#' A subgroup is accepted immediately when the first-stage count \eqn{D_1} is
#' at or below \code{floor(wl)} and signals immediately when \eqn{D_1} is at or
#' above \code{floor(ucl1) + 1}. Counts between those thresholds continue to
#' the second stage. A continued subgroup is accepted when \eqn{D_1 + D_2} is
#' at or below \code{floor(ucl2)} and signals otherwise.
#'
#' @param p Nonconforming proportion to evaluate, a finite numeric scalar or
#' vector in \eqn{[0, 1]}.
#' @param n1 First-stage sample size, a positive integer.
#' @param n2 Second-stage sample size, a positive integer.
#' @param wl Finite fractional warning limit.
#' @param ucl1 Finite fractional first-stage upper control limit greater than
#' \code{wl}.
#' @param ucl2 Finite fractional combined upper control limit greater than
#' \code{wl}.
#' @return A list with the following elements:
#' \describe{
#'   \item{pa1}{First-stage acceptance probability.}
#'   \item{pa2}{Second-stage acceptance probability.}
#'   \item{pt}{Total acceptance probability, \code{pa1 + pa2}.}
#'   \item{p_signal}{Total signal probability, \code{1 - pt}.}
#'   \item{p_decision_first}{Probability of either accepting or signaling at
#'   the first stage.}
#'   \item{p_second}{Probability that the second sample is required.}
#'   \item{n1, n2, wl, ucl1, ucl2}{The validated chart parameters.}
#'   \item{wl_accept}{Integer threshold \code{floor(wl)}; accept at stage one
#'   when \eqn{D_1} does not exceed this value.}
#'   \item{ucl1_reject}{Integer threshold \code{floor(ucl1) + 1}; signal at
#'   stage one when \eqn{D_1} is at least this value.}
#'   \item{ucl2_accept}{Integer threshold \code{floor(ucl2)}; accept at stage
#'   two when \eqn{D_1 + D_2} does not exceed this value.}
#' }
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @references Joekes, S., Smrekar, M. and Barbosa, E. P. (2015).
#' Extending a double sampling control chart for non-conforming proportion in
#' high quality processes to the case of small samples. Statistical Methodology,
#' 23, 35-49.
#' @seealso \code{\link{dsnp_arl}}, \code{\link{dsnp_ass}},
#' \code{\link{dsnp_limits}}
#' @examples
#' result <- dsnp_prob_accept(
#'     0.005, n1 = 34, n2 = 162, wl = 1.5, ucl1 = 2.5, ucl2 = 4.5
#' )
#' result$pt
#' result$p_signal
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
