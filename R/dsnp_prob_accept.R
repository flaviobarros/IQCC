#' Double-Sampling np Chart: Acceptance Probability
#'
#' Compute the total probability that the double-sampling np chart accepts
#' (does not signal) at a given nonconforming proportion p.
#'
#' The chart uses two sampling stages. At the first stage, a sample of size
#' n1 is inspected. If the count d1 is below the warning limit (wl), the
#' process is accepted. If d1 exceeds the upper control limit (ucl1), the
#' process signals out-of-control. If d1 falls between wl and ucl1, a second
#' sample of size n2 is inspected and the combined count d1 + d2 is compared
#' to ucl2.
#'
#' @param p The nonconforming proportion to evaluate. Can be a scalar or
#' numeric vector, with values in [0, 1].
#' @param n1 The first-stage sample size (positive integer).
#' @param n2 The second-stage sample size (positive integer).
#' @param wl The fractional warning limit. Must be less than ucl1.
#' @param ucl1 The fractional upper control limit for the first stage.
#' Must be greater than wl.
#' @param ucl2 The fractional upper control limit for the combined samples.
#' Must be greater than wl.
#' @return A list with the following elements:
#' \describe{
#'   \item{pa1}{Probability of acceptance at the first stage: P(D1 <= floor(wl)).}
#'   \item{pa2}{Probability of acceptance at the second stage.}
#'   \item{pt}{Total acceptance probability: pa1 + pa2.}
#'   \item{p_signal}{Total signaling probability: 1 - pt.}
#'   \item{p_decision_first}{Probability that a decision (accept or signal) is
#'   made on the first sample.}
#'   \item{p_second}{Probability of requiring the second sample: 1 - p_decision_first.}
#'   \item{n1, n2, wl, ucl1, ucl2}{The input chart parameters.}
#'   \item{wl_accept}{Integer threshold: floor(wl). Accept if D1 <= wl_accept.}
#'   \item{ucl1_reject}{Integer threshold: floor(ucl1) + 1. Signal if D1 >= ucl1_reject.}
#'   \item{ucl2_accept}{Integer threshold: floor(ucl2). Accept if D1 + D2 <= ucl2_accept.}
#' }
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @references Joekes, S., Smrekar, M. and Barbosa, E. (2015). Extending a
#' double sampling control chart for non-conforming proportion in high quality
#' processes to the case of small samples.
#' @importFrom stats dbinom pbinom
#' @examples
#'
#' # Published example from Joekes et al. (2015)
#' res <- dsnp_prob_accept(0.005, n1 = 34, n2 = 162,
#'                         wl = 1.5, ucl1 = 2.5, ucl2 = 4.5)
#' res$pt
#' res$p_signal
#'
dsnp_prob_accept <- function(p, n1, n2, wl, ucl1, ucl2)
{
    if(any(p < 0 | p > 1))
        stop("p must be between 0 and 1")
    if(n1 < 1 || n1 != floor(n1))
        stop("n1 must be a positive integer")
    if(n2 < 1 || n2 != floor(n2))
        stop("n2 must be a positive integer")
    if(wl >= ucl1)
        stop("wl must be less than ucl1")
    if(ucl2 <= wl)
        stop("ucl2 must be greater than wl")

    wl_accept   <- floor(wl)
    ucl1_reject <- floor(ucl1) + 1
    ucl2_accept <- floor(ucl2)

    pa1 <- pbinom(wl_accept, n1, p)

    d1_lower <- wl_accept + 1
    d1_upper <- ucl1_reject - 1

    if(d1_lower > d1_upper)
    {
        pa2 <- rep(0, length(p))
    }
    else
    {
        d1_seq <- d1_lower:d1_upper
        prob_d1 <- outer(p, d1_seq, function(pi, d) dbinom(d, n1, pi))
        cond_prob <- outer(p, d1_seq, function(pi, d) pbinom(ucl2_accept - d, n2, pi))
        pa2 <- rowSums(prob_d1 * cond_prob)
    }

    pt <- pa1 + pa2
    p_signal <- 1 - pt

    p_d1_le_wl <- pbinom(wl_accept, n1, p)
    p_d1_ge_ucl1 <- 1 - pbinom(ucl1_reject - 1, n1, p)
    p_decision_first <- p_d1_le_wl + p_d1_ge_ucl1
    p_second <- 1 - p_decision_first

    list(
        pa1 = pa1,
        pa2 = pa2,
        pt = pt,
        p_signal = p_signal,
        p_decision_first = p_decision_first,
        p_second = p_second,
        n1 = n1,
        n2 = n2,
        wl = wl,
        ucl1 = ucl1,
        ucl2 = ucl2,
        wl_accept = wl_accept,
        ucl1_reject = ucl1_reject,
        ucl2_accept = ucl2_accept
    )
}
