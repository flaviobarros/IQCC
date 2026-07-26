#' Double-Sampling np Chart: Average Sample Size
#'
#' Compute the average sample size for the double-sampling np chart.
#'
#' By default (\code{curtailed = FALSE}), every second-stage sample that is
#' requested is fully inspected. Therefore
#' \deqn{ASS(p) = n_1 + n_2 P_p(\text{second stage}).}
#'
#' When \code{curtailed = TRUE}, inspection of the second sample stops as soon
#' as the cumulative count of non-conformities exceeds \code{ucl2}.  For each
#' warning-zone first-stage count \eqn{d_1}, define
#' \eqn{r(d_1) = \lfloor ucl_2 \rfloor - d_1 + 1} as the number of
#' non-conformances needed to reject.  The expected number of stage-2 items
#' inspected is
#' \deqn{E[M(d_1)] = \sum_{j=0}^{n_2 - 1} P(Bin(j, p) \le r(d_1) - 1),}
#' with \eqn{E[M(d_1)] = 0} when \eqn{r(d_1) \le 0}.  Then
#' \deqn{ASS_{\text{curtailed}}(p) = n_1 + \sum_{d_1 = a+1}^{b-1}
#'       P(D_1 = d_1) \, E[M(d_1)].}
#'
#' Curtailed inspection does not change the signal probability or ARL; it only
#' reduces the number of items inspected when the eventual decision is already
#' determined before the full second sample is observed.
#'
#' @param p Nonconforming proportion to evaluate, a finite numeric scalar or
#' vector in \eqn{[0, 1]}.
#' @param n1 First-stage sample size, a positive integer.
#' @param n2 Second-stage sample size, a positive integer.
#' @param wl Finite fractional warning limit.
#' @param ucl1 Finite fractional first-stage upper control limit greater than
#' \code{wl}.
#' @param ucl2 Finite fractional second-stage upper control limit. Required
#' when \code{curtailed = TRUE}.
#' @param curtailed Logical. If \code{FALSE} (default), assume complete
#' inspection of every second-stage sample. If \code{TRUE}, use curtailed
#' (truncated) inspection within the second sample.
#' @return A list with:
#' \describe{
#'   \item{ass}{Average sample size (numeric vector).}
#'   \item{p_second}{Probability that the second sample is required.}
#'   \item{n1, n2, wl, ucl1, ucl2}{Validated chart parameters.}
#'   \item{curtailed}{The convention used.}
#' }
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @references Joekes, S., Smrekar, M. and Barbosa, E. P. (2015). Extending a
#' double sampling control chart for non-conforming proportion in high quality
#' processes to the case of small samples. \emph{Statistical Methodology}, 23,
#' 35--49. \doi{10.1016/j.stamet.2014.09.003}.
#' @seealso \link{dsnp_prob_accept}, \link{dsnp_arl}
#' @examples
#' dsnp_ass(0.005, 34, 162, 1.5, 2.5)$ass
#'
#' # Curtailed inspection
#' dsnp_ass(0.005, 34, 162, 1.5, 2.5, ucl2 = 4.5, curtailed = TRUE)$ass
#'
dsnp_ass <- function(p, n1, n2, wl, ucl1, ucl2 = NULL, curtailed = FALSE)
{
    p <- .dsnp_validate_probability(p)
    n1 <- .dsnp_validate_size(n1, "n1")
    n2 <- .dsnp_validate_size(n2, "n2")
    th <- .dsnp_thresholds(wl, ucl1, ucl2)

    p_second <- if(th$d1_lower > th$d1_upper)
        rep(0, length(p))
    else
        stats::pbinom(th$d1_upper, n1, p) -
            stats::pbinom(th$wl_accept, n1, p)

    p_second <- pmin(1, pmax(0, p_second))

    if(curtailed)
    {
        if(is.null(ucl2))
            stop("ucl2 must be provided when curtailed = TRUE")

        ass <- .dsnp_curtailed_ass(p, n1, n2, th)
    }
    else
    {
        ass <- n1 + n2 * p_second
    }

    list(
        ass = as.numeric(ass),
        p_second = p_second,
        n1 = n1,
        n2 = n2,
        wl = th$wl,
        ucl1 = th$ucl1,
        ucl2 = if(is.null(ucl2)) NULL else th$ucl2,
        curtailed = curtailed
    )
}
