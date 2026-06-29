#' Double-Sampling np Chart: Limit Search
#'
#' Search for feasible fractional control limits for the double-sampling np
#' chart by enumerating integer threshold combinations and evaluating each
#' candidate using the numeric core functions \code{dsnp_prob_accept},
#' \code{dsnp_arl}, and \code{dsnp_ass}.
#'
#' The function enumerates all valid combinations of integer thresholds
#' (wl_accept, ucl1_reject, ucl2_accept), converts them to fractional
#' limits compatible with \code{dsnp_prob_accept}, and evaluates each
#' candidate's performance at the in-control proportion p0. When p1 is
#' supplied, out-of-control metrics are also computed and used for ranking.
#'
#' When the warning zone is empty (ucl1_reject = wl_accept + 1), the
#' chart degenerates to a single-sample scheme and probabilities are
#' computed directly via \code{pbinom} without calling the core functions.
#'
#' @param p0 The in-control nonconforming proportion. Must be in [0, 1].
#' @param n1 The first-stage sample size (positive integer).
#' @param n2 The second-stage sample size (positive integer).
#' @param alpha The maximum desired false alarm probability at p0. Must be
#' in (0, 1). When \code{conservative = TRUE}, candidates with
#' p_signal0 <= alpha are preferred.
#' @param p1 Optional out-of-control proportion. When provided, candidates
#' are ranked by ARL1 (lower is better).
#' @param conservative Logical. If \code{TRUE} (default), feasible
#' candidates (p_signal0 <= alpha) are ranked first. If \code{FALSE},
#' all candidates are returned ordered by proximity to alpha.
#' @param allow_empty_warning Logical. If \code{FALSE} (default), discard
#' candidates with no integer in the warning zone (ucl1_reject <=
#' wl_accept + 1). This avoids single-sample disguised as DS-np.
#' @param max_results Maximum number of candidates to return. Must be a
#' positive integer.
#' @return A list with the following elements:
#' \describe{
#'   \item{best}{The first row of candidates (a data.frame row).}
#'   \item{candidates}{A data.frame with all evaluated candidates,
#'   sorted by the ranking criteria. Columns include wl, ucl1, ucl2,
#'   wl_accept, ucl1_reject, ucl2_accept, pt0, p_signal0, arl0, ass0,
#'   feasible, and (if p1 is provided) pt1, p_signal1, arl1, ass1.}
#'   \item{p0, p1, n1, n2, alpha}{The input parameters.}
#'   \item{conservative, allow_empty_warning}{The input flags.}
#' }
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{dsnp_prob_accept}, \link{dsnp_arl}, \link{dsnp_ass}
#' @references Joekes, S., Smrekar, M. and Barbosa, E. (2015). Extending a
#' double sampling control chart for non-conforming proportion in high quality
#' processes to the case of small samples.
#' @importFrom stats dbinom pbinom
#' @examples
#'
#' # Small example for fast execution
#' res <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.05)
#' res$best
#' nrow(res$candidates)
#'
#' # With out-of-control proportion for ranking
#' res <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.05,
#'                    p1 = 0.10)
#' res$best[, c("wl", "ucl1", "ucl2", "arl0", "arl1", "ass0")]
#'
dsnp_limits <- function(p0, n1, n2, alpha = 0.0027,
                        p1 = NULL,
                        conservative = TRUE,
                        allow_empty_warning = FALSE,
                        max_results = 20)
{
    if(p0 < 0 || p0 > 1)
        stop("p0 must be between 0 and 1")
    if(n1 < 1 || n1 != floor(n1))
        stop("n1 must be a positive integer")
    if(n2 < 1 || n2 != floor(n2))
        stop("n2 must be a positive integer")
    if(alpha <= 0 || alpha >= 1)
        stop("alpha must be between 0 and 1")
    if(max_results < 1 || max_results != floor(max_results))
        stop("max_results must be a positive integer")
    if(!is.null(p1) && (p1 < 0 || p1 > 1))
        stop("p1 must be between 0 and 1")

    has_p1 <- !is.null(p1)

    eval_candidate <- function(p, n1, n2, a, b, empty_zone)
    {
        if(empty_zone)
        {
            pa1 <- pbinom(a, n1, p)
            pt  <- pa1
            p_sig <- 1 - pt
            arl <- if(p_sig == 0) Inf else 1 / p_sig
            ass <- n1
            p_sec <- 0
        }
        else
        {
            wl   <- a + 0.5
            ucl1 <- b - 0.5
            ucl2 <- a + 1.5

            pa   <- dsnp_prob_accept(p, n1, n2, wl, ucl1, ucl2)
            arl_r <- dsnp_arl(p, n1, n2, wl, ucl1, ucl2)
            ass_r <- dsnp_ass(p, n1, n2, wl, ucl1)

            pt   <- pa$pt
            p_sig <- pa$p_signal
            arl  <- arl_r$arl
            ass  <- ass_r$ass
            p_sec <- pa$p_second
        }

        list(pt = pt, p_signal = p_sig, arl = arl, ass = ass, p_second = p_sec)
    }

    results <- list()

    for(a in 0:n1)
    {
        b_min <- if(allow_empty_warning) a + 1 else a + 2
        for(b in b_min:(n1 + 1))
        {
            empty_zone <- (b == a + 1)

            c_min <- if(empty_zone) b else a + 1
            c_max <- n1 + n2

            for(c in c_min:c_max)
            {
                wl   <- a + 0.5
                ucl1 <- b - 0.5
                ucl2 <- c + 0.5

                ev0 <- eval_candidate(p0, n1, n2, a, b, empty_zone)

                row <- data.frame(
                    wl            = wl,
                    ucl1          = ucl1,
                    ucl2          = ucl2,
                    wl_accept     = a,
                    ucl1_reject   = b,
                    ucl2_accept   = c,
                    pt0           = ev0$pt,
                    p_signal0     = ev0$p_signal,
                    arl0          = ev0$arl,
                    ass0          = ev0$ass,
                    feasible      = ev0$p_signal <= alpha,
                    stringsAsFactors = FALSE
                )

                if(has_p1)
                {
                    ev1 <- eval_candidate(p1, n1, n2, a, b, empty_zone)

                    row$pt1       <- ev1$pt
                    row$p_signal1 <- ev1$p_signal
                    row$arl1      <- ev1$arl
                    row$ass1      <- ev1$ass
                }

                results[[length(results) + 1]] <- row
            }
        }
    }

    if(length(results) == 0)
        stop("No feasible candidates found. Try different n1, n2, or alpha.")

    candidates <- do.call(rbind, results)

    if(has_p1)
    {
        if(conservative)
        {
            candidates <- candidates[order(
                -candidates$feasible,
                candidates$arl1,
                candidates$ass0,
                abs(candidates$p_signal0 - alpha)
            ), ]
        }
        else
        {
            candidates <- candidates[order(
                candidates$arl1,
                candidates$ass0,
                abs(candidates$p_signal0 - alpha)
            ), ]
        }
    }
    else
    {
        if(conservative)
        {
            candidates <- candidates[order(
                -candidates$feasible,
                abs(candidates$p_signal0 - alpha),
                candidates$ass0,
                candidates$arl0
            ), ]
        }
        else
        {
            candidates <- candidates[order(
                abs(candidates$p_signal0 - alpha),
                candidates$ass0,
                candidates$arl0
            ), ]
        }
    }

    rownames(candidates) <- NULL

    if(nrow(candidates) > max_results)
        candidates <- candidates[seq_len(max_results), ]

    list(
        best                = candidates[1, ],
        candidates          = candidates,
        p0                  = p0,
        p1                  = p1,
        n1                  = n1,
        n2                  = n2,
        alpha               = alpha,
        conservative        = conservative,
        allow_empty_warning = allow_empty_warning
    )
}
