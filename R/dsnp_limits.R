#' Double-Sampling np Chart: Limit Search
#'
#' Enumerate feasible fractional decision limits for fixed \code{n1} and
#' \code{n2}. This function searches limits only; it does not optimize the
#' complete sampling design over sample sizes.
#'
#' @param p0 In-control nonconforming proportion.
#' @param n1 First-stage sample size.
#' @param n2 Second-stage sample size.
#' @param alpha Maximum desired false-alarm probability.
#' @param p1 Optional out-of-control proportion used for ranking.
#' @param conservative Prefer candidates with false-alarm probability no
#' greater than \code{alpha}.
#' @param allow_empty_warning Allow a degenerate single-sample scheme.
#' @param max_results Maximum number of returned candidates.
#' @return A list with the best candidate and ranked candidate table.
#' @export
#' @examples
#' dsnp_limits(0.05, 5, 10, alpha = 0.05)$best
#'
dsnp_limits <- function(p0, n1, n2, alpha = 0.0027,
                        p1 = NULL,
                        conservative = TRUE,
                        allow_empty_warning = FALSE,
                        max_results = 20)
{
    p0 <- .dsnp_validate_probability(p0, "p0", scalar = TRUE)
    n1 <- .dsnp_validate_size(n1, "n1")
    n2 <- .dsnp_validate_size(n2, "n2")
    if(!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) ||
       alpha <= 0 || alpha >= 1)
        stop("alpha must be a finite scalar between 0 and 1")
    if(!is.null(p1))
        p1 <- .dsnp_validate_probability(p1, "p1", scalar = TRUE)
    if(!is.logical(conservative) || length(conservative) != 1 ||
       is.na(conservative))
        stop("conservative must be TRUE or FALSE")
    if(!is.logical(allow_empty_warning) || length(allow_empty_warning) != 1 ||
       is.na(allow_empty_warning))
        stop("allow_empty_warning must be TRUE or FALSE")
    if(!is.numeric(max_results) || length(max_results) != 1 ||
       !is.finite(max_results) || max_results < 1 ||
       max_results != floor(max_results))
        stop("max_results must be a positive integer")

    rows <- vector("list", 0)

    for(a in seq.int(0L, n1))
    {
        b_min <- if(allow_empty_warning) a + 1L else a + 2L
        b_max <- n1 + 1L
        if(b_min > b_max)
            next

        for(b in seq.int(b_min, b_max))
        {
            empty_zone <- b == a + 1L
            c_min <- if(empty_zone) b else a + 1L
            c_max <- n1 + n2
            if(c_min > c_max)
                next

            for(c in seq.int(c_min, c_max))
            {
                wl <- a + 0.5
                ucl1 <- b - 0.5
                ucl2 <- c + 0.5

                if(empty_zone)
                {
                    pt0 <- stats::pbinom(a, n1, p0)
                    p_signal0 <- 1 - pt0
                    arl0 <- if(p_signal0 == 0) Inf else 1 / p_signal0
                    ass0 <- n1
                }
                else
                {
                    perf0 <- .dsnp_performance(p0, n1, n2, wl, ucl1, ucl2)
                    pt0 <- perf0$pt
                    p_signal0 <- perf0$p_signal
                    arl0 <- perf0$arl
                    ass0 <- perf0$ass
                }

                row <- list(
                    wl = wl,
                    ucl1 = ucl1,
                    ucl2 = ucl2,
                    wl_accept = a,
                    ucl1_reject = b,
                    ucl2_accept = c,
                    pt0 = pt0,
                    p_signal0 = p_signal0,
                    arl0 = arl0,
                    ass0 = ass0,
                    feasible = p_signal0 <= alpha
                )

                if(!is.null(p1))
                {
                    if(empty_zone)
                    {
                        pt1 <- stats::pbinom(a, n1, p1)
                        p_signal1 <- 1 - pt1
                        arl1 <- if(p_signal1 == 0) Inf else 1 / p_signal1
                        ass1 <- n1
                    }
                    else
                    {
                        perf1 <- .dsnp_performance(p1, n1, n2, wl, ucl1, ucl2)
                        pt1 <- perf1$pt
                        p_signal1 <- perf1$p_signal
                        arl1 <- perf1$arl
                        ass1 <- perf1$ass
                    }
                    row$pt1 <- pt1
                    row$p_signal1 <- p_signal1
                    row$arl1 <- arl1
                    row$ass1 <- ass1
                }

                rows[[length(rows) + 1L]] <- row
            }
        }
    }

    if(length(rows) == 0)
        stop("No candidates were generated for the requested search space")

    candidates <- do.call(
        rbind,
        lapply(rows, function(x) as.data.frame(x, stringsAsFactors = FALSE))
    )

    if(!is.null(p1))
    {
        ord <- if(conservative)
            order(-candidates$feasible, candidates$arl1, candidates$ass0,
                  abs(candidates$p_signal0 - alpha))
        else
            order(candidates$arl1, candidates$ass0,
                  abs(candidates$p_signal0 - alpha))
    }
    else
    {
        ord <- if(conservative)
            order(-candidates$feasible, abs(candidates$p_signal0 - alpha),
                  candidates$ass0, candidates$arl0)
        else
            order(abs(candidates$p_signal0 - alpha), candidates$ass0,
                  candidates$arl0)
    }

    candidates <- candidates[ord, , drop = FALSE]
    rownames(candidates) <- NULL
    candidates <- candidates[seq_len(min(nrow(candidates), max_results)), ,
                             drop = FALSE]

    list(
        best = candidates[1, , drop = FALSE],
        candidates = candidates,
        p0 = p0,
        p1 = p1,
        n1 = n1,
        n2 = n2,
        alpha = alpha,
        conservative = conservative,
        allow_empty_warning = allow_empty_warning
    )
}
