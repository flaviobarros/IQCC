# Internal validation and probability helpers for DS-np charts.

.dsnp_validate_probability <- function(p, name = "p", scalar = FALSE)
{
    if(scalar)
    {
        if(!is.numeric(p) || length(p) != 1 || !is.finite(p) ||
           p < 0 || p > 1)
            stop(paste0(name,
                        " must be between 0 and 1 and be a finite scalar"))
    }
    else
    {
        if(!is.numeric(p) || length(p) < 1 || any(!is.finite(p)) ||
           any(p < 0 | p > 1))
            stop(paste0(name,
                        " must be between 0 and 1 and contain finite values"))
    }
    p
}

.dsnp_validate_size <- function(n, name)
{
    if(!is.numeric(n) || length(n) != 1 || !is.finite(n) ||
       n < 1 || n != floor(n))
        stop(paste0(name, " must be a positive integer"))
    as.integer(n)
}

.dsnp_validate_limit <- function(x, name)
{
    if(!is.numeric(x) || length(x) != 1 || !is.finite(x))
        stop(paste0(name, " must be a finite numeric scalar"))
    x
}

.dsnp_thresholds <- function(wl, ucl1, ucl2 = NULL)
{
    wl <- .dsnp_validate_limit(wl, "wl")
    ucl1 <- .dsnp_validate_limit(ucl1, "ucl1")
    if(wl >= ucl1)
        stop("wl must be less than ucl1")

    wl_accept <- floor(wl)
    ucl1_reject <- floor(ucl1) + 1L

    out <- list(
        wl = wl,
        ucl1 = ucl1,
        wl_accept = as.integer(wl_accept),
        ucl1_reject = as.integer(ucl1_reject),
        d1_lower = as.integer(wl_accept + 1L),
        d1_upper = as.integer(ucl1_reject - 1L)
    )

    if(!is.null(ucl2))
    {
        ucl2 <- .dsnp_validate_limit(ucl2, "ucl2")
        if(ucl2 <= wl)
            stop("ucl2 must be greater than wl")
        out$ucl2 <- ucl2
        out$ucl2_accept <- as.integer(floor(ucl2))
    }

    out
}

.dsnp_performance <- function(p, n1, n2, wl, ucl1, ucl2)
{
    p <- .dsnp_validate_probability(p)
    n1 <- .dsnp_validate_size(n1, "n1")
    n2 <- .dsnp_validate_size(n2, "n2")
    th <- .dsnp_thresholds(wl, ucl1, ucl2)

    pa1 <- stats::pbinom(th$wl_accept, n1, p)

    if(th$d1_lower > th$d1_upper)
    {
        pa2 <- rep(0, length(p))
    }
    else
    {
        d1 <- seq.int(th$d1_lower, th$d1_upper)
        pa2 <- vapply(
            p,
            function(prob)
            {
                sum(
                    stats::dbinom(d1, n1, prob) *
                        stats::pbinom(th$ucl2_accept - d1, n2, prob)
                )
            },
            numeric(1)
        )
    }

    pt <- pmin(1, pmax(0, pa1 + pa2))
    p_signal <- pmin(1, pmax(0, 1 - pt))

    p_second <- if(th$d1_lower > th$d1_upper)
        rep(0, length(p))
    else
        stats::pbinom(th$d1_upper, n1, p) -
            stats::pbinom(th$wl_accept, n1, p)

    p_second <- pmin(1, pmax(0, p_second))
    p_decision_first <- 1 - p_second
    arl <- ifelse(p_signal == 0, Inf, 1 / p_signal)
    ass <- n1 + n2 * p_second

    list(
        pa1 = pa1,
        pa2 = pa2,
        pt = pt,
        p_signal = p_signal,
        p_decision_first = p_decision_first,
        p_second = p_second,
        arl = arl,
        ass = ass,
        n1 = n1,
        n2 = n2,
        wl = th$wl,
        ucl1 = th$ucl1,
        ucl2 = th$ucl2,
        wl_accept = th$wl_accept,
        ucl1_reject = th$ucl1_reject,
        ucl2_accept = th$ucl2_accept
    )
}

# Deliberately simple enumeration used as an independent oracle in tests.
# It sums every accepted (d1, d2) pair and is intended only for small samples.
.dsnp_reference_prob_accept <- function(p, n1, n2, wl, ucl1, ucl2)
{
    p <- .dsnp_validate_probability(p)
    n1 <- .dsnp_validate_size(n1, "n1")
    n2 <- .dsnp_validate_size(n2, "n2")
    th <- .dsnp_thresholds(wl, ucl1, ucl2)

    vapply(
        p,
        function(prob)
        {
            total <- 0
            for(d1 in 0:n1)
            {
                pr1 <- stats::dbinom(d1, n1, prob)
                if(d1 <= th$wl_accept)
                {
                    total <- total + pr1
                }
                else if(d1 < th$ucl1_reject)
                {
                    for(d2 in 0:n2)
                    {
                        if(d1 + d2 <= th$ucl2_accept)
                            total <- total + pr1 * stats::dbinom(d2, n2, prob)
                    }
                }
            }
            total
        },
        numeric(1)
    )
}
