#' False-Alarm Risk for Generalized Variance Charts
#'
#' @inheritParams gv_limits
#' @return A list with the nominal and actual false-alarm probabilities and ARL0.
#' @export
gv_alpha_risk <- function(n, p, det_sigma = 1, alpha = 0.0027,
                          type = c("normal", "cf", "exact", "simulation"),
                          side = c("upper", "two.sided"), cf_order = 1,
                          nsim = 200000, seed = NULL)
{
    type <- match.arg(type)
    side <- match.arg(side)
    limits <- gv_limits(n, p, det_sigma, alpha, type, side,
                        cf_order, nsim, seed)

    if(p == 2)
    {
        df <- 2 * n - 4
        cdf <- function(x)
        {
            if(x <= 0)
                return(0)
            stats::pchisq(2 * (n - 1) * sqrt(x / det_sigma), df)
        }
        actual <- if(side == "upper")
            1 - cdf(limits$ucl)
        else
            cdf(limits$lcl) + 1 - cdf(limits$ucl)
    }
    else
    {
        sim <- det_sigma * .gv_simulate_multiplier(n, p, nsim,
                                                   if(is.null(seed)) NULL else seed + 1)
        actual <- if(side == "upper")
            mean(sim > limits$ucl)
        else
            mean(sim < limits$lcl | sim > limits$ucl)
    }

    list(alpha = actual, nominal_alpha = alpha,
         arl0 = if(actual == 0) Inf else 1 / actual,
         limits = limits, method = if(p == 2) "exact evaluation" else "simulation")
}
