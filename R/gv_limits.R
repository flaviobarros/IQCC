#' Generalized Variance Control Limits
#'
#' @param n Subgroup sample size.
#' @param p Process dimension.
#' @param det_sigma Determinant of the in-control covariance matrix.
#' @param alpha False-alarm probability.
#' @param type Limit method: normal, Cornish-Fisher, exact, or simulation.
#' @param side Upper or two-sided chart.
#' @param cf_order Cornish-Fisher order, 1 or 2.
#' @param nsim Number of simulations for simulated limits.
#' @param seed Optional simulation seed.
#' @return A list containing lower and upper control limits and distribution details.
#' @export
gv_limits <- function(n, p, det_sigma = 1, alpha = 0.0027,
                      type = c("normal", "cf", "exact", "simulation"),
                      side = c("upper", "two.sided"), cf_order = 1,
                      nsim = 100000, seed = NULL)
{
    type <- match.arg(type)
    side <- match.arg(side)
    p <- .gv_validate_dimension(p)
    n <- .gv_validate_sample_size(n, p)
    det_sigma <- .gv_validate_det_sigma(det_sigma)
    alpha <- .gv_validate_alpha(alpha)
    moments <- .gv_moments(n, p, det_sigma)

    probs <- if(side == "upper") c(NA_real_, 1 - alpha) else c(alpha / 2, 1 - alpha / 2)

    if(type == "normal")
    {
        lower <- if(side == "upper") 0 else moments$mean + stats::qnorm(probs[1]) * moments$sd
        upper <- moments$mean + stats::qnorm(probs[2]) * moments$sd
    }
    else if(type == "cf")
    {
        q_upper <- .gv_cf_standard_quantile(probs[2], moments, cf_order)
        upper <- moments$mean + q_upper * moments$sd
        lower <- if(side == "upper") 0 else
            moments$mean + .gv_cf_standard_quantile(probs[1], moments, cf_order) * moments$sd
    }
    else if(type == "exact" && p == 2)
    {
        df <- 2 * n - 4
        qfun <- function(prob) stats::qchisq(prob, df)^2 / (4 * (n - 1)^2)
        lower <- if(side == "upper") 0 else det_sigma * qfun(probs[1])
        upper <- det_sigma * qfun(probs[2])
    }
    else if(type == "exact" && p == 3)
    {
        if(side != "upper")
            stop("exact p = 3 limits are available only for an upper chart")
        lower <- 0
        upper <- det_sigma * .gv_exact_d3_upper(n, alpha)
    }
    else if(type == "exact")
    {
        stop("exact limits are implemented only for p = 2 and tabulated p = 3 cases")
    }
    else
    {
        sim <- .gv_simulate_multiplier(n, p, nsim, seed)
        lower <- if(side == "upper") 0 else det_sigma * unname(stats::quantile(sim, probs[1], type = 8))
        upper <- det_sigma * unname(stats::quantile(sim, probs[2], type = 8))
    }

    lower <- max(0, lower)
    list(lcl = lower, ucl = upper, center = moments$mean, type = type,
         side = side, alpha = alpha, n = n, p = p, det_sigma = det_sigma,
         moments = moments, cf_order = cf_order, nsim = nsim, seed = seed)
}
