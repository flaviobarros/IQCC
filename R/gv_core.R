# Internal helpers for generalized variance control charts.

.gv_validate_dimension <- function(p)
{
    if(!is.numeric(p) || length(p) != 1 || !is.finite(p) ||
       p < 2 || p != floor(p))
        stop("p must be an integer greater than or equal to 2")
    as.integer(p)
}

.gv_validate_sample_size <- function(n, p)
{
    if(!is.numeric(n) || length(n) != 1 || !is.finite(n) ||
       n <= p || n != floor(n))
        stop("n must be an integer greater than p")
    as.integer(n)
}

.gv_validate_det_sigma <- function(det_sigma)
{
    if(!is.numeric(det_sigma) || length(det_sigma) != 1 ||
       !is.finite(det_sigma) || det_sigma <= 0)
        stop("det_sigma must be a positive finite scalar")
    det_sigma
}

.gv_validate_alpha <- function(alpha)
{
    if(!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) ||
       alpha <= 0 || alpha >= 1)
        stop("alpha must be between 0 and 1")
    alpha
}

.gv_moments <- function(n, p, det_sigma = 1)
{
    p <- .gv_validate_dimension(p)
    n <- .gv_validate_sample_size(n, p)
    det_sigma <- .gv_validate_det_sigma(det_sigma)

    ordinary <- numeric(4)
    for(r in seq_len(4L))
    {
        log_moment <- p * r * log(2 / (n - 1)) + r * log(det_sigma)
        for(k in seq_len(p))
        {
            log_moment <- log_moment +
                lgamma(r + (n - k) / 2) - lgamma((n - k) / 2)
        }
        ordinary[r] <- exp(log_moment)
    }

    a1 <- ordinary[1]
    a2 <- ordinary[2]
    a3 <- ordinary[3]
    a4 <- ordinary[4]

    mu2 <- a2 - a1^2
    mu3 <- a3 - 3 * a1 * a2 + 2 * a1^3
    mu4 <- a4 - 4 * a3 * a1 + 6 * a2 * a1^2 - 3 * a1^4

    if(mu2 <= 0)
        stop("computed generalized variance is not positive")

    sd_value <- sqrt(mu2)
    skewness <- mu3 / sd_value^3
    excess_kurtosis <- mu4 / sd_value^4 - 3

    list(
        ordinary = ordinary,
        mean = a1,
        variance = mu2,
        sd = sd_value,
        skewness = skewness,
        excess_kurtosis = excess_kurtosis,
        b1 = a1 / det_sigma,
        b2 = mu2 / det_sigma^2,
        n = n,
        p = p,
        det_sigma = det_sigma
    )
}

.gv_cf_standard_quantile <- function(prob, moments, order = 1)
{
    if(!is.numeric(prob) || any(!is.finite(prob)) ||
       any(prob <= 0 | prob >= 1))
        stop("prob must contain values strictly between 0 and 1")
    if(!is.numeric(order) || length(order) != 1 ||
       !order %in% c(1, 2))
        stop("order must be 1 or 2")

    z <- stats::qnorm(prob)
    k3 <- moments$skewness
    corrected <- z + k3 * (z^2 - 1) / 6

    if(order == 2)
    {
        k4 <- moments$excess_kurtosis
        corrected <- corrected +
            k4 * (z^3 - 3 * z) / 24 -
            k3^2 * (2 * z^3 - 5 * z) / 36
    }

    corrected
}

.gv_b3 <- function(m, n, p)
{
    if(!is.numeric(m) || length(m) != 1 || !is.finite(m) ||
       m < 1 || m != floor(m))
        stop("m must be a positive integer")
    p <- .gv_validate_dimension(p)
    n <- .gv_validate_sample_size(n, p)

    total_df <- m * (n - 1)
    if(total_df < p)
        stop("m * (n - 1) must be at least p")

    prod(total_df - seq_len(p) + 1) / total_df^p
}

.gv_with_seed <- function(seed, code)
{
    if(is.null(seed))
        return(force(code))
    if(!is.numeric(seed) || length(seed) != 1 || !is.finite(seed))
        stop("seed must be NULL or a finite numeric scalar")

    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if(had_seed)
        old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

    on.exit({
        if(had_seed)
            assign(".Random.seed", old_seed, envir = .GlobalEnv)
        else if(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)

    set.seed(as.integer(seed))
    force(code)
}

.gv_simulate_multiplier <- function(n, p, nsim = 100000, seed = NULL)
{
    p <- .gv_validate_dimension(p)
    n <- .gv_validate_sample_size(n, p)
    if(!is.numeric(nsim) || length(nsim) != 1 || !is.finite(nsim) ||
       nsim < 1000 || nsim != floor(nsim))
        stop("nsim must be an integer of at least 1000")

    .gv_with_seed(seed, {
        value <- rep(1, as.integer(nsim))
        for(k in seq_len(p))
            value <- value * stats::rchisq(nsim, df = n - k)
        value / (n - 1)^p
    })
}

.gv_exact_d3_upper <- function(n, alpha)
{
    n_values <- 4:15
    q_0020 <- c(6.111, 6.453, 6.200, 5.833, 5.487, 5.180,
                4.908, 4.673, 4.468, 4.287, 4.127, 3.985)
    q_0027 <- c(5.370, 5.828, 5.656, 5.375, 5.084, 4.822,
                4.588, 4.383, 4.202, 4.042, 3.900, 3.772)

    if(!n %in% n_values)
        stop("exact p = 3 limits are tabulated only for n from 4 to 15")

    if(isTRUE(all.equal(alpha, 0.0020, tolerance = 1e-12)))
        return(q_0020[match(n, n_values)])
    if(isTRUE(all.equal(alpha, 0.0027, tolerance = 1e-12)))
        return(q_0027[match(n, n_values)])

    stop("exact p = 3 limits are available only for alpha 0.0020 or 0.0027")
}
