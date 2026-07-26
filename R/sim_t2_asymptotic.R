#' Simulate Asymptotic Distribution of Hotelling T²
#'
#' Monte Carlo simulation to study the convergence of the Hotelling T²
#' statistic to its asymptotic \eqn{\chi^2_p} distribution under various
#' multivariate distributions. Used to validate the asymptotic robustness
#' result of Gneri and Barbosa (2006).
#'
#' For each combination of sample size \eqn{n}, dimension \eqn{p}, and
#' distribution, the function generates \code{nsim} samples, computes the
#' Hotelling T² statistic, and compares empirical quantiles against the
#' limiting \eqn{\chi^2_p} distribution.
#'
#' @param n Integer vector of sample sizes to evaluate. Default is
#'   \code{c(10, 30, 100, 500)}.
#' @param p Integer vector of dimensions to evaluate. Default is
#'   \code{c(2, 5)}.
#' @param distributions Character vector of distribution names.
#'   Available options:
#'   \describe{
#'     \item{\code{"normal"}}{Multivariate normal \eqn{N(0, \Sigma)}.
#'       All moments finite. Baseline case.}
#'     \item{\code{"t5"}}{Multivariate t with 5 degrees of freedom,
#'       scaled to covariance \eqn{\Sigma}. Symmetric, heavy-tailed,
#'       finite fourth moment (\eqn{4 < 5}).}
#'     \item{\code{"gamma2"}}{Independent Gamma(2, 1) margins,
#'       centered and scaled to covariance \eqn{\Sigma}. Asymmetric,
#'       all moments finite.}
#'     \item{\code{"t4"}}{Multivariate t with 4 degrees of freedom
#'       (stress test). Has infinite fourth moment, violating the
#'       formal condition of Theorem 3.}
#'   }
#' @param nsim Number of Monte Carlo replications. Default is 10000.
#' @param sig_levels Numeric vector of significance levels (quantiles)
#'   to compare. Default is \code{c(0.90, 0.95, 0.99)}.
#' @param seed Random seed for reproducibility. Default is \code{42}.
#' @param rho Correlation parameter for the covariance matrix
#'   \eqn{\Sigma_{ij} = 1} if \eqn{i = j}, \eqn{\rho} otherwise.
#'   Default is \code{0.3}.
#' @return A data frame (invisible \code{\link[tibble]{tibble}}) with columns:
#'   \describe{
#'     \item{\code{n}}{Sample size.}
#'     \item{\code{p}}{Dimension.}
#'     \item{\code{distribution}}{Distribution name.}
#'     \item{\code{level}}{Quantile level (e.g., 0.95).}
#'     \item{\code{empirical}}{Empirical quantile of simulated T².}
#'     \item{\code{chisq}}{Theoretical \eqn{\chi^2_p} quantile.}
#'     \item{\code{mcse}}{Monte Carlo standard error of the
#'       empirical quantile.}
#'     \item{\code{nsim}}{Number of replications used.}
#'   }
#' @section RNG preservation:
#'   The function saves and restores \code{.Random.seed} on exit, so it
#'   does not alter the global RNG state.
#' @examples
#' # Quick test with few replications
#' res <- sim_t2_asymptotic(
#'   n = c(30, 100),
#'   p = 2,
#'   distributions = c("normal", "t5"),
#'   nsim = 500,
#'   seed = 42
#' )
#' res
#'
#' @references
#' Gneri, M. A. and Barbosa, E. P. (2006). "Robustez Asintótica de la
#' Estadística de Hotelling". Sección 4.2, Teorema 3, pp. 34-36.
#'
#' @export
#' @importFrom stats cov dchisq quantile rchisq rgamma rnorm rt
sim_t2_asymptotic <- function(
    n = c(10, 30, 100, 500),
    p = c(2, 5),
    distributions = c("normal", "t5", "gamma2"),
    nsim = 10000,
    sig_levels = c(0.90, 0.95, 0.99),
    seed = 42,
    rho = 0.3
) {
  # ── Input validation ──────────────────────────────────────────────────────
  stopifnot(
    "`n` must be a vector of positive integers" =
      is.numeric(n) && all(n > 0 & n == as.integer(n)),
    "`p` must be a vector of positive integers" =
      is.numeric(p) && all(p > 0 & p == as.integer(p)),
    "`nsim` must be a single positive integer" =
      is.numeric(nsim) && length(nsim) == 1 && nsim > 0,
    "`rho` must be in (-1, 1)" =
      is.numeric(rho) && length(rho) == 1 && rho > -1 && rho < 1,
    "`seed` must be a valid integer seed" =
      is.numeric(seed) && length(seed) == 1
  )

  valid_dists <- c("normal", "t5", "gamma2", "t4")
  dist_names <- match.arg(distributions, choices = valid_dists, several.ok = TRUE)

  # ── RNG preservation ──────────────────────────────────────────────────────
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    old_seed <- .GlobalEnv$.Random.seed
    on.exit(.GlobalEnv$.Random.seed <- old_seed)
  } else {
    on.exit(rm(".Random.seed", envir = .GlobalEnv))
  }
  set.seed(seed)

  # ── Build covariance matrix ───────────────────────────────────────────────
  build_sigma <- function(p, rho) {
    sigma <- matrix(rho, nrow = p, ncol = p)
    diag(sigma) <- 1
    sigma
  }

  # ── Distribution samplers ─────────────────────────────────────────────────
  # All return an n x p matrix with (approximately) mean 0 and covariance Sigma

  rnormal <- function(n, p, sigma) {
    MASS::mvrnorm(n, mu = rep(0, p), Sigma = sigma)
  }

  rt5 <- function(n, p, sigma) {
    # Multivariate t with 5 df: covariance = (df/(df-2)) * scale
    # So we set scale = sigma * (df-2)/df to get Cov = sigma
    df <- 5
    scale_mat <- sigma * (df - 2) / df
    x <- matrix(rt(n * p, df = df), nrow = n, ncol = p)
    # Apply covariance structure via Cholesky
    chol_scale <- chol(scale_mat)
    x %*% chol_scale
  }

  rt4 <- function(n, p, sigma) {
    # Multivariate t with 4 df: infinite 4th moment
    # scale = sigma * (df-2)/df
    df <- 4
    scale_mat <- sigma * (df - 2) / df
    x <- matrix(rt(n * p, df = df), nrow = n, ncol = p)
    chol_scale <- chol(scale_mat)
    x %*% chol_scale
  }

  rgamma2 <- function(n, p, sigma) {
    # Independent Gamma(2, 1) margins, then centered and scaled
    # Gamma(2,1) has mean 2, variance 2
    x <- matrix(rgamma(n * p, shape = 2, rate = 1), nrow = n, ncol = p)
    # Center to mean 0
    x <- sweep(x, 2, 2, FUN = "-")
    # Scale to have variance 1
    x <- sweep(x, 2, sqrt(2), FUN = "/")
    # Apply correlation structure
    chol_sigma <- chol(sigma)
    x %*% chol_sigma
  }

  # ── Single T² computation ─────────────────────────────────────────────────
  compute_t2 <- function(x) {
    n_obs <- nrow(x)
    xbar <- colMeans(x)
    if (n_obs > 1) {
      s <- cov(x)
    } else {
      return(NA_real_)
    }
    tryCatch(
      n_obs * t(xbar) %*% solve(s) %*% xbar,
      error = function(e) NA_real_
    )
  }

  # ── Main simulation loop ──────────────────────────────────────────────────
  results_list <- list()
  row_idx <- 1

  for (dim_p in p) {
    sigma <- build_sigma(dim_p, rho)

    for (dist_name in dist_names) {
      sampler <- switch(dist_name,
        normal = rnormal,
        t5     = rt5,
        t4     = rt4,
        gamma2 = rgamma2
      )

      for (sample_size in n) {
        # Generate nsim T² statistics
        t2_vals <- replicate(nsim, {
          x <- sampler(sample_size, dim_p, sigma)
          compute_t2(x)
        })

        # Remove NAs (from singular covariance matrices)
        t2_vals <- t2_vals[!is.na(t2_vals)]
        n_valid <- length(t2_vals)

        # Theoretical chi-squared quantiles
        chisq_quants <- qchisq(sig_levels, df = dim_p)

        # Empirical quantiles
        emp_quants <- quantile(t2_vals, probs = sig_levels, names = FALSE)

        # Monte Carlo standard error for quantile estimate
        # For a p-quantile, MCSE = sqrt(p*(1-p) / (n * f(q)^2))
        # Using the density of chi-squared as approximation
        mcse <- sqrt(sig_levels * (1 - sig_levels) /
                     (n_valid * dchisq(chisq_quants, df = dim_p)^2))

        for (k in seq_along(sig_levels)) {
          results_list[[row_idx]] <- data.frame(
            n            = sample_size,
            p            = dim_p,
            distribution = dist_name,
            level        = sig_levels[k],
            empirical    = emp_quants[k],
            chisq        = chisq_quants[k],
            mcse         = mcse[k],
            nsim         = n_valid,
            stringsAsFactors = FALSE
          )
          row_idx <- row_idx + 1
        }
      }
    }
  }

  result <- do.call(rbind, results_list)
  rownames(result) <- NULL
  result
}
