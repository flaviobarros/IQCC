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
#' The T² statistic is computed from a single sample \eqn{X_1, \dots, X_n}
#' as \eqn{T^2 = n (\bar{X} - \mu)' S^{-1} (\bar{X} - \mu)} where
#' \eqn{\mu = 0} is the known null mean and \eqn{S} is the sample
#' covariance. This matches the form in Theorem 3 of Gneri and Barbosa
#' (2006). It does \strong{not} match the Phase I statistic
#' \code{\link{T2.1}}, which uses estimated grand means and pooled
#' covariance from multiple subgroups.
#'
#' @param n Integer vector of sample sizes to evaluate. Each element must
#'   satisfy \eqn{n > p} for the corresponding dimension. Default is
#'   \code{c(10, 30, 100, 500)}.
#' @param p Integer vector of dimensions to evaluate. Default is
#'   \code{c(2, 5)}.
#' @param distributions Character vector of distribution names.
#'   Available options:
#'   \describe{
#'     \item{\code{"normal"}}{Multivariate normal \eqn{N(0, \Sigma)}.
#'       All moments finite. Baseline case.}
#'     \item{\code{"t5"}}{Elliptical multivariate t with 5 degrees of
#'       freedom, scaled to covariance \eqn{\Sigma}. Symmetric,
#'       heavy-tailed, finite fourth moment (\eqn{4 < 5}).}
#'     \item{\code{"gamma2"}}{Independent Gamma(2, 1) margins,
#'       centered and scaled to covariance \eqn{\Sigma}. Asymmetric,
#'       all moments finite.}
#'     \item{\code{"t4"}}{Elliptical multivariate t with 4 degrees of
#'       freedom (stress test). Has infinite fourth moment, violating the
#'       formal condition of Theorem 3.}
#'   }
#' @param nsim Number of Monte Carlo replications. A single positive
#'   integer. Default is 10000.
#' @param sig_levels Numeric vector of quantile levels in \eqn{(0, 1)}
#'   to compare. Default is \code{c(0.90, 0.95, 0.99)}.
#' @param seed Random seed for reproducibility. A single integer.
#'   Default is \code{42}.
#' @param rho Correlation parameter for the equicorrelation matrix
#'   \eqn{\Sigma_{ij} = 1} if \eqn{i = j}, \eqn{\rho} otherwise.
#'   Must satisfy \eqn{\rho > -1/(p-1)} for each \eqn{p} to ensure
#'   positive definiteness. Default is \code{0.3}.
#' @return A data frame with columns:
#'   \describe{
#'     \item{\code{n}}{Sample size.}
#'     \item{\code{p}}{Dimension.}
#'     \item{\code{distribution}}{Distribution name.}
#'     \item{\code{level}}{Quantile level (e.g., 0.95).}
#'     \item{\code{empirical}}{Empirical quantile of simulated T².}
#'     \item{\code{chisq}}{Theoretical \eqn{\chi^2_p} quantile.}
#'     \item{\code{mcse}}{Monte Carlo standard error of the
#'       empirical quantile.}
#'     \item{\code{nsim}}{Number of valid replications (after
#'       discarding singular covariance draws).}
#'   }
#' @section RNG preservation:
#'   The function saves and restores \code{.Random.seed} on exit, so it
#'   does not alter the global RNG state.
#' @section Monte Carlo standard error:
#'   The MCSE of a sample quantile at level \eqn{q} is approximated as
#'   \eqn{\sqrt{q(1-q) / (N f(q)^2)}} where \eqn{f} is the \eqn{\chi^2_p}
#'   density. This approximation assumes the density of the T² statistic is
#'   close to \eqn{\chi^2_p}, which is reasonable when the asymptotic
#'   approximation holds. It is not a substitute for the true standard error.
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
  if(!is.numeric(n) || length(n) < 1 || any(!is.finite(n)) ||
     any(n != as.integer(n)) || any(n < 1))
    stop("n must be a vector of positive integers")

  if(!is.numeric(p) || length(p) < 1 || any(!is.finite(p)) ||
     any(p != as.integer(p)) || any(p < 1))
    stop("p must be a vector of positive integers")

  if(!is.numeric(nsim) || length(nsim) != 1 || !is.finite(nsim) ||
     nsim < 1 || nsim != as.integer(nsim))
    stop("nsim must be a single positive integer")

  if(!is.numeric(sig_levels) || length(sig_levels) < 1 ||
     any(!is.finite(sig_levels)) || any(sig_levels <= 0 | sig_levels >= 1))
    stop("sig_levels must be a numeric vector with all entries in (0, 1)")

  if(!is.numeric(seed) || length(seed) != 1 || !is.finite(seed) ||
     seed != as.integer(seed))
    stop("seed must be a single integer")

  if(!is.numeric(rho) || length(rho) != 1 || !is.finite(rho) || rho <= -1 || rho >= 1)
    stop("rho must be a finite scalar in (-1, 1)")

  for (dim_p in p) {
    if (rho <= -1 / (dim_p - 1))
      stop(sprintf("rho must be > -1/(p-1) = %.4f for p = %d; got rho = %.4f",
                   -1 / (dim_p - 1), dim_p, rho))
  }

  for (dim_p in p) {
    for (sample_size in n) {
      if (sample_size <= dim_p)
        stop(sprintf("n must be greater than p for all combinations; got n = %d, p = %d",
                     sample_size, dim_p))
    }
  }

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

  # ── Build equicorrelation matrix ──────────────────────────────────────────
  build_sigma <- function(p, rho) {
    sigma <- matrix(rho, nrow = p, ncol = p)
    diag(sigma) <- 1
    sigma
  }

  # ── Distribution samplers ─────────────────────────────────────────────────
  # All return an n x p matrix with mean 0 and covariance Sigma

  rnormal <- function(n, p, sigma) {
    MASS::mvrnorm(n, mu = rep(0, p), Sigma = sigma)
  }

  rt_elliptical <- function(n, p, sigma, df) {
    # Elliptical multivariate t: X = Z / sqrt(U/df)
    # where Z ~ N(0, Sigma), U ~ chi^2_df, independent.
    # Cov(X) = Sigma * df/(df-2) for df > 2.
    # We scale to get Cov(X) = Sigma.
    scale_mat <- sigma * (df - 2) / df
    chol_scale <- chol(scale_mat)
    z <- matrix(rnorm(n * p), nrow = n, ncol = p) %*% chol_scale
    u <- rchisq(n, df = df)
    z / sqrt(u / df)
  }

  rt5 <- function(n, p, sigma) {
    rt_elliptical(n, p, sigma, df = 5)
  }

  rt4 <- function(n, p, sigma) {
    rt_elliptical(n, p, sigma, df = 4)
  }

  rgamma2 <- function(n, p, sigma) {
    # Independent Gamma(2, 1) margins, then centered and scaled.
    # Gamma(2,1) has mean 2, variance 2.
    x <- matrix(rgamma(n * p, shape = 2, rate = 1), nrow = n, ncol = p)
    x <- sweep(x, 2, 2, FUN = "-")
    x <- sweep(x, 2, sqrt(2), FUN = "/")
    chol_sigma <- chol(sigma)
    x %*% chol_sigma
  }

  # ── Single T² computation ─────────────────────────────────────────────────
  compute_t2 <- function(x) {
    n_obs <- nrow(x)
    xbar <- colMeans(x)
    s <- cov(x)
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
        t2_vals <- replicate(nsim, {
          x <- sampler(sample_size, dim_p, sigma)
          compute_t2(x)
        })

        t2_vals <- t2_vals[!is.na(t2_vals)]
        n_valid <- length(t2_vals)

        chisq_quants <- qchisq(sig_levels, df = dim_p)
        emp_quants <- quantile(t2_vals, probs = sig_levels, names = FALSE)

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
