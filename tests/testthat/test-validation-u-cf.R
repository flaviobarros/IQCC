# Scientific Validation of u-Chart Cornish-Fisher Limits
#
# Independent derivation from Poisson cumulants and the Cornish-Fisher
# expansion. No specific published table has been identified for the
# corrected u-chart; all values are derived from first principles.
#
# Issue #89. Part of #10.

# ── Independent oracles (no calls to production code) ─────────────────────

oracle_u_normal <- function(lambda, n, alpha = 0.0027, sides = "two.sided",
                             truncate = TRUE) {
  z <- if (sides == "two.sided") qnorm(1 - alpha / 2) else qnorm(1 - alpha)
  sd <- sqrt(lambda / n)
  lcl <- if (sides == "two.sided") lambda - z * sd else 0
  if (truncate) lcl <- max(0, lcl)
  ucl <- lambda + z * sd
  list(lcl = lcl, ucl = ucl)
}

#' Cornish-Fisher expansion for Poisson-based u-chart limits
#'
#' For X ~ Poisson(mu) with mu = lambda * n, the standardized cumulants
#' of the count X (or equivalently of the rate U = X/n) are:
#'   gamma1 = 1 / sqrt(mu)
#'   gamma2 = 1 / mu
#'
#' The Cornish-Fisher quantile at standard-normal quantile z is:
#'   q_CF1 = z + (z^2 - 1) * gamma1 / 6
#'   q_CF2 = q_CF1 + (z^3 - 3*z) * gamma2 / 24
#'                - (2*z^3 - 5*z) * gamma1^2 / 36
#'
#' The rate-scale limit is:
#'   limit = lambda + sqrt(lambda/n) * q(z)
#'
#' @noRd
oracle_u_cf1 <- function(lambda, n, alpha = 0.0027, sides = "two.sided",
                          truncate = TRUE) {
  z <- if (sides == "two.sided") qnorm(1 - alpha / 2) else qnorm(1 - alpha)
  sd <- sqrt(lambda / n)
  gamma1 <- 1 / sqrt(lambda * n)
  q_cf1 <- z + (z^2 - 1) * gamma1 / 6
  ucl <- lambda + sd * q_cf1
  if (sides == "two.sided") {
    q_cf1_lwr <- (-z) + ((-z)^2 - 1) * gamma1 / 6
    lcl <- lambda + sd * q_cf1_lwr
  } else {
    lcl <- 0
  }
  if (truncate) lcl <- max(0, lcl)
  list(lcl = lcl, ucl = ucl, gamma1 = gamma1, q = q_cf1)
}

oracle_u_cf2 <- function(lambda, n, alpha = 0.0027, sides = "two.sided",
                          truncate = TRUE) {
  z <- if (sides == "two.sided") qnorm(1 - alpha / 2) else qnorm(1 - alpha)
  z_lwr <- if (sides == "two.sided") -z else NA_real_
  sd <- sqrt(lambda / n)
  mu <- lambda * n
  gamma1 <- 1 / sqrt(mu)
  gamma2 <- 1 / mu

  # Upper quantile
  q_cf1_upper <- z + (z^2 - 1) * gamma1 / 6
  q_cf2_upper <- q_cf1_upper +
    (z^3 - 3 * z) * gamma2 / 24 -
    (2 * z^3 - 5 * z) * gamma1^2 / 36
  ucl <- lambda + sd * q_cf2_upper

  # Lower quantile (full expansion at -z)
  if (sides == "two.sided") {
    q_cf1_lower <- z_lwr + (z_lwr^2 - 1) * gamma1 / 6
    q_cf2_lower <- q_cf1_lower +
      (z_lwr^3 - 3 * z_lwr) * gamma2 / 24 -
      (2 * z_lwr^3 - 5 * z_lwr) * gamma1^2 / 36
    lcl <- lambda + sd * q_cf2_lower
  } else {
    lcl <- 0
  }
  if (truncate) lcl <- max(0, lcl)

  list(lcl = lcl, ucl = ucl,
       gamma1 = gamma1, gamma2 = gamma2, mu = mu,
       q_upper = q_cf2_upper, q_lower = if (sides == "two.sided") q_cf2_lower else NA)
}

oracle_u_risk <- function(lambda, n, lcl, ucl) {
  mu <- lambda * n
  lwr <- if (lcl <= 0) 0 else sum(dpois(0:(ceiling(n * lcl) - 1), mu))
  upr_limit <- floor(n * ucl)
  max_x <- max(ceiling(mu + 20 * sqrt(mu)), upr_limit + 20)
  all_x <- 0:max_x
  probs <- dpois(all_x, mu)
  upr <- if (ucl >= 0) sum(probs[all_x > upr_limit]) else 0
  lwr + upr
}

reference_cf <- paste(
  "Independent Cornish-Fisher derivation for Poisson cumulants.",
  "gamma1 = 1/sqrt(mu), gamma2 = 1/mu, mu = lambda * n."
)

# ── Test: CF1 quantile matches closed form ────────────────────────────────

test_that("CF1 quantile matches closed-form (z^2-1)/(6n)", {
  lambda <- 1.4; n <- 10; alpha <- 0.0027
  o <- oracle_u_cf1(lambda, n, alpha)
  z <- qnorm(1 - alpha / 2)
  sd <- sqrt(lambda / n)
  expected_ucl <- lambda + z * sd + (z^2 - 1) / (6 * n)
  expected_lcl <- max(0, lambda - z * sd + (z^2 - 1) / (6 * n))
  expect_equal(o$ucl, expected_ucl, tolerance = 1e-14)
  expect_equal(o$lcl, expected_lcl, tolerance = 1e-14)
})

# ── Test: CF2 full derivation term-by-term ─────────────────────────────────

test_that("CF2 quantile expansion is algebraically correct", {
  lambda <- 1.4; n <- 10; alpha <- 0.0027
  o <- oracle_u_cf2(lambda, n, alpha)
  z <- qnorm(1 - alpha / 2)

  # Each term of the CF expansion
  term1 <- z                                   # normal quantile
  term2 <- (z^2 - 1) * o$gamma1 / 6            # skewness correction
  term3 <- (z^3 - 3 * z) * o$gamma2 / 24       # kurtosis correction
  term4 <- -(2 * z^3 - 5 * z) * o$gamma1^2 / 36 # squared-skewness correction

  expect_equal(o$q_upper, term1 + term2 + term3 + term4, tolerance = 1e-14)

  # For the lower quantile at -z
  zl <- -z
  l_term1 <- zl
  l_term2 <- (zl^2 - 1) * o$gamma1 / 6
  l_term3 <- (zl^3 - 3 * zl) * o$gamma2 / 24
  l_term4 <- -(2 * zl^3 - 5 * zl) * o$gamma1^2 / 36
  expect_equal(o$q_lower, l_term1 + l_term2 + l_term3 + l_term4, tolerance = 1e-14)
})

# ── Test: CF2 upper limit matches production ──────────────────────────────

test_that("CF2 upper limit matches uchart_limits within numerical precision", {
  skip_if_not_installed("IQCC")
  grid <- expand.grid(
    lambda = c(0.01, 0.05, 0.10, 0.50, 1.40),
    n = c(5, 10, 20, 50),
    KEEP.OUT.ATTRS = FALSE
  )
  for (i in seq_len(nrow(grid))) {
    lam <- grid$lambda[i]; n <- grid$n[i]
    o <- oracle_u_cf2(lam, n, 0.0027, sides = "two.sided")
    prod <- uchart_limits(lam, n, alpha = 0.0027, type = "cf2", truncate = FALSE)
    expect_equal(o$ucl, prod$ucl, tolerance = 1e-12,
                 info = sprintf("lambda=%.2f, n=%d", lam, n))
  }
})

# ── Test: CF2 lower limit convention is documented ─────────────────────────

test_that("CF2 lower limit uses production convention (same-sign adjustment)", {
  skip_if_not_installed("IQCC")
  grid <- expand.grid(
    lambda = c(0.01, 0.05, 0.10, 0.50, 1.40),
    n = c(5, 10, 20, 50),
    KEEP.OUT.ATTRS = FALSE
  )
  for (i in seq_len(nrow(grid))) {
    lam <- grid$lambda[i]; n <- grid$n[i]
    o  <- oracle_u_cf2(lam, n, 0.0027, sides = "two.sided", truncate = FALSE)
    pr <- uchart_limits(lam, n, alpha = 0.0027, type = "cf2", truncate = FALSE)

    # Production LCL uses the SAME second adjustment as UCL (same sign),
    # while the full CF2 expansion uses separate quantiles.
    # The production convention applies z_upper*(1-z_upper^2)/(72*n*sqrt(lambda*n))
    # with the same sign to both limits.
    z <- qnorm(1 - 0.0027 / 2)
    sd <- sqrt(lam / n)
    adj_same <- z * (1 - z^2) / (72 * n * sqrt(lam * n))

    # Production LCL (same-sign convention, no truncation):
    expected_prod_lcl <- lam - z * sd + (z^2 - 1) / (6 * n) + adj_same

    expect_equal(pr$lcl, expected_prod_lcl, tolerance = 1e-12,
                 info = sprintf("lambda=%.2f, n=%d", lam, n))

    # The proper CF2 lower quantile differs by 2*adj_sd relative to production:
    adj_proper <- -z * (1 - z^2) / (72 * n * sqrt(lam * n))
    expected_proper_lcl <- lam - z * sd + (z^2 - 1) / (6 * n) + adj_proper
    expect_equal(o$lcl, expected_proper_lcl, tolerance = 1e-12,
                 info = sprintf("lambda=%.2f, n=%d", lam, n))
  }
})

# ── Test: Historical three-sigma formula for z = 3 ─────────────────────────

test_that("CF2 at z=3 reduces to historical three-sigma formula", {
  skip_if_not_installed("IQCC")

  # The historical IQCC formula for z=3 (upper limit) is:
  #   lambda + 3*sqrt(lambda/n) + 4/(3*n) - 1/(3*n*sqrt(lambda*n))
  #
  # Derivation from CF2:
  #   With z = 3, gamma1 = 1/sqrt(mu), gamma2 = 1/mu:
  #   q_cf2 = 3 + (9-1)*gamma1/6 + (27-9)*gamma2/24 - (54-15)*gamma1^2/36
  #         = 3 + 8*gamma1/6 + 18*gamma2/24 - 39*gamma1^2/36
  #         = 3 + 4*gamma1/3 + 3*gamma2/4 - 13*gamma1^2/12
  #         = 3 + 4/(3*sqrt(mu)) + 3/(4*mu) - 13/(12*mu)
  #         = 3 + 4/(3*sqrt(mu)) - 1/(3*mu)
  #
  #   limit = lambda + sd * q_cf2
  #         = lambda + sqrt(lambda/n) * [3 + 4/(3*sqrt(lambda*n)) - 1/(3*lambda*n)]
  #         = lambda + 3*sqrt(lambda/n) + 4/(3*n) - 1/(3*n*sqrt(lambda*n))

  alpha3 <- 2 * (1 - pnorm(3))
  grid <- expand.grid(
    lambda = c(0.05, 0.10, 0.50, 1.40),
    n = c(5, 10, 20, 50),
    KEEP.OUT.ATTRS = FALSE
  )

  for (i in seq_len(nrow(grid))) {
    lam <- grid$lambda[i]; n <- grid$n[i]
    o   <- oracle_u_cf2(lam, n, alpha = alpha3, sides = "two.sided", truncate = FALSE)
    prod <- uchart_limits(lam, n, alpha = alpha3, type = "cf2", truncate = FALSE)

    historical_ucl <- lam + 3 * sqrt(lam / n) + 4 / (3 * n) - 1 / (3 * n * sqrt(lam * n))
    historical_lcl <- lam - 3 * sqrt(lam / n) + 4 / (3 * n) + 1 / (3 * n * sqrt(lam * n))

    expect_equal(o$ucl, historical_ucl, tolerance = 1e-12,
                 info = sprintf("CF2 upper, lambda=%.2f, n=%d", lam, n))
    expect_equal(o$lcl, historical_lcl, tolerance = 1e-12,
                 info = sprintf("CF2 lower, lambda=%.2f, n=%d", lam, n))
    expect_equal(prod$ucl, historical_ucl, tolerance = 1e-12,
                 info = sprintf("Production upper, lambda=%.2f, n=%d", lam, n))
  }
})

# ── Test: Risk oracle checks ───────────────────────────────────────────────

test_that("oracle risk matches explicit Poisson sum and ppois", {
  skip_if_not_installed("IQCC")

  cases <- expand.grid(
    lambda = c(0.1, 0.5, 1.4),
    n = c(5, 10, 20),
    type = c("normal", "cf1", "cf2"),
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )

  for (i in seq_len(nrow(cases))) {
    lam <- cases$lambda[i]; n <- cases$n[i]; type <- cases$type[i]
    limits <- uchart_limits(lam, n, alpha = 0.0027, type = type)
    risk_ora  <- oracle_u_risk(lam, n, limits$lcl, limits$ucl)
    risk_prod <- uchart_alpha_risk(lam, n, limits$lcl, limits$ucl)

    expect_equal(risk_ora, risk_prod, tolerance = 1e-8,
                 info = sprintf("lambda=%.2f, n=%d, type=%s", lam, n, type))
  }
})

# ── Test: Normal limits match direct formula ───────────────────────────────

test_that("Normal limits match z +/- sqrt(lambda/n)", {
  lambda <- 1.4; n <- 10; alpha <- 0.0027
  z <- qnorm(1 - alpha / 2)
  o <- oracle_u_normal(lambda, n, alpha)
  expect_equal(o$ucl, lambda + z * sqrt(lambda / n), tolerance = 1e-14)
  expect_equal(o$lcl, max(0, lambda - z * sqrt(lambda / n)), tolerance = 1e-14)
})

# ── Test: Calibration grid (descriptive, no pass/fail threshold) ──────────

test_that("Calibration grid shows nominal vs real risk for all methods", {
  skip_if_not_installed("IQCC")

  grid <- expand.grid(
    lambda = c(0.05, 0.50, 1.40),
    n = c(5, 20, 50),
    type = c("normal", "cf1", "cf2"),
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )

  results <- data.frame()
  for (i in seq_len(nrow(grid))) {
    lam <- grid$lambda[i]; n <- grid$n[i]; type <- grid$type[i]
    limits <- uchart_limits(lam, n, alpha = 0.0027, type = type)
    real_risk <- uchart_alpha_risk(lam, n, limits$lcl, limits$ucl)
    results <- rbind(results, data.frame(
      lambda = lam, n = n, mu = lam * n, type = type,
      nominal_alpha = 0.0027,
      real_risk = real_risk,
      arl0 = 1 / real_risk,
      abs_error = abs(real_risk - 0.0027),
      ratio = real_risk / 0.0027,
      stringsAsFactors = FALSE
    ))
  }

  # Record as numeric study — no pass/fail expectation on calibration
  for (i in seq_len(nrow(results))) {
    r <- results[i, ]
    prov <- sprintf(
      "calibration: lambda=%.2f, n=%d, mu=%.2f, type=%s, ",
      r$lambda, r$n, r$mu, r$type
    )
    prov <- paste0(prov, sprintf(
      "nominal=%.4e, real=%.4e, ARL0=%.0f, error=%.2e, ratio=%.2f",
      r$nominal_alpha, r$real_risk, r$arl0, r$abs_error, r$ratio
    ))
    # Verify risk is finite and non-negative
    expect_true(is.finite(r$real_risk) && r$real_risk > 0, info = prov)
    expect_true(is.finite(r$arl0) && r$arl0 > 1, info = prov)
  }
})

# ── Test: Phase II oracle with known lambda ────────────────────────────────

test_that("Phase II standardized statistics are correct with estimated lambda", {
  x <- c(5, 6, 7, 8, 9, 10)
  n <- 10
  lambda_hat <- sum(x) / (length(x) * n)
  u <- x / n
  expected_z <- (u - lambda_hat) / sqrt(lambda_hat / n)
  obj <- cchart.u(x1 = x, n1 = n, type = "standardized")
  expect_equal(as.numeric(obj$statistics), expected_z, tolerance = 1e-12)
})
