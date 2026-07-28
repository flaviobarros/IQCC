# Hardening tests for the Hotelling T² simulation
#
# Issue #97. Complements the scientific study from #83.

test_that("elliptical t uses one radial scale per observation", {
  sigma <- matrix(c(1, 0.4, 0.4, 1), nrow = 2)
  n <- 8
  p <- 2
  df <- 5

  set.seed(123)
  observed <- .rt_elliptical_cov(n, p, sigma, df)

  set.seed(123)
  scale_mat <- sigma * (df - 2) / df
  gaussian <- matrix(rnorm(n * p), nrow = n, ncol = p) %*%
    chol(scale_mat)
  radial_scale <- sqrt(rchisq(n, df = df) / df)
  expected <- gaussian / radial_scale

  expect_equal(observed, expected, tolerance = 0)
})

test_that("elliptical t is scaled to the requested covariance", {
  sigma <- matrix(c(1, 0.35, 0.35, 1), nrow = 2)

  set.seed(42)
  sample <- .rt_elliptical_cov(
    n = 40000,
    p = 2,
    sigma = sigma,
    df = 5
  )

  expect_equal(cov(sample), sigma, tolerance = 0.05)
})

test_that("empirical quantile MCSE is finite and scale equivariant", {
  set.seed(42)
  x <- rchisq(5000, df = 3)
  probs <- c(0.90, 0.95, 0.99)

  mcse <- .quantile_mcse_spacing(x, probs)
  scaled_mcse <- .quantile_mcse_spacing(2 * x, probs)

  expect_true(all(is.finite(mcse)))
  expect_true(all(mcse > 0))
  expect_equal(scaled_mcse, 2 * mcse, tolerance = 1e-12)
})

test_that("simulated Hotelling quantiles report empirical MCSE", {
  result <- sim_t2_asymptotic(
    n = 100,
    p = 2,
    distributions = c("normal", "t5"),
    nsim = 500,
    seed = 321
  )

  expect_true(all(is.finite(result$mcse)))
  expect_true(all(result$mcse > 0))
  expect_equal(result$nsim, rep(500, nrow(result)))
})
