# Tests for asymptotic robustness of Hotelling T²
#
# Issue #68: Document and validate asymptotic robustness of Hotelling T²
# Reference: Gneri & Barbosa (2006), Seccion 4.2, Teorema 3

# ── Design contract ──────────────────────────────────────────────────────────

test_that("sim_t2_asymptotic validates n parameter", {
  expect_error(sim_t2_asymptotic(n = 0, nsim = 100), "positive integers")
  expect_error(sim_t2_asymptotic(n = -5, nsim = 100), "positive integers")
  expect_error(sim_t2_asymptotic(n = 2.5, nsim = 100), "positive integers")
})

test_that("sim_t2_asymptotic validates p parameter", {
  expect_error(sim_t2_asymptotic(p = 0, nsim = 100), "positive integers")
  expect_error(sim_t2_asymptotic(p = -1, nsim = 100), "positive integers")
})

test_that("sim_t2_asymptotic validates rho parameter", {
  expect_error(sim_t2_asymptotic(rho = -1, nsim = 100), "in \\(-1, 1\\)")
  expect_error(sim_t2_asymptotic(rho = 1, nsim = 100), "in \\(-1, 1\\)")
  expect_error(sim_t2_asymptotic(rho = 2, nsim = 100), "in \\(-1, 1\\)")
})

test_that("sim_t2_asymptotic validates distributions", {
  expect_error(sim_t2_asymptotic(distributions = "invalid", nsim = 100),
               "should be one of")
})

# ── Returns expected structure ───────────────────────────────────────────────

test_that("sim_t2_asymptotic returns a data frame with expected columns", {
  res <- sim_t2_asymptotic(
    n = 30,
    p = 2,
    distributions = "normal",
    nsim = 500,
    seed = 42
  )

  expect_s3_class(res, "data.frame")
  expect_named(res, c("n", "p", "distribution", "level",
                       "empirical", "chisq", "mcse", "nsim"))
  expect_equal(nrow(res), 3)  # 3 sig_levels
})

test_that("sim_t2_asymptotic runs for multiple distributions", {
  res <- sim_t2_asymptotic(
    n = 100,
    p = 2,
    distributions = c("normal", "t5", "gamma2"),
    nsim = 200,
    seed = 42
  )

  expect_equal(nrow(res), 9)  # 3 dists × 3 levels
  expect_setequal(unique(res$distribution), c("normal", "t5", "gamma2"))
})

test_that("sim_t2_asymptotic runs for multiple n and p", {
  res <- sim_t2_asymptotic(
    n = c(30, 100),
    p = c(2, 5),
    distributions = "normal",
    nsim = 200,
    seed = 42
  )

  expect_equal(nrow(res), 12)  # 2 n × 2 p × 3 levels
  expect_setequal(unique(res$n), c(30, 100))
  expect_setequal(unique(res$p), c(2, 5))
})

# ── RNG preservation ─────────────────────────────────────────────────────────

test_that("sim_t2_asymptotic preserves RNG state", {
  set.seed(123)
  before <- .Random.seed

  res <- sim_t2_asymptotic(n = 30, p = 2, nsim = 100, seed = 42)

  after <- .Random.seed
  expect_equal(before, after)
})

test_that("sim_t2_asymptotic is reproducible with same seed", {
  res1 <- sim_t2_asymptotic(n = 30, p = 2, nsim = 100, seed = 42)
  res2 <- sim_t2_asymptotic(n = 30, p = 2, nsim = 100, seed = 42)

  expect_equal(res1, res2)
})

test_that("sim_t2_asymptotic changes with different seed", {
  res1 <- sim_t2_asymptotic(n = 30, p = 2, nsim = 100, seed = 42)
  res2 <- sim_t2_asymptotic(n = 30, p = 2, nsim = 100, seed = 123)

  expect_false(isTRUE(all.equal(res1$empirical, res2$empirical)))
})

# ── Numerical correctness (normal distribution baseline) ────────────────────

test_that("normal distribution approximates chi-squared for large n", {
  res <- sim_t2_asymptotic(
    n = 500,
    p = 2,
    distributions = "normal",
    nsim = 2000,
    seed = 42
  )

  # For n=500, p=2, normal distribution, empirical quantiles should
  # be close to chi-squared quantiles. Allow 2 MCSE tolerance.
  for (i in seq_len(nrow(res))) {
    tol <- 3 * res$mcse[i]
    expect_equal(res$empirical[i], res$chisq[i],
                 tolerance = tol,
                 info = sprintf("level=%.2f: emp=%.3f chisq=%.3f mcse=%.3f",
                                res$level[i], res$empirical[i],
                                res$chisq[i], res$mcse[i]))
  }
})

# ── t5 distribution (finite 4th moment) ─────────────────────────────────────

test_that("t5 distribution shows convergence for large n", {
  res <- sim_t2_asymptotic(
    n = 500,
    p = 2,
    distributions = "t5",
    nsim = 2000,
    seed = 42
  )

  # t5 has finite 4th moment, should converge. Use wider tolerance
  # due to heavier tails slowing convergence.
  for (i in seq_len(nrow(res))) {
    tol <- 5 * res$mcse[i]
    expect_equal(res$empirical[i], res$chisq[i],
                 tolerance = tol,
                 info = sprintf("level=%.2f: emp=%.3f chisq=%.3f mcse=%.3f",
                                res$level[i], res$empirical[i],
                                res$chisq[i], res$mcse[i]))
  }
})

# ── All values non-negative ──────────────────────────────────────────────────

test_that("all T² values are non-negative", {
  # Test uses direct computation via T2.1
  set.seed(42)
  datum <- data.1(50, 1, c(0, 0), diag(2))
  estat <- stats(datum, 50, 1, 2)
  t2_vals <- T2.1(estat, 50, 1)
  expect_true(all(t2_vals >= 0))
})
