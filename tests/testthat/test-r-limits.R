# Tests for r_exact_limits and r_shewhart_limits
#
# Issue #64: Extract pure numerical functions for the R chart limits.
# Reference: Barbosa, Gneri, and Meneguetti (2013),
#   "Range control charts revisited", doi:10.1080/03610918.2011.639967

# ── Design contract ──────────────────────────────────────────────────────────

test_that("r_exact_limits validates sigma", {
  expect_error(r_exact_limits(sigma = NA_real_, n = 5), "finite positive scalar")
  expect_error(r_exact_limits(sigma = NaN, n = 5), "finite positive scalar")
  expect_error(r_exact_limits(sigma = Inf, n = 5), "finite positive scalar")
  expect_error(r_exact_limits(sigma = -1, n = 5), "finite positive scalar")
  expect_error(r_exact_limits(sigma = 0, n = 5), "finite positive scalar")
  expect_error(r_exact_limits(sigma = "a", n = 5), "finite positive scalar")
})

test_that("r_exact_limits validates n", {
  expect_error(r_exact_limits(sigma = 1, n = 1), "greater than or equal to 2")
  expect_error(r_exact_limits(sigma = 1, n = 0), "greater than or equal to 2")
  expect_error(r_exact_limits(sigma = 1, n = 2.5), "integer")
  expect_error(r_exact_limits(sigma = 1, n = NA), "integer")
  expect_error(r_exact_limits(sigma = 1, n = Inf), "integer")
})

test_that("r_exact_limits validates alpha", {
  expect_error(r_exact_limits(sigma = 1, n = 5, alpha = 0), "between 0 and 1")
  expect_error(r_exact_limits(sigma = 1, n = 5, alpha = 1), "between 0 and 1")
  expect_error(r_exact_limits(sigma = 1, n = 5, alpha = -0.1), "between 0 and 1")
  expect_error(r_exact_limits(sigma = 1, n = 5, alpha = NA), "between 0 and 1")
  expect_error(r_exact_limits(sigma = 1, n = 5, alpha = Inf), "between 0 and 1")
})

test_that("r_shewhart_limits validates sigma", {
  expect_error(r_shewhart_limits(sigma = NA_real_, n = 5), "finite positive scalar")
  expect_error(r_shewhart_limits(sigma = NaN, n = 5), "finite positive scalar")
  expect_error(r_shewhart_limits(sigma = Inf, n = 5), "finite positive scalar")
  expect_error(r_shewhart_limits(sigma = -1, n = 5), "finite positive scalar")
  expect_error(r_shewhart_limits(sigma = 0, n = 5), "finite positive scalar")
})

test_that("r_shewhart_limits validates n", {
  expect_error(r_shewhart_limits(sigma = 1, n = 1), "greater than or equal to 2")
  expect_error(r_shewhart_limits(sigma = 1, n = 0), "greater than or equal to 2")
  expect_error(r_shewhart_limits(sigma = 1, n = 2.5), "integer")
})

test_that("r_shewhart_limits validates nsigmas", {
  expect_error(r_shewhart_limits(sigma = 1, n = 5, nsigmas = 0), "positive scalar")
  expect_error(r_shewhart_limits(sigma = 1, n = 5, nsigmas = -1), "positive scalar")
  expect_error(r_shewhart_limits(sigma = 1, n = 5, nsigmas = NA), "positive scalar")
})

# ── Numerical correctness ────────────────────────────────────────────────────

test_that("r_exact_limits reproduces Table 2 published quantiles", {
  reference <- paste(
    "Barbosa, Gneri, and Meneguetti (2013), Table 2, row n = 5",
    "doi:10.1080/03610918.2011.639967"
  )

  lims <- r_exact_limits(sigma = 1, n = 5, alpha = 0.0027)

  # Published values from Table 2 for n = 5:
  #   q(0.00135) = 0.39653  (LCL / sigma)
  #   q(0.99865) = 5.37740  (UCL / sigma)
  # Table 2 prints five decimal places; tolerance is half a unit in the last place.
  published_lcl <- 0.39653
  published_ucl <- 5.37740
  tolerance <- 5e-5

  expect_equal(lims$lcl, published_lcl, tolerance = tolerance,
               info = paste(reference, "LCL = q(0.00135)"))
  expect_equal(lims$ucl, published_ucl, tolerance = tolerance,
               info = paste(reference, "UCL = q(0.99865)"))
})

test_that("r_exact_limits center is d2(n) * sigma", {
  lims <- r_exact_limits(sigma = 2, n = 5)
  expect_equal(lims$center, d2(5) * 2)
})

test_that("r_exact_limits scales linearly with sigma", {
  lims1 <- r_exact_limits(sigma = 1, n = 5)
  lims2 <- r_exact_limits(sigma = 3, n = 5)
  expect_equal(lims2$lcl, 3 * lims1$lcl)
  expect_equal(lims2$ucl, 3 * lims1$ucl)
  expect_equal(lims2$center, 3 * lims1$center)
})

test_that("r_shewhart_limits uses d2 and d3 correctly", {
  sigma <- 1.5
  n <- 7
  lims <- r_shewhart_limits(sigma = sigma, n = n)
  expected_lcl <- max(0, d2(n) - 3 * d3(n)) * sigma
  expected_ucl <- (d2(n) + 3 * d3(n)) * sigma
  expected_center <- d2(n) * sigma
  expect_equal(lims$lcl, expected_lcl)
  expect_equal(lims$ucl, expected_ucl)
  expect_equal(lims$center, expected_center)
})

test_that("r_shewhart_limits accepts custom nsigmas", {
  lims3 <- r_shewhart_limits(sigma = 1, n = 5, nsigmas = 3)
  lims2 <- r_shewhart_limits(sigma = 1, n = 5, nsigmas = 2)
  expect_true(lims3$ucl > lims2$ucl)
  d2_5 <- d2(5)
  d3_5 <- d3(5)
  expect_equal(lims2$lcl, max(0, d2_5 - 2 * d3_5))
  expect_equal(lims2$ucl, d2_5 + 2 * d3_5)
})

test_that("r_shewhart_limits scales linearly with sigma", {
  lims1 <- r_shewhart_limits(sigma = 1, n = 5)
  lims2 <- r_shewhart_limits(sigma = 4, n = 5)
  expect_equal(lims2$lcl, 4 * lims1$lcl)
  expect_equal(lims2$ucl, 4 * lims1$ucl)
  expect_equal(lims2$center, 4 * lims1$center)
})

test_that("r_shewhart_limits lower limit is zero-truncated", {
  # For n=2, d2(2) - 3*d3(2) is negative, so LCL should be 0
  lims <- r_shewhart_limits(sigma = 1, n = 2)
  expect_equal(lims$lcl, 0)
})

# ── Returns structure ────────────────────────────────────────────────────────

test_that("r_exact_limits returns a named list with expected components", {
  lims <- r_exact_limits(sigma = 1, n = 5)
  expect_type(lims, "list")
  expect_named(lims, c("lcl", "ucl", "center", "sigma", "n", "alpha"))
  expect_true(is.numeric(lims$lcl))
  expect_true(is.numeric(lims$ucl))
  expect_true(is.numeric(lims$center))
})

test_that("r_shewhart_limits returns a named list with expected components", {
  lims <- r_shewhart_limits(sigma = 1, n = 5)
  expect_type(lims, "list")
  expect_named(lims, c("lcl", "ucl", "center", "nsigmas", "sigma", "n"))
  expect_true(is.numeric(lims$lcl))
  expect_true(is.numeric(lims$ucl))
  expect_true(is.numeric(lims$center))
})

# ── Consistency with the qcc wrapper ─────────────────────────────────────────

test_that("exact limits computed by cchart.R match r_exact_limits", {
  data(pistonrings, package = "IQCC")

  # Build the chart with tukey method. We need to capture the limits from the
  # qcc object and verify they match the pure function.
  sigma_hat <- qcc::sd.R(pistonrings[1:25, ])
  pure_lims <- r_exact_limits(sigma = sigma_hat, n = 5)

  suppressMessages({
    chart <- cchart.R(
      pistonrings[26:40, ],
      5,
      type = "tukey",
      y = pistonrings[1:25, ]
    )
  })

  # qcc stores limits as a 1×2 matrix; column names may have irregular
  # trailing spaces, so access by position rather than name.
  expect_equal(chart$limits[1, 1L], pure_lims$lcl)
  expect_equal(chart$limits[1, 2L], pure_lims$ucl)
})

# ── False-alarm risk (Table 1a context) ──────────────────────────────────────

test_that("alpha.risk values are consistent with r_shewhart_limits", {
  # alpha.risk computes the actual false-alarm probability of the Shewhart
  # three-sigma limits. Verify that the risk corresponds to the limits returned
  # by r_shewhart_limits.
  for (n in c(2, 3, 5, 10, 15)) {
    lims <- r_shewhart_limits(sigma = 1, n = n)
    computed_risk <- 1 - (stats::ptukey(lims$ucl, n, Inf) -
                          stats::ptukey(lims$lcl, n, Inf))
    expect_equal(computed_risk, alpha.risk(n), tolerance = 1e-12,
                 info = sprintf("alpha.risk consistency failed for n = %d", n))
  }
})

test_that("alpha.risk shows inflated false-alarm probability for small n", {
  # For n=2, the paper reports ARL(0) = 109 instead of 370, implying
  # alpha = 1/109 ~ 0.00917. Our computed value is 0.00915.
  risk_2 <- alpha.risk(2)
  expect_true(risk_2 > 0.009)
  expect_true(risk_2 > 3 * 0.0027)  # at least 3x the nominal value

  # As n increases, the risk should approach but remain above 0.0027
  risk_20 <- alpha.risk(20)
  expect_true(risk_20 > 0.0027)
  expect_true(risk_20 < risk_2)  # monotonic decrease is not guaranteed but
                                 # n=20 should be less inflated than n=2
})
