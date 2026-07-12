test_that("uchart_limits normal limits match direct formula", {
  lambda <- 1.4
  n <- 10
  alpha <- 0.0027
  z <- qnorm(1 - alpha / 2)
  res <- uchart_limits(lambda, n, alpha = alpha, type = "normal",
                       truncate = FALSE)
  expect_equal(res$lcl, lambda - z * sqrt(lambda / n))
  expect_equal(res$ucl, lambda + z * sqrt(lambda / n))
})

test_that("uchart_limits cf1 follows Poisson Cornish-Fisher correction", {
  lambda <- 1.4
  n <- 10
  alpha <- 0.0027
  z <- qnorm(1 - alpha / 2)
  expected <- lambda + z * sqrt(lambda / n) + (z^2 - 1) / (6 * n)
  res <- uchart_limits(lambda, n, alpha = alpha, type = "cf1",
                       truncate = FALSE)
  expect_equal(res$ucl, expected)
})

test_that("uchart_limits cf2 reduces to historical three-sigma formula", {
  lambda <- 1.4
  n <- 10
  alpha_three_sigma <- 2 * (1 - pnorm(3))
  expected <- lambda + 3 * sqrt(lambda / n) + 4 / (3 * n) -
    1 / (3 * n * sqrt(lambda * n))
  res <- uchart_limits(lambda, n, alpha = alpha_three_sigma, type = "cf2",
                       truncate = FALSE)
  expect_equal(res$ucl, expected, tolerance = 1e-12)
})

test_that("uchart_alpha_risk matches direct Poisson tail calculation", {
  lambda <- 0.1
  n <- 10
  limits <- uchart_limits(lambda, n, type = "cf2")
  expected <- ppois(ceiling(n * limits$lcl) - 1, lambda * n) +
    1 - ppois(floor(n * limits$ucl), lambda * n)
  if(limits$lcl == 0) expected <- expected - ppois(-1, lambda * n)
  expect_equal(uchart_alpha_risk(lambda, n, limits$lcl, limits$ucl), expected)
})

test_that("cchart.u uses pooled Poisson estimator", {
  x <- c(1, 18)
  n <- c(10, 90)
  obj <- cchart.u(x1 = x, n1 = n, type = "normal")
  expect_equal(obj$center, sum(x) / sum(n))
  expect_false(isTRUE(all.equal(obj$center, mean(x / n))))
})

test_that("cchart.u standardized chart preserves z statistics", {
  x <- c(1, 18)
  n <- c(10, 90)
  lambda <- sum(x) / sum(n)
  expected_z <- (x / n - lambda) / sqrt(lambda / n)
  obj <- cchart.u(x1 = x, n1 = n, type = "standardized")
  expect_equal(as.numeric(obj$statistics), expected_z)
  expect_equal(obj$center, 0)
})

test_that("legacy u-chart aliases remain supported", {
  x <- c(1, 2, 3)
  n <- c(10, 10, 10)
  expect_no_error(cchart.u(x1 = x, n1 = n, type = "norm"))
  expect_no_error(cchart.u(x1 = x, n1 = n, type = "CF"))
  expect_no_error(cchart.u(x1 = x, n1 = n, type = "std"))
})

test_that("u-chart functions validate inputs", {
  expect_error(uchart_limits(0, 10), "positive scalar")
  expect_error(uchart_limits(1, 0), "positive values")
  expect_error(uchart_alpha_risk(1, 10, 2, 1), "lcl <= ucl")
  expect_error(cchart.u(x1 = numeric(), n1 = numeric()), "must not be empty")
  expect_error(cchart.u(x1 = 1, u1 = 0.1, n1 = 10),
               "exactly one of x1 or u1")
})
