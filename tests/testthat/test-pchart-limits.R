test_that("pchart_limits normal limits match direct formula", {
  p <- 0.015
  n <- 20
  alpha <- 0.0027
  z <- qnorm(1 - alpha / 2)

  res <- pchart_limits(p, n, alpha = alpha, type = "normal",
                       truncate = FALSE)

  expect_equal(res$lcl, p - z * sqrt(p * (1 - p) / n))
  expect_equal(res$ucl, p + z * sqrt(p * (1 - p) / n))
})

test_that("pchart_limits cf1 matches Winterbottom correction", {
  p <- 0.015
  n <- 20
  alpha <- 0.0027
  z <- qnorm(1 - alpha / 2)

  expected <- p + z * sqrt(p * (1 - p) / n) +
    ((z^2 - 1) / (6 * n)) * (1 - 2 * p)

  res <- pchart_limits(p, n, alpha = alpha, type = "cf1",
                       truncate = FALSE)

  expect_equal(res$ucl, expected)
})

test_that("pchart_limits cf2 follows published operational limits", {
  p <- 0.015
  n <- 20
  alpha <- 0.0027
  z <- qnorm(1 - alpha / 2)
  sd_p <- sqrt(p * (1 - p) / n)

  cf1_upper <- p + z * sd_p +
    ((z^2 - 1) / (6 * n)) * (1 - 2 * p)
  second_adjustment <-
    ((z^3 - 3 * z) / (24 * n^2)) *
      (1 - 6 * p * (1 - p)) / sd_p -
    ((2 * z^3 - 5 * z) / (36 * n^2)) *
      (1 - 2 * p)^2 / sd_p

  res <- pchart_limits(p, n, alpha = alpha, type = "cf2",
                       truncate = FALSE)

  expect_equal(res$ucl, cf1_upper + second_adjustment)
  expect_true(res$lcl < res$ucl)
})

test_that("Joekes and Barbosa Table 2 is reproduced", {
  p <- 0.015
  n <- 20

  normal <- pchart_limits(p, n, type = "normal")
  cf1 <- pchart_limits(p, n, type = "cf1")
  cf2 <- pchart_limits(p, n, type = "cf2")

  expect_equal(normal$ucl, 0.0965, tolerance = 5e-5)
  expect_equal(cf1$ucl, 0.1612, tolerance = 5e-5)
  expect_equal(cf2$ucl, 0.1303, tolerance = 5e-5)

  expect_equal(pchart_alpha_risk(p, n, normal$lcl, normal$ucl),
               0.035746, tolerance = 5e-7)
  expect_equal(pchart_alpha_risk(p, n, cf1$lcl, cf1$ucl),
               0.000202, tolerance = 5e-7)
  expect_equal(pchart_alpha_risk(p, n, cf2$lcl, cf2$ucl),
               0.003178, tolerance = 5e-7)
})

test_that("Joekes and Barbosa Table 3 is reproduced", {
  p <- 0.004
  n <- 20

  normal <- pchart_limits(p, n, type = "normal")
  cf1 <- pchart_limits(p, n, type = "cf1")
  cf2 <- pchart_limits(p, n, type = "cf2")

  expect_equal(normal$ucl, 0.0463, tolerance = 5e-5)
  expect_equal(cf1$ucl, 0.1125, tolerance = 5e-5)
  expect_equal(cf2$ucl, 0.0533, tolerance = 5e-5)

  expect_equal(pchart_alpha_risk(p, n, normal$lcl, normal$ucl),
               0.077032, tolerance = 5e-7)
  expect_equal(pchart_alpha_risk(p, n, cf1$lcl, cf1$ucl),
               0.923038, tolerance = 5e-7)
  expect_equal(pchart_alpha_risk(p, n, cf2$lcl, cf2$ucl),
               0.002898, tolerance = 5e-7)
})

test_that("pchart_limits reports method applicability", {
  expect_true(pchart_limits(0.36, 20, type = "normal")$applicable)
  expect_true(pchart_limits(0.015, 20, type = "cf1")$applicable)
  expect_false(pchart_limits(0.01, 20, type = "cf1")$applicable)
  expect_true(pchart_limits(0.004, 20, type = "cf2")$npq > 0.079)
})

test_that("pchart_limits supports varying sample sizes", {
  res <- pchart_limits(0.02, c(20, 30, 40), type = "cf2")

  expect_length(res$lcl, 3)
  expect_length(res$ucl, 3)
  expect_true(all(diff(res$ucl) < 0))
})

test_that("cchart.p uses pooled binomial estimator", {
  x <- c(1, 18)
  n <- c(10, 90)

  obj <- cchart.p(x1 = x, n1 = n, type = "normal")

  expect_equal(obj$center, sum(x) / sum(n))
  expect_false(isTRUE(all.equal(obj$center, mean(x / n))))
})

test_that("cchart.p standardized chart preserves z statistics", {
  x <- c(1, 18)
  n <- c(10, 90)
  phat <- sum(x) / sum(n)
  expected_z <- (x / n - phat) / sqrt(phat * (1 - phat) / n)

  obj <- cchart.p(x1 = x, n1 = n, type = "standardized")

  expect_equal(as.numeric(obj$statistics), expected_z)
  expect_equal(obj$center, 0)
})

test_that("legacy p-chart type aliases remain supported", {
  x <- c(1, 2, 1)
  n <- c(20, 20, 20)

  expect_no_error(cchart.p(x1 = x, n1 = n, type = "norm"))
  expect_no_error(cchart.p(x1 = x, n1 = n, type = "CF"))
  expect_no_error(cchart.p(x1 = x, n1 = n, type = "std"))
})

test_that("pchart functions validate inputs", {
  expect_error(pchart_limits(0, 20), "strictly between 0 and 1")
  expect_error(pchart_limits(0.1, 0), "positive integers")
  expect_error(pchart_limits(0.1, 20, alpha = 1), "between 0 and 1")
  expect_error(pchart_alpha_risk(0.1, 20, 0.3, 0.2), "lcl <= ucl")
  expect_error(cchart.p(x1 = numeric(), n1 = numeric()), "must not be empty")
  expect_error(cchart.p(x1 = 1, p1 = 0.1, n1 = 10),
               "exactly one of x1 or p1")
})
