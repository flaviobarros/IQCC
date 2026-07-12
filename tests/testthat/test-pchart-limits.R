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

test_that("pchart_limits cf2 matches published expansion", {
  p <- 0.015
  n <- 20
  alpha <- 0.0027
  z <- qnorm(1 - alpha / 2)
  sd_p <- sqrt(p * (1 - p) / n)

  expected <- p + z * sd_p +
    ((z^2 - 1) / (6 * n)) * (1 - 2 * p) +
    ((z^3 - 3 * z) / (24 * n^2)) *
      (1 - 6 * p * (1 - p)) / sd_p -
    ((2 * z^3 - 5 * z) / (36 * n^2)) *
      (1 - 2 * p)^2 / sd_p

  res <- pchart_limits(p, n, alpha = alpha, type = "cf2",
                       truncate = FALSE)

  expect_equal(res$ucl, expected)
})

test_that("pchart_limits supports varying sample sizes", {
  res <- pchart_limits(0.02, c(20, 30, 40), type = "cf2")

  expect_length(res$lcl, 3)
  expect_length(res$ucl, 3)
  expect_true(all(diff(res$ucl) < 0))
})

test_that("cchart.p uses pooled binomial estimator", {
  x <- c(1, 9)
  n <- c(10, 90)

  obj <- cchart.p(x1 = x, n1 = n, type = "normal")

  expect_equal(obj$center, sum(x) / sum(n))
  expect_false(isTRUE(all.equal(obj$center, mean(x / n))))
})

test_that("cchart.p standardized chart preserves z statistics", {
  x <- c(1, 9)
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

test_that("pchart_limits validates inputs", {
  expect_error(pchart_limits(0, 20), "strictly between 0 and 1")
  expect_error(pchart_limits(0.1, 0), "positive integers")
  expect_error(pchart_limits(0.1, 20, alpha = 1), "between 0 and 1")
})
