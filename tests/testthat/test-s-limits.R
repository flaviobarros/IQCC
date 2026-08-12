test_that("s_exact_limits matches the chi-square distribution directly", {
  sigma <- 2.3
  n <- 5L
  alpha <- 0.0027
  out <- s_exact_limits(sigma, n, alpha)

  expect_equal(
    out$lcl,
    sigma * sqrt(qchisq(alpha / 2, n - 1) / (n - 1)),
    tolerance = 1e-14
  )
  expect_equal(
    out$ucl,
    sigma * sqrt(qchisq(1 - alpha / 2, n - 1) / (n - 1)),
    tolerance = 1e-14
  )
  expect_equal(out$center, c4(n) * sigma, tolerance = 1e-14)
})

test_that("s_exact_limits supports vector subgroup sizes and upper charts", {
  n <- 2:8
  sigma <- 1.7
  alpha <- 0.01
  out <- s_exact_limits(sigma, n, alpha, side = "upper")

  expect_length(out$lcl, length(n))
  expect_true(all(out$lcl == 0))
  expect_equal(
    out$ucl,
    sigma * sqrt(qchisq(1 - alpha, n - 1) / (n - 1)),
    tolerance = 1e-14
  )
})

test_that("s_shewhart_limits matches the c4 formula", {
  sigma <- 1.8
  n <- 6L
  k <- 3
  c4n <- c4(n)
  se_s <- sigma * sqrt(1 - c4n^2)
  out <- s_shewhart_limits(sigma, n, nsigmas = k)

  expect_equal(out$center, c4n * sigma, tolerance = 1e-14)
  expect_equal(out$lcl, max(0, c4n * sigma - k * se_s), tolerance = 1e-14)
  expect_equal(out$ucl, c4n * sigma + k * se_s, tolerance = 1e-14)
})

test_that("s_shewhart_limits can reproduce the qcc observed-center convention", {
  sigma <- 2
  n <- c(4L, 5L, 6L)
  center <- 1.9
  out <- s_shewhart_limits(sigma, n, center = center)
  se_s <- sigma * sqrt(1 - c4(n)^2)

  expect_equal(out$center, rep(center, length(n)))
  expect_equal(out$lcl, pmax(0, center - 3 * se_s), tolerance = 1e-14)
  expect_equal(out$ucl, center + 3 * se_s, tolerance = 1e-14)
})

test_that("S-chart limits scale linearly with sigma", {
  exact1 <- s_exact_limits(1, 7)
  exact3 <- s_exact_limits(3, 7)
  expect_equal(exact3$lcl, 3 * exact1$lcl, tolerance = 1e-14)
  expect_equal(exact3$ucl, 3 * exact1$ucl, tolerance = 1e-14)
  expect_equal(exact3$center, 3 * exact1$center, tolerance = 1e-14)

  sh1 <- s_shewhart_limits(1, 7)
  sh3 <- s_shewhart_limits(3, 7)
  expect_equal(sh3$lcl, 3 * sh1$lcl, tolerance = 1e-14)
  expect_equal(sh3$ucl, 3 * sh1$ucl, tolerance = 1e-14)
  expect_equal(sh3$center, 3 * sh1$center, tolerance = 1e-14)
})

test_that("S-chart lower Shewhart limit is truncated at zero", {
  out <- s_shewhart_limits(1, 2, nsigmas = 3)
  expect_equal(out$lcl, 0)
  expect_gt(out$ucl, out$center)
})

test_that("pure S-chart functions reject invalid arguments", {
  for (bad in list(0, -1, NA_real_, NaN, Inf, c(1, 2)))
    expect_error(s_exact_limits(bad, 5), "sigma")

  for (bad in list(1, 2.5, NA_real_, Inf, c(3, NA_real_)))
    expect_error(s_exact_limits(1, bad), "n")

  for (bad in list(0, 1, -0.1, NA_real_, Inf, c(0.01, 0.02)))
    expect_error(s_exact_limits(1, 5, alpha = bad), "alpha")

  for (bad in list(0, -1, NA_real_, Inf, c(2, 3)))
    expect_error(s_shewhart_limits(1, 5, nsigmas = bad), "nsigmas")

  expect_error(s_exact_limits(1, 5, side = "left"), "arg")
  expect_error(s_shewhart_limits(1, 5, side = "left"), "arg")
  expect_error(s_shewhart_limits(1, 2:5, center = c(1, 2)), "center")
  expect_error(s_shewhart_limits(1, 5, center = NA_real_), "center")
})

test_that("cchart.S normalized limits preserve the historical qcc convention", {
  data(softdrink, package = "IQCC")
  legacy <- qcc::qcc(softdrink, type = "S")
  chart <- cchart.S(softdrink, type = "n")

  expect_s3_class(chart, "qcc")
  expect_equal(chart$center, legacy$center, tolerance = 1e-14)
  expect_equal(chart$std.dev, legacy$std.dev, tolerance = 1e-14)
  expect_equal(chart$limits, legacy$limits, tolerance = 1e-12)
})

test_that("cchart.S exact limits equal s_exact_limits with legacy sigma estimate", {
  data(softdrink, package = "IQCC")
  m <- 10L
  sigma_hat <- qcc::sd.S(softdrink)
  expected <- s_exact_limits(sigma_hat, m)
  chart <- cchart.S(softdrink, type = "e", m = m)

  expect_equal(as.numeric(chart$limits), c(expected$lcl, expected$ucl),
               tolerance = 1e-12)
})

test_that("cchart.S historical positional calls remain valid", {
  data(softdrink, package = "IQCC")
  named <- cchart.S(softdrink, type = "e", m = 10)
  positional <- cchart.S(softdrink, "e", 10)
  expect_equal(positional$limits, named$limits, tolerance = 1e-14)
})
