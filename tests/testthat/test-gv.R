test_that("gv_stat computes covariance determinants by subgroup", {
  g1 <- cbind(c(0, 1, 2, 3), c(0, 2, 1, 3))
  g2 <- cbind(c(1, 2, 4, 7), c(3, 1, 5, 2))
  expected <- c(det(stats::cov(g1)), det(stats::cov(g2)))

  expect_equal(gv_stat(list(g1, g2)), expected)

  x <- rbind(g1, g2)
  expect_equal(gv_stat(x, size = 4), expected)
})

test_that("generalized variance moments scale with det Sigma", {
  m1 <- IQCC:::.gv_moments(10, 2, 1)
  m2 <- IQCC:::.gv_moments(10, 2, 5)

  expect_equal(m2$mean, 5 * m1$mean, tolerance = 1e-12)
  expect_equal(m2$variance, 25 * m1$variance, tolerance = 1e-11)
  expect_equal(m1$b1, 8 / 9, tolerance = 1e-12)
  expect_equal(m1$b2, 8 * 38 / 9^3, tolerance = 1e-12)
})

test_that("published d = 2 numerical example is reproduced", {
  det_sigma <- 0.5320

  normal <- gv_limits(10, 2, det_sigma, type = "normal", side = "upper")
  cf <- gv_limits(10, 2, det_sigma, type = "cf", side = "upper")
  exact <- gv_limits(10, 2, det_sigma, type = "exact", side = "upper")

  expect_equal(normal$ucl, 1.4286, tolerance = 1e-4)
  expect_equal(cf$ucl, 2.1602, tolerance = 2e-4)
  expect_equal(exact$ucl, 2.1536, tolerance = 1e-4)
})

test_that("published d = 3 numerical example is reproduced", {
  n <- 15
  p <- 3
  m <- 30
  b3 <- IQCC:::.gv_b3(m, n, p)
  det_sigma <- 69.8438 / b3

  normal <- gv_limits(n, p, det_sigma, type = "normal", side = "upper")
  cf <- gv_limits(n, p, det_sigma, type = "cf", side = "upper")
  exact <- gv_limits(n, p, det_sigma, type = "exact", side = "upper")

  expect_equal(normal$ucl, 170.294, tolerance = 0.002)
  expect_equal(cf$ucl, 267.652, tolerance = 0.002)
  expect_equal(exact$ucl, 265.462, tolerance = 0.01)
})

test_that("d = 2 exact false alarm equals nominal risk", {
  risk <- gv_alpha_risk(10, 2, type = "exact", side = "upper")
  expect_equal(risk$alpha, 0.0027, tolerance = 1e-10)
  expect_equal(risk$arl0, 1 / 0.0027, tolerance = 1e-6)
})

test_that("simulation is reproducible and preserves RNG state", {
  set.seed(99)
  before <- .Random.seed
  a <- gv_limits(8, 3, type = "simulation", nsim = 2000, seed = 42)
  after <- .Random.seed
  b <- gv_limits(8, 3, type = "simulation", nsim = 2000, seed = 42)

  expect_identical(before, after)
  expect_equal(a$ucl, b$ucl)
})

test_that("cchart.GV returns phase information and limits", {
  set.seed(1)
  x <- array(rnorm(6 * 8 * 2), dim = c(6, 8, 2))
  chart <- cchart.GV(x, Sigma = diag(2), type = "exact", plot = FALSE)

  expect_s3_class(chart, "cchart.GV")
  expect_length(chart$statistics, 6)
  expect_equal(chart$n, 8)
  expect_equal(chart$p, 2)
  expect_true(chart$limits$ucl > 0)
})

test_that("invalid generalized variance inputs are rejected", {
  expect_error(gv_limits(2, 2), "greater than p")
  expect_error(gv_limits(10, 1), "greater than or equal to 2")
  expect_error(gv_limits(10, 2, det_sigma = 0), "positive finite")
  expect_error(gv_limits(10, 4, type = "exact"), "implemented only")
})
