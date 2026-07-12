test_that("cchart.GV rejects indefinite Sigma with positive determinant", {
  x <- array(rnorm(6 * 8 * 2), dim = c(6, 8, 2))
  indefinite <- matrix(c(-2, 0, 0, -1), nrow = 2)

  expect_error(
    cchart.GV(x, Sigma = indefinite, type = "exact", plot = FALSE),
    "positive definite"
  )
})

test_that("cchart.GV rejects singular Phase I covariance estimate", {
  base <- cbind(1:8, 2 * (1:8))
  x <- array(rep(base, 6), dim = c(6, 8, 2))

  expect_error(
    cchart.GV(x, type = "exact", plot = FALSE),
    "average covariance matrix must be positive definite"
  )
})

test_that("cchart.DSnp rejects non-finite observations and limits", {
  expect_error(
    cchart.DSnp(Inf, n1 = 10, n2 = 20, p0 = 0.05,
                wl = 1.5, ucl1 = 2.5, ucl2 = 4.5, plot = FALSE),
    "finite"
  )
  expect_error(
    cchart.DSnp(0, n1 = 10, n2 = 20, p0 = 0.05,
                wl = 1.5, ucl1 = Inf, ucl2 = 4.5, plot = FALSE),
    "finite"
  )
})

test_that("cchart.DSnp validates limits object metadata", {
  lim <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10,
                     alpha = 0.10, max_results = 1)

  expect_error(
    cchart.DSnp(0, n1 = 6, n2 = 10, p0 = 0.05,
                limits = lim, plot = FALSE),
    "n1 is incompatible"
  )
  expect_error(
    cchart.DSnp(0, n1 = 5, n2 = 11, p0 = 0.05,
                limits = lim, plot = FALSE),
    "n2 is incompatible"
  )
  expect_error(
    cchart.DSnp(0, n1 = 5, n2 = 10, p0 = 0.04,
                limits = lim, plot = FALSE),
    "p0 is incompatible"
  )
})
