# --- cchart.DSnp tests ---

# --- 1. Structure tests ---

test_that("cchart.DSnp returns structured list with plot=FALSE", {
  x1 <- c(0, 1, 2, 3, 1)
  x2 <- c(NA, NA, 1, NA, NA)
  res <- cchart.DSnp(x1, n1 = 10, n2 = 20, p0 = 0.05,
                     x2 = x2, wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                     plot = FALSE)
  expect_type(res, "list")
  expect_s3_class(res, "cchart.DSnp")
  expect_named(res, c("call", "data", "limits", "parameters", "performance"))
})

test_that("cchart.DSnp data has correct columns", {
  x1 <- c(0, 1, 2)
  x2 <- c(NA, NA, 1)
  res <- cchart.DSnp(x1, n1 = 10, n2 = 20, p0 = 0.05,
                     x2 = x2, wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                     plot = FALSE)
  expect_s3_class(res$data, "data.frame")
  expect_named(res$data, c("index", "x1", "x2", "total", "stage", "signal"))
  expect_equal(nrow(res$data), 3)
})

test_that("cchart.DSnp limits contains all threshold elements", {
  res <- cchart.DSnp(c(0), n1 = 10, n2 = 20, p0 = 0.05,
                     wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                     plot = FALSE)
  expect_named(res$limits, c("wl", "ucl1", "ucl2",
                              "wl_accept", "ucl1_reject", "ucl2_accept"))
})

test_that("cchart.DSnp parameters stores inputs", {
  res <- cchart.DSnp(c(0), n1 = 10, n2 = 20, p0 = 0.05,
                     wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                     alpha = 0.01, plot = FALSE)
  expect_equal(res$parameters$n1, 10)
  expect_equal(res$parameters$n2, 20)
  expect_equal(res$parameters$p0, 0.05)
  expect_equal(res$parameters$alpha, 0.01)
  expect_null(res$parameters$p1)
})

test_that("cchart.DSnp performance contains arl0, ass0, p_signal0", {
  res <- cchart.DSnp(c(0), n1 = 10, n2 = 20, p0 = 0.05,
                     wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                     plot = FALSE)
  expect_true("arl0" %in% names(res$performance))
  expect_true("ass0" %in% names(res$performance))
  expect_true("p_signal0" %in% names(res$performance))
})

# --- 2. Manual limits integer thresholds ---

test_that("cchart.DSnp integer thresholds are correct", {
  res <- cchart.DSnp(c(0), n1 = 10, n2 = 20, p0 = 0.05,
                     wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                     plot = FALSE)
  expect_equal(res$limits$wl_accept, 1)
  expect_equal(res$limits$ucl1_reject, 3)
  expect_equal(res$limits$ucl2_accept, 4)
})

# --- 3. Classification logic ---

test_that("cchart.DSnp accepts at first stage when x1 <= wl_accept", {
  # wl_accept = 1, so x1 = 0 and x1 = 1 should accept
  res <- cchart.DSnp(c(0, 1, 1), n1 = 10, n2 = 20, p0 = 0.05,
                     wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                     plot = FALSE)
  expect_equal(res$data$stage[1], "accept_first")
  expect_false(res$data$signal[1])
  expect_equal(res$data$stage[2], "accept_first")
  expect_false(res$data$signal[2])
})

test_that("cchart.DSnp signals at first stage when x1 >= ucl1_reject", {
  # ucl1_reject = 3, so x1 = 3 should signal
  res <- cchart.DSnp(c(3, 4), n1 = 10, n2 = 20, p0 = 0.05,
                     wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                     plot = FALSE)
  expect_equal(res$data$stage[1], "signal_first")
  expect_true(res$data$signal[1])
  expect_equal(res$data$stage[2], "signal_first")
  expect_true(res$data$signal[2])
})

test_that("cchart.DSnp requires second sample when x1 in intermediate zone", {
  # x1 = 2 is between wl_accept=1 and ucl1_reject=3
  res <- cchart.DSnp(c(2), n1 = 10, n2 = 20, p0 = 0.05,
                     x2 = c(1),
                     wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                     plot = FALSE)
  expect_equal(res$data$stage[1], "accept_second")
  expect_false(res$data$signal[1])
  expect_equal(res$data$total[1], 3)
})

test_that("cchart.DSnp total <= ucl2_accept accepts at second stage", {
  # x1=2, x2=2, total=4, ucl2_accept=4 -> accept
  res <- cchart.DSnp(c(2), n1 = 10, n2 = 20, p0 = 0.05,
                     x2 = c(2),
                     wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                     plot = FALSE)
  expect_equal(res$data$stage[1], "accept_second")
  expect_false(res$data$signal[1])
  expect_equal(res$data$total[1], 4)
})

test_that("cchart.DSnp total > ucl2_accept signals at second stage", {
  # x1=2, x2=3, total=5, ucl2_accept=4 -> signal
  res <- cchart.DSnp(c(2), n1 = 10, n2 = 20, p0 = 0.05,
                     x2 = c(3),
                     wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                     plot = FALSE)
  expect_equal(res$data$stage[1], "signal_second")
  expect_true(res$data$signal[1])
  expect_equal(res$data$total[1], 5)
})

# --- 4. Validation error tests ---

test_that("cchart.DSnp errors on negative x1", {
  expect_error(
    cchart.DSnp(c(-1), n1 = 10, n2 = 20, p0 = 0.05,
                wl = 1.5, ucl1 = 2.5, ucl2 = 4.5, plot = FALSE),
    "x1 must not contain negative values"
  )
})

test_that("cchart.DSnp errors when x1 > n1", {
  expect_error(
    cchart.DSnp(c(11), n1 = 10, n2 = 20, p0 = 0.05,
                wl = 1.5, ucl1 = 2.5, ucl2 = 4.5, plot = FALSE),
    "x1 values must not exceed n1"
  )
})

test_that("cchart.DSnp errors on negative x2", {
  expect_error(
    cchart.DSnp(c(2), n1 = 10, n2 = 20, p0 = 0.05,
                x2 = c(-1),
                wl = 1.5, ucl1 = 2.5, ucl2 = 4.5, plot = FALSE),
    "x2 must not contain negative values"
  )
})

test_that("cchart.DSnp errors when x2 > n2", {
  expect_error(
    cchart.DSnp(c(2), n1 = 10, n2 = 20, p0 = 0.05,
                x2 = c(21),
                wl = 1.5, ucl1 = 2.5, ucl2 = 4.5, plot = FALSE),
    "x2 values must not exceed n2"
  )
})

test_that("cchart.DSnp errors when x2 needed but missing", {
  expect_error(
    cchart.DSnp(c(2), n1 = 10, n2 = 20, p0 = 0.05,
                wl = 1.5, ucl1 = 2.5, ucl2 = 4.5, plot = FALSE),
    "Second sample.*required"
  )
})

test_that("cchart.DSnp errors when x2 needed but NA", {
  expect_error(
    cchart.DSnp(c(2), n1 = 10, n2 = 20, p0 = 0.05,
                x2 = c(NA),
                wl = 1.5, ucl1 = 2.5, ucl2 = 4.5, plot = FALSE),
    "Second sample.*required"
  )
})

test_that("cchart.DSnp errors when only partial manual limits provided", {
  expect_error(
    cchart.DSnp(c(0), n1 = 10, n2 = 20, p0 = 0.05,
                wl = 1.5, ucl1 = 2.5, plot = FALSE),
    "All of.*wl.*ucl1.*ucl2.*must be provided together"
  )
  expect_error(
    cchart.DSnp(c(0), n1 = 10, n2 = 20, p0 = 0.05,
                wl = 1.5, plot = FALSE),
    "All of.*wl.*ucl1.*ucl2.*must be provided together"
  )
})

test_that("cchart.DSnp errors when limits and manual limits both given", {
  lim <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.10,
                     max_results = 1)
  expect_error(
    cchart.DSnp(c(0), n1 = 10, n2 = 20, p0 = 0.05,
                wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                limits = lim, plot = FALSE),
    "Cannot specify both.*limits.*and manual"
  )
})

test_that("cchart.DSnp errors on non-integer x1", {
  expect_error(
    cchart.DSnp(c(1.5), n1 = 10, n2 = 20, p0 = 0.05,
                wl = 1.5, ucl1 = 2.5, ucl2 = 4.5, plot = FALSE),
    "x1 must contain integer values"
  )
})

# --- 5. Use with dsnp_limits() ---

test_that("cchart.DSnp auto-computes limits via dsnp_limits when none given", {
  res <- cchart.DSnp(c(0, 1, 2, 0, 1), n1 = 10, n2 = 20, p0 = 0.05,
                     x2 = c(NA, NA, 1, NA, NA),
                     plot = FALSE)
  expect_true(!is.null(res$limits$wl))
  expect_true(!is.null(res$limits$ucl1))
  expect_true(!is.null(res$limits$ucl2))
  expect_true(is.numeric(res$limits$wl))
})

test_that("cchart.DSnp works with limits object from dsnp_limits", {
  lim <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.10,
                     max_results = 1)
  res <- cchart.DSnp(c(0, 1, 0), n1 = 5, n2 = 10, p0 = 0.05,
                     x2 = c(NA, 0, NA),
                     limits = lim, plot = FALSE)
  expect_equal(res$limits$wl, lim$best$wl)
  expect_equal(res$limits$ucl1, lim$best$ucl1)
  expect_equal(res$limits$ucl2, lim$best$ucl2)
})

# --- 6. Performance consistency ---

test_that("cchart.DSnp performance matches dsnp_* functions", {
  wl <- 1.5; ucl1 <- 2.5; ucl2 <- 4.5
  n1 <- 10; n2 <- 20; p0 <- 0.05

  res <- cchart.DSnp(c(0, 1, 2, 3), n1 = n1, n2 = n2, p0 = p0,
                     x2 = c(NA, NA, 1, NA),
                     wl = wl, ucl1 = ucl1, ucl2 = ucl2,
                     plot = FALSE)

  arl0 <- dsnp_arl(p0, n1, n2, wl, ucl1, ucl2)
  ass0 <- dsnp_ass(p0, n1, n2, wl, ucl1)
  pa0  <- dsnp_prob_accept(p0, n1, n2, wl, ucl1, ucl2)

  expect_equal(res$performance$arl0, arl0$arl)
  expect_equal(res$performance$ass0, ass0$ass)
  expect_equal(res$performance$p_signal0, pa0$p_signal)
})

test_that("cchart.DSnp performance includes p1 metrics when p1 provided", {
  res <- cchart.DSnp(c(0), n1 = 10, n2 = 20, p0 = 0.05,
                     wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                     p1 = 0.10, plot = FALSE)
  expect_true("arl1" %in% names(res$performance))
  expect_true("ass1" %in% names(res$performance))
  expect_true("p_signal1" %in% names(res$performance))

  arl1 <- dsnp_arl(0.10, 10, 20, 1.5, 2.5, 4.5)
  expect_equal(res$performance$arl1, arl1$arl)
})

# --- 7. Plot tests ---

test_that("cchart.DSnp plot=FALSE does not open graphics", {
  expect_no_error(
    cchart.DSnp(c(0, 1), n1 = 10, n2 = 20, p0 = 0.05,
                 wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                 plot = FALSE)
  )
})

test_that("cchart.DSnp plot=TRUE produces a plot without error", {
  pdf(NULL)  # null device
  on.exit(dev.off())
  res <- cchart.DSnp(c(0, 1, 2, 3, 0), n1 = 10, n2 = 20, p0 = 0.05,
                     x2 = c(NA, NA, 1, NA, NA),
                     wl = 1.5, ucl1 = 2.5, ucl2 = 4.5)
  expect_s3_class(res, "cchart.DSnp")
})

# --- 8. Mixed observation types ---

test_that("cchart.DSnp handles mixed stages correctly", {
  x1 <- c(0, 1, 2, 3, 1, 0, 2, 4, 1, 0)
  x2 <- c(NA, NA, 2, NA, NA, NA, 3, NA, NA, NA)
  res <- cchart.DSnp(x1, n1 = 10, n2 = 20, p0 = 0.05,
                     x2 = x2, wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                     plot = FALSE)

  # x1=0 -> accept_first
  expect_equal(res$data$stage[1], "accept_first")
  expect_false(res$data$signal[1])

  # x1=1 -> accept_first
  expect_equal(res$data$stage[2], "accept_first")

  # x1=2, x2=2 -> total=4, accept_second
  expect_equal(res$data$stage[3], "accept_second")
  expect_equal(res$data$total[3], 4)
  expect_false(res$data$signal[3])

  # x1=3 -> signal_first
  expect_equal(res$data$stage[4], "signal_first")
  expect_true(res$data$signal[4])

  # x1=2, x2=3 -> total=5, signal_second
  expect_equal(res$data$stage[7], "signal_second")
  expect_equal(res$data$total[7], 5)
  expect_true(res$data$signal[7])

  # x1=4 -> signal_first
  expect_equal(res$data$stage[8], "signal_first")
  expect_true(res$data$signal[8])
})
