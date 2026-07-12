test_that("DS-np optimized probabilities match direct enumeration", {
  grid <- expand.grid(
    p = c(0, 0.05, 0.2, 0.5, 1),
    n1 = c(2, 3, 4),
    n2 = c(2, 3),
    stringsAsFactors = FALSE
  )

  for(i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    opt <- dsnp_prob_accept(g$p, g$n1, g$n2, 0.5, 2.5, 3.5)$pt
    ref <- IQCC:::.dsnp_reference_prob_accept(
      g$p, g$n1, g$n2, 0.5, 2.5, 3.5
    )
    expect_equal(opt, ref, tolerance = 1e-14)
  }
})

test_that("DS-np vector probabilities match scalar evaluations", {
  p <- c(0.005, 0.0075, 0.01)
  vec <- dsnp_prob_accept(p, 34, 162, 1.5, 2.5, 4.5)$pt
  scalar <- vapply(
    p,
    function(pi) dsnp_prob_accept(pi, 34, 162, 1.5, 2.5, 4.5)$pt,
    numeric(1)
  )
  expect_equal(vec, scalar)
})

test_that("Joekes et al. Table 2 first design is reproduced", {
  p0 <- 0.005
  p1 <- 0.0075
  n1 <- 34
  n2 <- 162
  wl <- 1.5
  ucl1 <- 2.5
  ucl2 <- 4.5

  expect_identical(round(dsnp_ass(p0, n1, n2, wl, ucl1)$ass, 2), 35.94)
  expect_identical(round(dsnp_arl(p0, n1, n2, wl, ucl1, ucl2)$arl, 2),
                   803.41)
  expect_identical(round(dsnp_arl(p1, n1, n2, wl, ucl1, ucl2)$arl, 2),
                   193.22)
})

test_that("Joekes et al. Table 5 selected designs are reproduced", {
  cases <- data.frame(
    p0 = c(0.005, 0.01, 0.02),
    p1 = c(0.01, 0.02, 0.04),
    n1 = c(50, 25, 20),
    n2 = c(242, 118, 73),
    wl = c(1.5, 1.5, 1.5),
    ucl1 = c(2.5, 2.5, 3.5),
    ucl2 = c(4.5, 4.5, 5.5),
    ass0 = c(55.83, 27.81, 24.33),
    arl0 = c(200.04, 212.94, 205.27),
    arl1 = c(21.37, 22.24, 13.04)
  )

  for(i in seq_len(nrow(cases))) {
    g <- cases[i, ]
    expect_identical(
      round(dsnp_ass(g$p0, g$n1, g$n2, g$wl, g$ucl1)$ass, 2),
      g$ass0
    )
    expect_identical(
      round(dsnp_arl(g$p0, g$n1, g$n2, g$wl, g$ucl1, g$ucl2)$arl, 2),
      g$arl0
    )
    expect_identical(
      round(dsnp_arl(g$p1, g$n1, g$n2, g$wl, g$ucl1, g$ucl2)$arl, 2),
      g$arl1
    )
  }
})

test_that("second-stage probability equals the warning-zone probability", {
  p <- seq(0, 1, length.out = 11)
  res <- dsnp_prob_accept(p, 10, 20, 1.5, 3.5, 5.5)
  direct <- pbinom(3, 10, p) - pbinom(1, 10, p)
  expect_equal(res$p_second, direct)
  expect_equal(dsnp_ass(p, 10, 20, 1.5, 3.5)$ass,
               10 + 20 * direct)
})

test_that("raising UCL2 cannot increase signal probability", {
  p <- seq(0.01, 0.5, length.out = 20)
  low <- dsnp_prob_accept(p, 10, 20, 1.5, 3.5, 4.5)$p_signal
  high <- dsnp_prob_accept(p, 10, 20, 1.5, 3.5, 6.5)$p_signal
  expect_true(all(high <= low + 1e-15))
})

test_that("DS-np input validation rejects non-finite values", {
  expect_error(dsnp_prob_accept(NA_real_, 10, 20, 1.5, 2.5, 4.5),
               "finite values")
  expect_error(dsnp_prob_accept(0.1, Inf, 20, 1.5, 2.5, 4.5),
               "positive integer")
  expect_error(dsnp_prob_accept(0.1, 10, 20, NaN, 2.5, 4.5),
               "finite numeric scalar")
  expect_error(dsnp_limits(NA_real_, 5, 10), "finite scalar")
})

test_that("dsnp_limits does not generate descending-sequence candidates", {
  res <- dsnp_limits(0.05, n1 = 1, n2 = 2, alpha = 0.2,
                     allow_empty_warning = FALSE, max_results = 100)
  expect_true(all(res$candidates$ucl1_reject >=
                    res$candidates$wl_accept + 2))
  expect_true(all(res$candidates$wl_accept <= 0))
})
