# --- dsnp_prob_accept tests ---

test_that("dsnp_prob_accept returns structured list", {
  res <- dsnp_prob_accept(0.5, 10, 20, 1.5, 2.5, 4.5)
  expect_type(res, "list")
  expect_named(res, c("pa1", "pa2", "pt", "p_signal", "p_decision_first",
                       "p_second", "n1", "n2", "wl", "ucl1", "ucl2",
                       "wl_accept", "ucl1_reject", "ucl2_accept"))
})

test_that("dsnp_prob_accept p=0 always accepts", {
  res <- dsnp_prob_accept(0, 10, 20, 1.5, 2.5, 4.5)
  expect_equal(res$pt, 1)
  expect_equal(res$p_signal, 0)
  expect_equal(res$pa1, 1)
  expect_equal(res$pa2, 0)
})

test_that("dsnp_prob_accept p=1 always signals", {
  res <- dsnp_prob_accept(1, 10, 20, 1.5, 2.5, 4.5)
  expect_equal(res$pt, 0)
  expect_equal(res$p_signal, 1)
})

test_that("dsnp_prob_accept probabilities sum correctly", {
  res <- dsnp_prob_accept(0.3, 10, 20, 1.5, 2.5, 4.5)
  expect_equal(res$pt, res$pa1 + res$pa2)
  expect_equal(res$p_signal, 1 - res$pt)
  expect_equal(res$p_decision_first + res$p_second, 1)
})

test_that("dsnp_prob_accept pt is between 0 and 1", {
  res <- dsnp_prob_accept(0.5, 10, 20, 1.5, 2.5, 4.5)
  expect_true(res$pt >= 0)
  expect_true(res$pt <= 1)
})

test_that("dsnp_prob_accept vector input works", {
  res <- dsnp_prob_accept(c(0, 0.5, 1), 10, 20, 1.5, 2.5, 4.5)
  expect_length(res$pt, 3)
  expect_equal(res$pt[1], 1)
  expect_equal(res$pt[3], 0)
})

test_that("dsnp_prob_accept integer thresholds are correct", {
  res <- dsnp_prob_accept(0.5, 10, 20, 1.5, 2.5, 4.5)
  expect_equal(res$wl_accept, 1)
  expect_equal(res$ucl1_reject, 3)
  expect_equal(res$ucl2_accept, 4)
})

test_that("dsnp_prob_accept hand-check: small n, p=0.5", {
  # n1=2, n2=3, wl=0.5, ucl1=1.5, ucl2=2.5
  # wl_accept=0, ucl1_reject=2, ucl2_accept=2
  # Warning zone: d1=1 only
  # Pa1 = P(D1<=0) = dbinom(0,2,0.5) = 0.25
  # Pa2 = dbinom(1,2,0.5)*pbinom(1,3,0.5) = 0.5*0.5 = 0.25
  # PT = 0.5
  res <- dsnp_prob_accept(0.5, 2, 3, 0.5, 1.5, 2.5)
  expect_equal(res$pa1, 0.25)
  expect_equal(res$pa2, 0.25)
  expect_equal(res$pt, 0.5)
})

test_that("dsnp_prob_accept errors on invalid p", {
  expect_error(dsnp_prob_accept(-0.1, 10, 20, 1.5, 2.5, 4.5),
               "p must be between 0 and 1")
  expect_error(dsnp_prob_accept(1.1, 10, 20, 1.5, 2.5, 4.5),
               "p must be between 0 and 1")
})

test_that("dsnp_prob_accept errors on invalid n1", {
  expect_error(dsnp_prob_accept(0.5, 1.5, 20, 1.5, 2.5, 4.5),
               "n1 must be a positive integer")
  expect_error(dsnp_prob_accept(0.5, 0, 20, 1.5, 2.5, 4.5),
               "n1 must be a positive integer")
})

test_that("dsnp_prob_accept errors on invalid n2", {
  expect_error(dsnp_prob_accept(0.5, 10, 0, 1.5, 2.5, 4.5),
               "n2 must be a positive integer")
})

test_that("dsnp_prob_accept errors when wl >= ucl1", {
  expect_error(dsnp_prob_accept(0.5, 10, 20, 2.5, 2.5, 4.5),
               "wl must be less than ucl1")
  expect_error(dsnp_prob_accept(0.5, 10, 20, 3.0, 2.5, 4.5),
               "wl must be less than ucl1")
})

test_that("dsnp_prob_accept errors when ucl2 <= wl", {
  expect_error(dsnp_prob_accept(0.5, 10, 20, 1.5, 2.5, 1.5),
               "ucl2 must be greater than wl")
})

test_that("dsnp_prob_accept handles empty warning zone", {
  # wl=1.5, ucl1=1.5001: wl_accept=1, ucl1_reject=2
  # d1_lower=2, d1_upper=1 -> empty warning zone, pa2 must be 0
  res <- dsnp_prob_accept(0.5, 10, 20, 1.5, 1.5001, 4.5)
  expect_equal(res$pa2, 0)
  expect_equal(res$pt, res$pa1)
  expect_equal(res$p_second, 0)
})

# --- dsnp_arl tests ---

test_that("dsnp_arl returns structured list", {
  res <- dsnp_arl(0.5, 10, 20, 1.5, 2.5, 4.5)
  expect_type(res, "list")
  expect_named(res, c("arl", "pt", "p_signal", "n1", "n2", "wl", "ucl1", "ucl2"))
})

test_that("dsnp_arl at p=0 is Inf", {
  res <- dsnp_arl(0, 10, 20, 1.5, 2.5, 4.5)
  expect_equal(res$arl, Inf)
})

test_that("dsnp_arl at p=1 is 1", {
  res <- dsnp_arl(1, 10, 20, 1.5, 2.5, 4.5)
  expect_equal(res$arl, 1)
})

test_that("dsnp_arl matches 1/p_signal", {
  res <- dsnp_arl(0.3, 10, 20, 1.5, 2.5, 4.5)
  expect_equal(res$arl, 1 / res$p_signal)
})

test_that("dsnp_arl ARL decreases as p increases", {
  res_low <- dsnp_arl(0.1, 10, 20, 1.5, 2.5, 4.5)
  res_high <- dsnp_arl(0.5, 10, 20, 1.5, 2.5, 4.5)
  expect_true(res_low$arl > res_high$arl)
})

# --- dsnp_ass tests ---

test_that("dsnp_ass returns structured list", {
  res <- dsnp_ass(0.5, 10, 20, 1.5, 2.5)
  expect_type(res, "list")
  expect_named(res, c("ass", "p_second", "n1", "n2", "wl", "ucl1"))
})

test_that("dsnp_ass at p=0 equals n1", {
  res <- dsnp_ass(0, 10, 20, 1.5, 2.5)
  expect_equal(res$ass, 10)
  expect_equal(res$p_second, 0)
})

test_that("dsnp_ass is between n1 and n1+n2", {
  res <- dsnp_ass(0.3, 10, 20, 1.5, 2.5)
  expect_true(res$ass >= 10)
  expect_true(res$ass <= 30)
})

test_that("dsnp_ass vector input works", {
  res <- dsnp_ass(c(0, 0.5, 1), 10, 20, 1.5, 2.5)
  expect_length(res$ass, 3)
  expect_equal(res$ass[1], 10)
})

test_that("dsnp_ass errors on invalid p", {
  expect_error(dsnp_ass(-0.1, 10, 20, 1.5, 2.5),
               "p must be between 0 and 1")
})

# --- Published table values (Joekes et al. 2015) ---

test_that("dsnp_arl matches published ARL0", {
  res <- dsnp_arl(0.005, n1 = 34, n2 = 162,
                  wl = 1.5, ucl1 = 2.5, ucl2 = 4.5)
  expect_equal(res$arl, 803.41, tolerance = 0.01)
})

test_that("dsnp_arl matches published ARL1", {
  res <- dsnp_arl(0.0075, n1 = 34, n2 = 162,
                  wl = 1.5, ucl1 = 2.5, ucl2 = 4.5)
  expect_equal(res$arl, 193.22, tolerance = 0.01)
})

test_that("dsnp_ass matches published ASS", {
  res <- dsnp_ass(0.005, n1 = 34, n2 = 162,
                  wl = 1.5, ucl1 = 2.5)
  expect_equal(res$ass, 35.94, tolerance = 0.01)
})
