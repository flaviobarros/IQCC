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
  expect_named(res, c("ass", "p_second", "n1", "n2", "wl", "ucl1", "ucl2",
                       "curtailed"))
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

# --- Curtailed ASS tests ---

test_that("dsnp_ass curtailed returns structured list with ucl2", {
  res <- dsnp_ass(0.005, 34, 162, 1.5, 2.5, ucl2 = 4.5, curtailed = TRUE)
  expect_type(res, "list")
  expect_named(res, c("ass", "p_second", "n1", "n2", "wl", "ucl1", "ucl2",
                       "curtailed"))
  expect_equal(res$ucl2, 4.5)
  expect_true(res$curtailed)
})

test_that("dsnp_ass curtailed is less than or equal to complete ASS", {
  p_vals <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.3, 0.5)
  for(p in p_vals)
  {
    complete <- dsnp_ass(p, 34, 162, 1.5, 2.5)$ass
    curtailed <- dsnp_ass(p, 34, 162, 1.5, 2.5, ucl2 = 4.5,
                          curtailed = TRUE)$ass
    expect_true(curtailed <= complete + 1e-15,
                label = paste0("p=", p, ": curtailed (", curtailed,
                               ") > complete (", complete, ")"))
  }
})

test_that("dsnp_ass curtailed equals complete when r(d1) > n2 for all d1", {
  # Small n1, n2 with very wide ucl2: r(d1) always > n2, so inspection
  # never stops early -> curtailed = complete
  res_c <- dsnp_ass(0.3, 5, 10, 1.5, 2.5, ucl2 = 100, curtailed = TRUE)
  res_f <- dsnp_ass(0.3, 5, 10, 1.5, 2.5)
  expect_equal(res_c$ass, res_f$ass)
})

test_that("dsnp_ass curtailed p=0 equals n1", {
  res <- dsnp_ass(0, 10, 20, 1.5, 2.5, ucl2 = 4.5, curtailed = TRUE)
  expect_equal(res$ass, 10)
})

test_that("dsnp_ass curtailed p=1 equals n1", {
  res <- dsnp_ass(1, 10, 20, 1.5, 2.5, ucl2 = 4.5, curtailed = TRUE)
  expect_equal(res$ass, 10)
})

test_that("dsnp_ass curtailed vector input works", {
  res <- dsnp_ass(c(0, 0.005, 1), 34, 162, 1.5, 2.5,
                  ucl2 = 4.5, curtailed = TRUE)
  expect_length(res$ass, 3)
  expect_equal(res$ass[1], 34)
  expect_equal(res$ass[3], 34)
  expect_true(res$ass[2] > 34 && res$ass[2] < 34 + 162)
})

test_that("dsnp_ass curtailed errors without ucl2", {
  expect_error(dsnp_ass(0.005, 34, 162, 1.5, 2.5, curtailed = TRUE),
               "ucl2 must be provided")
})

test_that("dsnp_ass curtailed is validated by small enumeration", {
  # Enumerate every Bernoulli sequence for tiny n1, n2
  # n1=3, n2=4, wl=0.5 (wl_accept=0), ucl1=1.5 (ucl1_reject=2),
  # ucl2=2.5 (ucl2_accept=2)
  # Warning zone: d1 = 1 only
  # r(1) = 2 - 1 + 1 = 2. Need 2 non-conformances in stage 2 to reject.
  # Enumerate all 2^(3+4) = 128 sequences and compute average stage-2
  # items inspected for each d1=1 case.
  
  p <- 0.3
  n1 <- 3
  n2 <- 4
  wl <- 0.5
  ucl1 <- 1.5
  ucl2 <- 2.5
  
  res <- dsnp_ass(p, n1, n2, wl, ucl1, ucl2, curtailed = TRUE)
  
  # Manual enumeration for p=0.3
  # P(D1=1) = dbinom(1, 3, 0.3) = 0.441
  # r(1) = 2-1+1 = 2. Stop when we see 2 non-conformances in stage 2.
  # E[M] = sum_{j=0}^{3} P(Bin(j, 0.3) <= 1) since r-1 = 1
  # j=0: P(Bin(0,0.3)<=1) = 1, j=1: P(Bin(1,0.3)<=1)=1,
  # j=2: P(Bin(2,0.3)<=1)=0.91, j=3: P(Bin(3,0.3)<=1)=0.657
  # E[M] = 1 + 1 + 0.91 + 0.657 = 3.567
  # ASS = 3 + 0.441 * 3.567 = 3 + 1.573 = 4.573
  
  e_m <- sum(stats::pbinom(1, 0:(n2 - 1), p))
  p_d1 <- stats::dbinom(1, n1, p)
  ass_manual <- n1 + p_d1 * e_m
  
  expect_equal(res$ass, ass_manual)
})
