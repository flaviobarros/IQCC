# --- dsnp_limits tests ---

test_that("dsnp_limits returns structured list", {
  res <- dsnp_limits(p0 = 0.05, n1 = 3, n2 = 5, alpha = 0.05)
  expect_type(res, "list")
  expect_named(res, c("best", "candidates", "p0", "p1", "n1", "n2",
                       "alpha", "conservative", "allow_empty_warning"))
})

test_that("dsnp_limits candidates is a data.frame", {
  res <- dsnp_limits(p0 = 0.05, n1 = 3, n2 = 5, alpha = 0.05)
  expect_s3_class(res$candidates, "data.frame")
})

test_that("dsnp_limits best matches first row of candidates", {
  res <- dsnp_limits(p0 = 0.05, n1 = 3, n2 = 5, alpha = 0.05)
  expect_equal(res$best$wl, res$candidates$wl[1])
  expect_equal(res$best$ucl1, res$candidates$ucl1[1])
  expect_equal(res$best$ucl2, res$candidates$ucl2[1])
})

test_that("dsnp_limits respects max_results", {
  res <- dsnp_limits(p0 = 0.05, n1 = 3, n2 = 5, alpha = 0.05,
                     max_results = 5)
  expect_true(nrow(res$candidates) <= 5)
})

test_that("dsnp_limits stores input parameters", {
  res <- dsnp_limits(p0 = 0.05, n1 = 3, n2 = 5, alpha = 0.10,
                     conservative = FALSE, allow_empty_warning = TRUE)
  expect_equal(res$p0, 0.05)
  expect_equal(res$n1, 3)
  expect_equal(res$n2, 5)
  expect_equal(res$alpha, 0.10)
  expect_false(res$conservative)
  expect_true(res$allow_empty_warning)
})

test_that("dsnp_limits conservative prefers feasible candidates", {
  res <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.00001,
                     conservative = TRUE, max_results = 200)
  feasible_idx <- which(res$candidates$feasible)
  infeasible_idx <- which(!res$candidates$feasible)
  if(length(feasible_idx) > 0 && length(infeasible_idx) > 0)
    expect_true(max(feasible_idx) < min(infeasible_idx))
})

test_that("dsnp_limits allow_empty_warning=FALSE filters empty zones", {
  res <- dsnp_limits(p0 = 0.05, n1 = 3, n2 = 5, alpha = 0.10,
                     allow_empty_warning = FALSE)
  expect_true(all(res$candidates$ucl1_reject >= res$candidates$wl_accept + 2))
})

test_that("dsnp_limits allow_empty_warning=TRUE permits empty zones", {
  res <- dsnp_limits(p0 = 0.05, n1 = 3, n2 = 5, alpha = 0.50,
                     allow_empty_warning = TRUE)
  expect_true(any(res$candidates$ucl1_reject == res$candidates$wl_accept + 1))
})

test_that("dsnp_limits best is consistent with numeric core", {
  res <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.05)
  b <- res$best

  arl_check <- dsnp_arl(res$p0, res$n1, res$n2,
                        b$wl, b$ucl1, b$ucl2)
  ass_check <- dsnp_ass(res$p0, res$n1, res$n2,
                        b$wl, b$ucl1)
  pa_check  <- dsnp_prob_accept(res$p0, res$n1, res$n2,
                                b$wl, b$ucl1, b$ucl2)

  expect_equal(b$arl0, arl_check$arl)
  expect_equal(b$ass0, ass_check$ass)
  expect_equal(b$p_signal0, pa_check$p_signal)
})

test_that("dsnp_limits with p1 includes out-of-control metrics", {
  res <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.05,
                     p1 = 0.10)
  expect_true("arl1" %in% names(res$candidates))
  expect_true("ass1" %in% names(res$candidates))
  expect_true("p_signal1" %in% names(res$candidates))
  expect_true("pt1" %in% names(res$candidates))
})

test_that("dsnp_limits with p1 arl1 is computed correctly", {
  res <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.05,
                     p1 = 0.10)
  b <- res$best
  arl1_check <- dsnp_arl(res$p1, res$n1, res$n2,
                         b$wl, b$ucl1, b$ucl2)
  expect_equal(b$arl1, arl1_check$arl)
})

test_that("dsnp_limits with p1 ranks by arl1", {
  res <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.05,
                     p1 = 0.10, conservative = TRUE)
  arl1_vals <- res$candidates$arl1
  feasible <- res$candidates$feasible
  if(any(feasible))
    expect_true(!is.unsorted(arl1_vals[feasible]))
})

# --- Validation error tests ---

test_that("dsnp_limits errors on invalid p0", {
  expect_error(dsnp_limits(p0 = -0.1, n1 = 5, n2 = 10),
               "p0 must be between 0 and 1")
  expect_error(dsnp_limits(p0 = 1.1, n1 = 5, n2 = 10),
               "p0 must be between 0 and 1")
})

test_that("dsnp_limits errors on invalid alpha", {
  expect_error(dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0),
               "alpha must be between 0 and 1")
  expect_error(dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 1),
               "alpha must be between 0 and 1")
})

test_that("dsnp_limits errors on invalid n1", {
  expect_error(dsnp_limits(p0 = 0.05, n1 = 1.5, n2 = 10),
               "n1 must be a positive integer")
  expect_error(dsnp_limits(p0 = 0.05, n1 = 0, n2 = 10),
               "n1 must be a positive integer")
})

test_that("dsnp_limits errors on invalid n2", {
  expect_error(dsnp_limits(p0 = 0.05, n1 = 5, n2 = 0),
               "n2 must be a positive integer")
})

test_that("dsnp_limits errors on invalid max_results", {
  expect_error(dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, max_results = 0),
               "max_results must be a positive integer")
})

test_that("dsnp_limits errors on invalid p1", {
  expect_error(dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, p1 = -0.1),
               "p1 must be between 0 and 1")
})

# --- Published example compatibility ---

test_that("dsnp_limits published example is compatible with numeric core", {
  wl <- 1.5; ucl1 <- 2.5; ucl2 <- 4.5
  n1 <- 34; n2 <- 162; p0 <- 0.005; p1 <- 0.0075

  pa0 <- dsnp_prob_accept(p0, n1, n2, wl, ucl1, ucl2)
  arl0 <- dsnp_arl(p0, n1, n2, wl, ucl1, ucl2)
  ass0 <- dsnp_ass(p0, n1, n2, wl, ucl1)
  arl1 <- dsnp_arl(p1, n1, n2, wl, ucl1, ucl2)

  expect_equal(arl0$arl, 803.41, tolerance = 0.01)
  expect_equal(arl1$arl, 193.22, tolerance = 0.01)
  expect_equal(ass0$ass, 35.94, tolerance = 0.01)
})

# --- ucl2 evaluation consistency tests ---

test_that("dsnp_limits candidates with same (wl,ucl1) but different ucl2 differ in p_signal", {
  res <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.10,
                     allow_empty_warning = FALSE, max_results = 200)
  cand <- res$candidates
  # Pick a (wl_accept, ucl1_reject) pair that has multiple ucl2 values
  ab_pairs <- unique(cand[, c("wl_accept", "ucl1_reject")])
  found_divergence <- FALSE
  for(i in seq_len(nrow(ab_pairs)))
  {
    a_val <- ab_pairs$wl_accept[i]
    b_val <- ab_pairs$ucl1_reject[i]
    subset <- cand[cand$wl_accept == a_val & cand$ucl1_reject == b_val, ]
    if(nrow(subset) >= 2)
    {
      expect_true(length(unique(subset$p_signal0)) > 1,
                  info = paste("For wl_accept=", a_val, "ucl1_reject=", b_val,
                               "all p_signal0 are identical despite different ucl2"))
      found_divergence <- TRUE
      break
    }
  }
  expect_true(found_divergence, info = "No (wl,ucl1) pair had multiple ucl2 candidates to test")
})

test_that("dsnp_limits every candidate is consistent with direct numeric core call", {
  res <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.10,
                     allow_empty_warning = FALSE, max_results = 50)
  cand <- res$candidates
  for(i in seq_len(nrow(cand)))
  {
    row <- cand[i, ]
    pa  <- dsnp_prob_accept(res$p0, res$n1, res$n2,
                            row$wl, row$ucl1, row$ucl2)
    arl <- dsnp_arl(res$p0, res$n1, res$n2,
                    row$wl, row$ucl1, row$ucl2)
    expect_equal(row$p_signal0, pa$p_signal,
                 tolerance = 1e-10,
                 info = paste("Candidate", i, "p_signal0 mismatch"))
    expect_equal(row$arl0, arl$arl,
                 tolerance = 1e-10,
                 info = paste("Candidate", i, "arl0 mismatch"))
  }
})

test_that("dsnp_limits ucl2 column matches c+0.5 for every candidate", {
  res <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.10,
                     allow_empty_warning = FALSE, max_results = 50)
  cand <- res$candidates
  # ucl2 should equal ucl2_accept + 0.5
  expect_equal(cand$ucl2, cand$ucl2_accept + 0.5)
})

test_that("dsnp_limits larger ucl2 gives lower p_signal (more accepting)", {
  res <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.10,
                     allow_empty_warning = FALSE, max_results = 200)
  cand <- res$candidates
  # For fixed (wl_accept, ucl1_reject), p_signal should be non-increasing in ucl2
  ab_pairs <- unique(cand[, c("wl_accept", "ucl1_reject")])
  for(i in seq_len(nrow(ab_pairs)))
  {
    a_val <- ab_pairs$wl_accept[i]
    b_val <- ab_pairs$ucl1_reject[i]
    subset <- cand[cand$wl_accept == a_val & cand$ucl1_reject == b_val, ]
    subset <- subset[order(subset$ucl2_accept), ]
    if(nrow(subset) >= 2)
    {
      expect_true(!is.unsorted(-subset$p_signal0),
                  info = paste("p_signal0 not non-increasing with ucl2 for",
                               "wl_accept=", a_val, "ucl1_reject=", b_val))
    }
  }
})
