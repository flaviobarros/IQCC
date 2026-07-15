# --- dsnp_design tests ---

# --- 1. Structure and class ---

test_that("dsnp_design returns object of class dsnp_design", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "arl1")
  expect_s3_class(res, "dsnp_design")
  expect_type(res, "list")
  expect_named(res, c("best", "candidates", "parameters", "search", "failures"))
})

test_that("dsnp_design best is a one-row data.frame", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "arl1")
  expect_s3_class(res$best, "data.frame")
  expect_equal(nrow(res$best), 1)
})

test_that("dsnp_design candidates contains required columns", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "arl1")
  expected_cols <- c("n1", "n2", "wl", "ucl1", "ucl2",
                     "wl_accept", "ucl1_reject", "ucl2_accept",
                     "p_signal0", "arl0", "ass0",
                     "p_signal1", "arl1", "ass1",
                     "feasible", "objective")
  for(col in expected_cols)
    expect_true(col %in% names(res$candidates),
                info = paste("Missing column:", col))
})

test_that("dsnp_design parameters stores all inputs", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 100, alpha = 0.01,
                     objective = "weighted",
                     weights = c(arl1 = 2, ass0 = 1),
                     allow_empty_warning = TRUE,
                     max_results = 5, ass0_max = 7)
  expect_equal(res$parameters$p0, 0.05)
  expect_equal(res$parameters$p1, 0.10)
  expect_equal(res$parameters$arl0_min, 100)
  expect_equal(res$parameters$alpha, 0.01)
  expect_equal(res$parameters$ass0_max, 7)
  expect_equal(res$parameters$objective, "weighted")
  expect_equal(res$parameters$weights, c(arl1 = 2, ass0 = 1))
  expect_true(res$parameters$allow_empty_warning)
  expect_equal(res$parameters$max_results, 5)
})

test_that("dsnp_design search counts are present", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "arl1")
  expect_type(res$search, "list")
  expect_true(is.integer(res$search$n_pairs_evaluated))
  expect_true(is.integer(res$search$n_candidates_evaluated))
  expect_true(is.integer(res$search$n_feasible))
  expect_true(is.integer(res$search$n_failed_pairs))
})

# --- 2. Deterministic ordering ---

test_that("dsnp_design ordering is deterministic", {
  res1 <- dsnp_design(p0 = 0.05, p1 = 0.10,
                      n1_range = 5:6, n2_range = 8:9,
                      arl0_min = 50, objective = "arl1")
  res2 <- dsnp_design(p0 = 0.05, p1 = 0.10,
                      n1_range = 5:6, n2_range = 8:9,
                      arl0_min = 50, objective = "arl1")
  expect_equal(res1$candidates, res2$candidates)
})

# --- 3. Full argument validation ---

test_that("dsnp_design errors on invalid p0", {
  expect_error(dsnp_design(p0 = -0.1, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9, arl0_min = 50),
               "p0 must be a finite scalar in \\(0, 1\\)")
  expect_error(dsnp_design(p0 = 1.1, p1 = 0.50,
               n1_range = 5:6, n2_range = 8:9, arl0_min = 50),
               "p0 must be a finite scalar in \\(0, 1\\)")
  expect_error(dsnp_design(p0 = 0, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9, arl0_min = 50),
               "p0 must be a finite scalar in \\(0, 1\\)")
  expect_error(dsnp_design(p0 = 1, p1 = 0.50,
               n1_range = 5:6, n2_range = 8:9, arl0_min = 50),
               "p0 must be a finite scalar in \\(0, 1\\)")
  expect_error(dsnp_design(p0 = NA, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9, arl0_min = 50),
               "p0 must be a finite scalar in \\(0, 1\\)")
  expect_error(dsnp_design(p0 = NaN, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9, arl0_min = 50),
               "p0 must be a finite scalar in \\(0, 1\\)")
  expect_error(dsnp_design(p0 = Inf, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9, arl0_min = 50),
               "p0 must be a finite scalar in \\(0, 1\\)")
})

test_that("dsnp_design errors on invalid p1", {
  expect_error(dsnp_design(p0 = 0.05, p1 = -0.1,
               n1_range = 5:6, n2_range = 8:9, arl0_min = 50),
               "p1 must be a finite scalar in \\(0, 1\\)")
  expect_error(dsnp_design(p0 = 0.05, p1 = 0,
               n1_range = 5:6, n2_range = 8:9, arl0_min = 50),
               "p1 must be a finite scalar in \\(0, 1\\)")
})

# --- 4. p1 > p0 ---

test_that("dsnp_design requires p1 > p0", {
  expect_error(dsnp_design(p0 = 0.10, p1 = 0.05,
               n1_range = 5:6, n2_range = 8:9, arl0_min = 50),
               "p1 must be greater than p0")
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.05,
               n1_range = 5:6, n2_range = 8:9, arl0_min = 50),
               "p1 must be greater than p0")
})

# --- 5. Range deduplication and sorting ---

test_that("dsnp_design deduplicates and sorts ranges", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = c(6, 5, 6, 5),
                     n2_range = c(9, 8, 9),
                     arl0_min = 50, objective = "arl1")
  # Should have evaluated 2 n1 values * 2 n2 values = 4 pairs
  expect_equal(res$search$n_pairs_evaluated, 4L)
})

# --- 6. arl0_min respected ---

test_that("dsnp_design respects arl0_min", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "arl1")
  expect_true(all(res$candidates$arl0 >= 50))
})

# --- 7. alpha respected ---

# When alpha is supplied alone (arl0_min = NULL), only p_signal0 <= alpha
# is enforced. If both alpha and arl0_min are given, both constraints apply.
test_that("dsnp_design respects alpha alone", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     alpha = 0.01, arl0_min = NULL, objective = "arl1")
  expect_true(all(res$candidates$p_signal0 <= 0.01))
  expect_null(res$parameters$arl0_min)
})

# --- 8. Simultaneous arl0_min and alpha ---

test_that("dsnp_design respects both arl0_min and alpha simultaneously", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:7, n2_range = 8:10,
                     arl0_min = 100, alpha = 0.01,
                     objective = "arl1")
  expect_true(all(res$candidates$arl0 >= 100))
  expect_true(all(res$candidates$p_signal0 <= 0.01))
})

test_that("dsnp_design ass0_max filters candidates and is recorded", {
  unrestricted <- dsnp_design(
    p0 = 0.05, p1 = 0.10,
    n1_range = 5:6, n2_range = 8:9,
    arl0_min = 50, max_results = 100
  )
  explicit_null <- dsnp_design(
    p0 = 0.05, p1 = 0.10,
    n1_range = 5:6, n2_range = 8:9,
    arl0_min = 50, ass0_max = NULL, max_results = 100
  )
  constrained <- dsnp_design(
    p0 = 0.05, p1 = 0.10,
    n1_range = 5:6, n2_range = 8:9,
    arl0_min = 50, ass0_max = 5.4, max_results = 100
  )

  expect_null(unrestricted$parameters$ass0_max)
  expect_equal(explicit_null$candidates, unrestricted$candidates)
  expect_equal(explicit_null$search, unrestricted$search)
  expect_equal(constrained$parameters$ass0_max, 5.4)
  expect_true(all(constrained$candidates$ass0 <= 5.4))
  expect_lt(constrained$search$n_feasible, unrestricted$search$n_feasible)
})

test_that("dsnp_design combines ass0_max with arl0_min and alpha", {
  res <- dsnp_design(
    p0 = 0.05, p1 = 0.10,
    n1_range = 5:7, n2_range = 8:10,
    arl0_min = 100, alpha = 0.01, ass0_max = 6.2,
    max_results = 100
  )

  expect_true(all(res$candidates$arl0 >= 100))
  expect_true(all(res$candidates$p_signal0 <= 0.01))
  expect_true(all(res$candidates$ass0 <= 6.2))
})

# --- 9. objective = arl1 returns lowest arl1 ---

test_that("dsnp_design objective=arl1 returns lowest arl1", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "arl1")
  best_arl1 <- res$best$arl1
  expect_equal(best_arl1, min(res$candidates$arl1))
})

test_that("dsnp_design objective=arl1 sorts arl1 ascending", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:7, n2_range = 8:10,
                     arl0_min = 50, objective = "arl1")
  expect_true(!is.unsorted(res$candidates$arl1))
})

# --- 10. objective = ass0 returns lowest ass0 ---

test_that("dsnp_design objective=ass0 returns lowest ass0", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "ass0")
  best_ass0 <- res$best$ass0
  expect_equal(best_ass0, min(res$candidates$ass0))
})

test_that("dsnp_design objective=ass0 sorts ass0 ascending", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:7, n2_range = 8:10,
                     arl0_min = 50, objective = "ass0")
  expect_true(!is.unsorted(res$candidates$ass0))
})

# --- 11. objective = weighted computes normalized score correctly ---

test_that("dsnp_design objective=weighted computes score correctly", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "weighted")
  cand <- res$candidates
  expect_true("score" %in% names(cand))
  expect_true("arl1_scaled" %in% names(cand))
  expect_true("ass0_scaled" %in% names(cand))

  # Scaled values must be in [0, 1]
  expect_true(all(cand$arl1_scaled >= 0 & cand$arl1_scaled <= 1))
  expect_true(all(cand$ass0_scaled >= 0 & cand$ass0_scaled <= 1))

  # score = w1 * arl1_scaled + w2 * ass0_scaled with default weights
  expected_score <- cand$arl1_scaled + cand$ass0_scaled
  expect_equal(cand$score, expected_score, tolerance = 1e-10)

  # Candidates must be sorted by score ascending
  expect_true(!is.unsorted(cand$score))
})

test_that("dsnp_design objective=weighted respects weights", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "weighted",
                     weights = c(arl1 = 2, ass0 = 1))
  cand <- res$candidates

  # score = 2 * arl1_scaled + 1 * ass0_scaled
  expected_score <- 2 * cand$arl1_scaled + 1 * cand$ass0_scaled
  expect_equal(cand$score, expected_score, tolerance = 1e-10)

  # Scaled values must be in [0, 1]
  expect_true(all(cand$arl1_scaled >= 0 & cand$arl1_scaled <= 1))
  expect_true(all(cand$ass0_scaled >= 0 & cand$ass0_scaled <= 1))

  # Candidates sorted by score
  expect_true(!is.unsorted(cand$score))
})

# --- 12. Constant scale in weighted normalization ---

test_that("dsnp_design weighted handles constant arl1 or ass0", {
  # Force constant arl1 by overriding dsnp_limits to return identical arl1
  real_dsnp_limits <- dsnp_limits
  mock_dsnp_limits <- function(p0, n1, n2, ...) {
    res <- real_dsnp_limits(p0 = p0, n1 = n1, n2 = n2, ...)
    res$candidates$arl1 <- 100
    res$best <- res$candidates[1, , drop = FALSE]
    res
  }
  testthat::local_mocked_bindings(dsnp_limits = mock_dsnp_limits)

  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "weighted")
  cand <- res$candidates

  # All arl1 values are identical, so arl1_scaled must be 0
  expect_true(all(cand$arl1 == 100))
  expect_true(all(cand$arl1_scaled == 0))

  # ass0 still varies, so ass0_scaled is not all zero
  expect_true(length(unique(cand$ass0)) > 1)

  # score reduces to weight_ass0 * ass0_scaled
  expect_equal(cand$score, 1 * cand$ass0_scaled, tolerance = 1e-10)
})

# --- 13. best equals first row of candidates ---

test_that("dsnp_design best is first row of candidates", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:7, n2_range = 8:10,
                     arl0_min = 50, objective = "arl1")
  expect_equal(res$best$n1, res$candidates$n1[1])
  expect_equal(res$best$n2, res$candidates$n2[1])
  expect_equal(res$best$wl, res$candidates$wl[1])
  expect_equal(res$best$ucl1, res$candidates$ucl1[1])
  expect_equal(res$best$ucl2, res$candidates$ucl2[1])
  expect_equal(res$best$arl1, res$candidates$arl1[1])
})

# --- 14. max_results limits output only ---

test_that("dsnp_design max_results limits output, not search", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:7, n2_range = 8:10,
                     arl0_min = 50, objective = "arl1",
                     max_results = 3)
  expect_true(nrow(res$candidates) <= 3)
  # But n_feasible should be >= nrow(candidates)
  expect_true(res$search$n_feasible >= nrow(res$candidates))
})

# --- 15. failures recorded without aborting ---

test_that("dsnp_design records failures without aborting", {
  # Mock dsnp_limits to fail on (n1=6, n2=8) and succeed otherwise
  real_dsnp_limits <- dsnp_limits
  mock_dsnp_limits <- function(p0, n1, n2, ...) {
    if(n1 == 6 && n2 == 8)
      stop("mock failure for n1=6, n2=8")
    real_dsnp_limits(p0 = p0, n1 = n1, n2 = n2, ...)
  }
  testthat::local_mocked_bindings(dsnp_limits = mock_dsnp_limits)

  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "arl1")

  # One pair failed, three succeeded
  expect_equal(res$search$n_failed_pairs, 1L)
  expect_equal(res$search$n_pairs_evaluated, 4L)

  # failures data.frame has the right structure and content
  expect_s3_class(res$failures, "data.frame")
  expect_equal(nrow(res$failures), 1)
  expect_equal(res$failures$n1, 6)
  expect_equal(res$failures$n2, 8)
  expect_true(grepl("mock failure", res$failures$message))

  # Search still produced results from the other 3 pairs
  expect_true(nrow(res$candidates) > 0)
})

# --- 16. Result matches manual enumeration ---

test_that("dsnp_design matches manual dsnp_limits enumeration", {
  p0 <- 0.05
  p1 <- 0.10
  n1_range <- 5:6
  n2_range <- 8:9
  arl0_min <- 50
  alpha_for_limits <- 1 / arl0_min

  # Manual enumeration
  manual_results <- list()
  for(n1 in n1_range)
  {
    for(n2 in n2_range)
    {
      lim <- dsnp_limits(p0 = p0, n1 = n1, n2 = n2,
                         alpha = alpha_for_limits, p1 = p1,
                         conservative = TRUE,
                         allow_empty_warning = FALSE,
                         max_results = .Machine$integer.max)
      cand <- lim$candidates
      cand$n1 <- n1
      cand$n2 <- n2
      manual_results[[length(manual_results) + 1]] <- cand
    }
  }
  manual_all <- do.call(rbind, manual_results)
  manual_all$feasible <- manual_all$arl0 >= arl0_min
  manual_feasible <- manual_all[manual_all$feasible, ]

  # Design search
  res <- dsnp_design(p0 = p0, p1 = p1,
                     n1_range = n1_range, n2_range = n2_range,
                     arl0_min = arl0_min, objective = "arl1")

  # Same number of feasible candidates (search counts, not truncated output)
  expect_equal(res$search$n_feasible, as.integer(nrow(manual_feasible)))
})

test_that("dsnp_design matches an independent small-sample enumeration", {
  direct_performance <- function(p, n1, n2, a, b, c) {
    acceptance <- 0
    p_second <- 0

    for(d1 in 0:n1) {
      p_d1 <- dbinom(d1, n1, p)
      if(d1 <= a) {
        acceptance <- acceptance + p_d1
      } else if(d1 < b) {
        p_second <- p_second + p_d1
        for(d2 in 0:n2) {
          if(d1 + d2 <= c)
            acceptance <- acceptance + p_d1 * dbinom(d2, n2, p)
        }
      }
    }

    p_signal <- min(1, max(0, 1 - acceptance))
    c(
      arl = if(p_signal == 0) Inf else 1 / p_signal,
      ass = n1 + n2 * p_second,
      p_signal = p_signal
    )
  }

  p0 <- 0.05
  p1 <- 0.15
  arl0_min <- 47.3
  ass0_max <- 4.25
  rows <- list()

  for(n1 in 3:4) {
    for(n2 in 4:5) {
      for(a in 0:n1) {
        b_min <- a + 2L
        b_max <- n1 + 1L
        if(b_min > b_max)
          next

        for(b in seq.int(b_min, b_max)) {
          for(c in seq.int(a + 1L, n1 + n2)) {
            perf0 <- direct_performance(p0, n1, n2, a, b, c)
            perf1 <- direct_performance(p1, n1, n2, a, b, c)
            rows[[length(rows) + 1L]] <- data.frame(
              n1 = n1,
              n2 = n2,
              wl = a + 0.5,
              ucl1 = b - 0.5,
              ucl2 = c + 0.5,
              wl_accept = a,
              ucl1_reject = b,
              ucl2_accept = c,
              arl0 = unname(perf0["arl"]),
              ass0 = unname(perf0["ass"]),
              p_signal0 = unname(perf0["p_signal"]),
              arl1 = unname(perf1["arl"])
            )
          }
        }
      }
    }
  }

  direct <- do.call(rbind, rows)
  direct <- direct[
    direct$arl0 >= arl0_min & direct$ass0 <= ass0_max,
    , drop = FALSE
  ]
  direct <- direct[order(
    direct$arl1,
    direct$ass0,
    -direct$arl0,
    direct$n1 + direct$n2,
    direct$n1,
    direct$n2,
    direct$wl_accept,
    direct$ucl1_reject,
    direct$ucl2_accept
  ), , drop = FALSE]
  rownames(direct) <- NULL

  design <- dsnp_design(
    p0 = p0,
    p1 = p1,
    n1_range = 3:4,
    n2_range = 4:5,
    arl0_min = arl0_min,
    ass0_max = ass0_max,
    objective = "arl1",
    max_results = 1000
  )

  keys <- c("n1", "n2", "wl", "ucl1", "ucl2", "arl0", "arl1", "ass0")
  expect_gt(nrow(direct), 0)
  expect_equal(design$search$n_feasible, as.integer(nrow(direct)))
  expect_equal(design$best[, keys], direct[1, keys], tolerance = 1e-12)
})

# --- 17. Each line consistent with direct core calls ---

test_that("dsnp_design each candidate is consistent with direct core calls", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "arl1")
  cand <- res$candidates

  for(i in seq_len(nrow(cand)))
  {
    row <- cand[i, ]

    pa <- dsnp_prob_accept(res$parameters$p0, row$n1, row$n2,
                           row$wl, row$ucl1, row$ucl2)
    arl <- dsnp_arl(res$parameters$p0, row$n1, row$n2,
                    row$wl, row$ucl1, row$ucl2)
    ass <- dsnp_ass(res$parameters$p0, row$n1, row$n2,
                    row$wl, row$ucl1)

    expect_equal(row$p_signal0, pa$p_signal, tolerance = 1e-10,
                 info = paste("Candidate", i, "p_signal0 mismatch"))
    expect_equal(row$arl0, arl$arl, tolerance = 1e-10,
                 info = paste("Candidate", i, "arl0 mismatch"))
    expect_equal(row$ass0, ass$ass, tolerance = 1e-10,
                 info = paste("Candidate", i, "ass0 mismatch"))

    arl1 <- dsnp_arl(res$parameters$p1, row$n1, row$n2,
                     row$wl, row$ucl1, row$ucl2)
    expect_equal(row$arl1, arl1$arl, tolerance = 1e-10,
                 info = paste("Candidate", i, "arl1 mismatch"))
  }
})

# --- 18. print.dsnp_design works ---

test_that("print.dsnp_design produces compact summary", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "arl1")
  output <- capture.output(print(res))
  expect_true(length(output) > 0)
  expect_true(any(grepl("DS-np Complete Design Search", output)))
  expect_true(any(grepl("Best design:", output)))
  expect_true(any(grepl("p0", output)))
  expect_true(any(grepl("n1", output)))
})

# --- 19. Reproducibility independent of range order ---

test_that("dsnp_design is reproducible regardless of range order", {
  res1 <- dsnp_design(p0 = 0.05, p1 = 0.10,
                      n1_range = c(7, 5, 6), n2_range = c(10, 8, 9),
                      arl0_min = 50, objective = "arl1")
  res2 <- dsnp_design(p0 = 0.05, p1 = 0.10,
                      n1_range = 5:7, n2_range = 8:10,
                      arl0_min = 50, objective = "arl1")
  expect_equal(res1$candidates, res2$candidates)
})

# --- Validation edge cases ---

test_that("dsnp_design errors on no arl0_min and no alpha", {
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9,
               arl0_min = NULL, alpha = NULL),
               "at least one of arl0_min or alpha must be provided")
})

test_that("dsnp_design errors on invalid arl0_min", {
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9,
               arl0_min = 1),
               "arl0_min must be a finite scalar greater than 1")
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9,
               arl0_min = -5),
               "arl0_min must be a finite scalar greater than 1")
})

test_that("dsnp_design errors on invalid alpha", {
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9,
               alpha = 0),
               "alpha must be NULL or a finite scalar in \\(0, 1\\)")
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9,
               alpha = 1),
               "alpha must be NULL or a finite scalar in \\(0, 1\\)")
})

test_that("dsnp_design errors on invalid ass0_max", {
  invalid <- list(0, -1, Inf, NA_real_, NaN, c(5, 6), "5")

  for(value in invalid) {
    expect_error(
      dsnp_design(
        p0 = 0.05, p1 = 0.10,
        n1_range = 5:6, n2_range = 8:9,
        arl0_min = 50, ass0_max = value
      ),
      "ass0_max must be NULL or a finite positive scalar"
    )
  }
})

test_that("dsnp_design infeasibility reports ass0_max", {
  expect_error(
    dsnp_design(
      p0 = 0.05, p1 = 0.10,
      n1_range = 5:6, n2_range = 8:9,
      arl0_min = 50, ass0_max = 1
    ),
    "ass0_max = 1"
  )
})

test_that("dsnp_design infeasibility reports NULL constraints explicitly", {
  expect_error(
    dsnp_design(
      p0 = 0.05, p1 = 0.10,
      n1_range = 5:6, n2_range = 8:9,
      arl0_min = NULL, alpha = 0.01, ass0_max = 1
    ),
    "arl0_min = NULL, alpha = 0.01, ass0_max = 1"
  )
})

test_that("dsnp_design errors on invalid objective", {
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9,
               arl0_min = 50, objective = "invalid"))
})

test_that("dsnp_design errors on invalid weights", {
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9,
               arl0_min = 50, objective = "weighted",
               weights = c(arl1 = 0, ass0 = 0)),
               "at least one weight must be positive")
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9,
               arl0_min = 50, objective = "weighted",
               weights = c(arl1 = -1, ass0 = 1)),
               "weights must be finite and non-negative")
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9,
               arl0_min = 50, objective = "weighted",
               weights = c(x = 1, y = 2)),
               "weights must have names")
})

test_that("dsnp_design errors on invalid max_results", {
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9,
               arl0_min = 50, max_results = 0),
               "max_results must be a positive integer")
})

test_that("dsnp_design errors on progress = NA", {
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9,
               arl0_min = 50, progress = NA),
               "progress must be a logical scalar")
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = 8:9,
               arl0_min = 50, progress = NA_real_),
               "progress must be a logical scalar")
})

test_that("dsnp_design errors on empty ranges", {
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = integer(0), n2_range = 8:9,
               arl0_min = 50),
               "n1_range must be a non-empty vector")
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = integer(0),
               arl0_min = 50),
               "n2_range must be a non-empty vector")
})

test_that("dsnp_design errors on non-integer ranges", {
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = c(5.5, 6), n2_range = 8:9,
               arl0_min = 50),
               "n1_range must contain positive integers")
  expect_error(dsnp_design(p0 = 0.05, p1 = 0.10,
               n1_range = 5:6, n2_range = c(8, NA),
               arl0_min = 50),
               "n2_range must contain positive integers")
})

test_that("dsnp_design weighted has score, arl1_scaled, ass0_scaled columns", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:7, n2_range = 8:10,
                     arl0_min = 50, objective = "weighted")
  expect_true("score" %in% names(res$candidates))
  expect_true("arl1_scaled" %in% names(res$candidates))
  expect_true("ass0_scaled" %in% names(res$candidates))
})

test_that("dsnp_design arl1 and ass0 objectives do not have score columns", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "arl1")
  expect_false("score" %in% names(res$candidates))
  expect_false("arl1_scaled" %in% names(res$candidates))
  expect_false("ass0_scaled" %in% names(res$candidates))
})

# --- .scale_minimize unit tests ---

test_that(".scale_minimize finite values use min-max scaling", {
  expect_equal(IQCC:::.scale_minimize(c(10, 20, 30)), c(0, 0.5, 1))
})

test_that(".scale_minimize assigns scale 1 to +Inf", {
  expect_equal(IQCC:::.scale_minimize(c(10, 20, Inf)), c(0, 1, 1))
})

test_that(".scale_minimize constant finite produces zeros", {
  expect_equal(IQCC:::.scale_minimize(c(10, 10, Inf)), c(0, 0, 1))
})

test_that(".scale_minimize all +Inf produces ones", {
  expect_equal(IQCC:::.scale_minimize(c(Inf, Inf)), c(1, 1))
})

test_that(".scale_minimize rejects NA", {
  expect_error(IQCC:::.scale_minimize(c(10, NA, 20)),
               "objective values contain invalid non-finite values")
})

test_that(".scale_minimize rejects NaN", {
  expect_error(IQCC:::.scale_minimize(c(10, NaN, 20)),
               "objective values contain invalid non-finite values")
})

test_that(".scale_minimize rejects -Inf", {
  expect_error(IQCC:::.scale_minimize(c(10, -Inf, 20)),
               "objective values contain invalid non-finite values")
})

.mock_dsnp_design_candidates <- function(arl1, ass0)
{
  n <- length(arl1)
  p_signal1 <- ifelse(is.finite(arl1), 1 / arl1, 0)

  data.frame(
    wl = seq_len(n) - 0.5,
    ucl1 = seq_len(n) + 1.5,
    ucl2 = seq_len(n) + 2.5,
    wl_accept = seq_len(n) - 1L,
    ucl1_reject = seq_len(n) + 1L,
    ucl2_accept = seq_len(n) + 2L,
    pt0 = rep(0.995, n),
    p_signal0 = rep(0.005, n),
    arl0 = rep(200, n),
    ass0 = ass0,
    feasible = rep(TRUE, n),
    pt1 = 1 - p_signal1,
    p_signal1 = p_signal1,
    arl1 = arl1,
    ass1 = ass0,
    stringsAsFactors = FALSE
  )
}

.mock_dsnp_limits_factory <- function(candidates)
{
  force(candidates)
  function(p0, n1, n2, ...) {
    list(
      best = candidates[1, , drop = FALSE],
      candidates = candidates
    )
  }
}

test_that("dsnp_design weighted handles zero arl1 weight with real candidates", {
  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5:6, n2_range = 8:9,
                     arl0_min = 50, objective = "weighted",
                     weights = c(arl1 = 0, ass0 = 1),
                     max_results = 1000)
  cand <- res$candidates

  expect_gt(res$search$n_feasible, 1L)
  expect_equal(nrow(cand), res$search$n_feasible)
  expect_true(all(is.finite(cand$score)))
  expect_equal(cand$score, cand$ass0_scaled, tolerance = 1e-10)
  expect_true(!is.unsorted(cand$score))
})

test_that("dsnp_design weighted supports zero and positive weight combinations", {
  candidates <- .mock_dsnp_design_candidates(
    arl1 = c(10, 30, 50),
    ass0 = c(20, 10, 12)
  )
  testthat::local_mocked_bindings(
    dsnp_limits = .mock_dsnp_limits_factory(candidates)
  )

  arl1_only <- dsnp_design(p0 = 0.05, p1 = 0.10,
                           n1_range = 5, n2_range = 8,
                           arl0_min = 50, objective = "weighted",
                           weights = c(arl1 = 1, ass0 = 0))
  ass0_only <- dsnp_design(p0 = 0.05, p1 = 0.10,
                           n1_range = 5, n2_range = 8,
                           arl0_min = 50, objective = "weighted",
                           weights = c(arl1 = 0, ass0 = 1))
  both <- dsnp_design(p0 = 0.05, p1 = 0.10,
                      n1_range = 5, n2_range = 8,
                      arl0_min = 50, objective = "weighted",
                      weights = c(arl1 = 1, ass0 = 1))

  expect_equal(arl1_only$best$arl1, 10)
  expect_equal(ass0_only$best$ass0, 10)
  expect_equal(both$best$arl1, 30)
  expect_equal(both$best$ass0, 10)
})

test_that("dsnp_design weighted with zero arl1 weight does not penalize Inf arl1", {
  candidates <- .mock_dsnp_design_candidates(
    arl1 = c(100, Inf),
    ass0 = c(10, 1)
  )
  testthat::local_mocked_bindings(
    dsnp_limits = .mock_dsnp_limits_factory(candidates)
  )

  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5, n2_range = 8,
                     arl0_min = 50, objective = "weighted",
                     weights = c(arl1 = 0, ass0 = 1),
                     max_results = 10)

  expect_equal(nrow(res$candidates), 2)
  expect_equal(res$search$n_feasible, 2L)
  expect_equal(res$best$arl1, Inf)
  expect_equal(res$best$ass0, 1)
  expect_true(all(is.finite(res$candidates$score)))
})

test_that("dsnp_design weighted keeps finite arl1 before Inf arl1", {
  candidates <- .mock_dsnp_design_candidates(
    arl1 = c(100, Inf),
    ass0 = c(10, 1)
  )
  testthat::local_mocked_bindings(
    dsnp_limits = .mock_dsnp_limits_factory(candidates)
  )

  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5, n2_range = 8,
                     arl0_min = 50, objective = "weighted",
                     weights = c(arl1 = 0.1, ass0 = 1),
                     max_results = 10)
  cand <- res$candidates

  expect_equal(nrow(cand), 2)
  expect_true(is.finite(cand$arl1[1]))
  expect_equal(cand$arl1[2], Inf)
  expect_gt(cand$score[1], cand$score[2])
})

test_that("dsnp_design weighted handles max_results one", {
  candidates <- .mock_dsnp_design_candidates(
    arl1 = c(10, 30, 50),
    ass0 = c(20, 10, 12)
  )
  testthat::local_mocked_bindings(
    dsnp_limits = .mock_dsnp_limits_factory(candidates)
  )

  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5, n2_range = 8,
                     arl0_min = 50, objective = "weighted",
                     weights = c(arl1 = 1, ass0 = 1),
                     max_results = 1)

  expect_equal(nrow(res$candidates), 1)
  expect_equal(res$search$n_feasible, 3L)
  expect_equal(res$best, res$candidates[1, , drop = FALSE])
})

test_that("dsnp_design weighted permits all arl1 values to be Inf", {
  candidates <- .mock_dsnp_design_candidates(
    arl1 = c(Inf, Inf),
    ass0 = c(5, 1)
  )
  testthat::local_mocked_bindings(
    dsnp_limits = .mock_dsnp_limits_factory(candidates)
  )

  res <- dsnp_design(p0 = 0.05, p1 = 0.10,
                     n1_range = 5, n2_range = 8,
                     arl0_min = 50, objective = "weighted",
                     weights = c(arl1 = 1, ass0 = 1),
                     max_results = 10)

  expect_equal(nrow(res$candidates), 2)
  expect_true(all(res$candidates$arl1 == Inf))
  expect_true(all(is.finite(res$candidates$score)))
  expect_true(!is.unsorted(res$candidates$score))
  expect_equal(res$best$ass0, 1)
})
