.covariance_group <- function(S)
{
  base <- rbind(c(1, 1), c(1, -1), c(-1, 1), c(-1, -1))
  base <- sqrt(3 / 4) * base
  base %*% chol(S)
}

test_that("trv_stat detects trace changes with unchanged determinant", {
  g_identity <- .covariance_group(diag(2))
  g_trace_shift <- .covariance_group(diag(c(4, 0.25)))

  expect_equal(gv_stat(list(g_identity, g_trace_shift)), c(1, 1),
               tolerance = 1e-12)
  expect_equal(trv_stat(list(g_identity), Sigma0 = diag(2)), 6,
               tolerance = 1e-12)
  expect_equal(trv_stat(list(g_trace_shift), Sigma0 = diag(2)), 12.75,
               tolerance = 1e-12)
})

test_that("trv_stat is invariant under compatible linear transformations", {
  Sigma0 <- diag(c(2, 0.5))
  S <- matrix(c(3, 0.4, 0.4, 0.5), nrow = 2)
  A <- matrix(c(2, 0.2, 0.3, 1.5), nrow = 2)

  group <- .covariance_group(S)
  transformed_group <- group %*% t(A)
  transformed_sigma <- A %*% Sigma0 %*% t(A)

  expect_equal(
    trv_stat(list(transformed_group), Sigma0 = transformed_sigma),
    trv_stat(list(group), Sigma0 = Sigma0),
    tolerance = 1e-10
  )
})

test_that("trv_limits uses exact chi-square distribution", {
  limits <- trv_limits(n = 5, p = 3, alpha = 0.05)

  expect_equal(limits$df, 12)
  expect_equal(limits$center, 12)
  expect_equal(limits$lcl, 0)
  expect_equal(limits$ucl, stats::qchisq(0.95, df = 12))
})

test_that("trv_limits reproduces three published Table 3 rows", {
  published <- data.frame(
    reference = rep(
      "Barbosa, Gneri, and Meneguetti, research report",
      6
    ),
    table = rep(3L, 6),
    line = c(
      "N = 3, printed column 0.9980", "N = 3, printed column 0.9973",
      "N = 16, printed column 0.9980", "N = 16, printed column 0.9973",
      "N = 30, printed column 0.9980", "N = 30, printed column 0.9973"
    ),
    p = rep(3L, 6),
    subgroup_size = rep(c(3L, 16L, 30L), each = 2),
    df = rep(c(6L, 45L, 87L), each = 2),
    printed_probability = rep(c(0.9980, 0.9973), 3),
    nominal_probability = rep(c(0.9973, 0.9980), 3),
    published_value = c(20.07, 20.79, 75.88, 77.17, 128.18, 129.83),
    tolerance = rep(0.03, 6),
    stringsAsFactors = FALSE
  )

  published$calculated_value <- vapply(
    seq_len(nrow(published)),
    function(i) trv_limits(
      n = published$subgroup_size[i],
      p = published$p[i],
      alpha = 1 - published$nominal_probability[i]
    )$ucl,
    numeric(1)
  )
  published$tolerance_ratio <- abs(
    published$calculated_value - published$published_value
  ) / published$tolerance

  expect_equal(
    published$df,
    published$p * (published$subgroup_size - 1L)
  )
  expect_true(
    all(published$tolerance_ratio < 1),
    info = paste(capture.output(print(published)), collapse = "\n")
  )

  # Probabilities are intentionally not swapped in trv_limits(). The published
  # values confirm that Table 3's two probability labels are reversed.
  expect_true(all(published$printed_probability !=
                  published$nominal_probability))
})

test_that("published Case B summary matrices are partially reproduced", {
  case_a_sbar <- matrix(
    c(4.2366, 1.4773, 1.1929,
      1.4773, 6.1264, 2.3399,
      1.1929, 2.3399, 3.9335),
    nrow = 3, byrow = TRUE
  )
  case_b_sbar <- matrix(
    c(4.0112, 1.7865, 1.2484,
      1.7865, 5.8933, 1.7188,
      1.2484, 1.7188, 3.8909),
    nrow = 3, byrow = TRUE
  )
  reproduced <- data.frame(
    reference = rep(
      "Barbosa, Gneri, and Meneguetti, research report",
      2
    ),
    case = c("A", "B"),
    table = c(9L, 10L),
    line = rep("Mean covariance matrix and determinant", 2),
    p = rep(3L, 2),
    subgroup_size = rep(15L, 2),
    published_value = c(69.8438, 66.1893),
    calculated_value = c(det(case_a_sbar), det(case_b_sbar)),
    tolerance = rep(0.003, 2),
    stringsAsFactors = FALSE
  )
  reproduced$tolerance_ratio <- abs(
    reproduced$calculated_value - reproduced$published_value
  ) / reproduced$tolerance

  expect_true(
    all(reproduced$tolerance_ratio < 1),
    info = paste(capture.output(print(reproduced)), collapse = "\n")
  )
  expect_lt(
    abs(reproduced$published_value[2] /
        reproduced$published_value[1] - 1),
    0.06
  )

  case_b_ucl <- data.frame(
    reference = "Barbosa, Gneri, and Meneguetti, research report",
    figure = "9c",
    p = 3L,
    subgroup_size = 15L,
    df = 42L,
    nominal_probability = 0.9973,
    published_value = 72.01,
    calculated_value = trv_limits(n = 15, p = 3, alpha = 0.0027)$ucl,
    tolerance = 0.03
  )
  case_b_ucl$tolerance_ratio <- abs(
    case_b_ucl$calculated_value - case_b_ucl$published_value
  ) / case_b_ucl$tolerance
  expect_lt(case_b_ucl$tolerance_ratio, 1)

  # Only four of the 70 generated subgroup rows are printed. Without the raw
  # subgroups or simulation seed, the Phase II tr(V) signals cannot be rerun.
})

test_that("trv_alpha_risk equals nominal risk for chi-square limits", {
  limits <- trv_limits(n = 8, p = 2, alpha = 0.01)
  risk <- trv_alpha_risk(n = 8, p = 2, ucl = limits$ucl)

  expect_equal(risk$alpha, 0.01, tolerance = 1e-12)
  expect_equal(risk$arl0, 100, tolerance = 1e-10)
  expect_equal(risk$method, "exact chi-square")
})

test_that("trv simulation is reproducible and preserves RNG state", {
  set.seed(99)
  before <- .Random.seed
  a <- trv_limits(8, 3, type = "simulation", nsim = 2000, seed = 42)
  after <- .Random.seed
  b <- trv_limits(8, 3, type = "simulation", nsim = 2000, seed = 42)

  expect_identical(before, after)
  expect_equal(a$ucl, b$ucl)
})

test_that("trv simulated limits are close to chi-square limits", {
  exact <- trv_limits(6, 2, alpha = 0.05)
  simulated <- trv_limits(6, 2, alpha = 0.05, type = "simulation",
                          nsim = 20000, seed = 2026)

  expect_equal(simulated$ucl, exact$ucl, tolerance = 0.35)
})

test_that("cchart.trV returns phase information and detects trace signal", {
  g_identity <- .covariance_group(diag(2))
  g_trace_shift <- .covariance_group(diag(c(4, 0.25)))

  chart <- cchart.trV(
    list(g_identity, g_identity),
    Sigma0 = diag(2),
    alpha = 0.05,
    newdata = list(g_trace_shift),
    plot = FALSE
  )

  expect_s3_class(chart, "cchart.trV")
  expect_length(chart$statistics, 3)
  expect_equal(chart$phase1, 1:2)
  expect_equal(chart$phase2, 3L)
  expect_equal(chart$out.of.control, 3L)
  expect_equal(chart$limits$df, 6)
})

test_that("cchart.trV signals only strictly above the upper limit", {
  g_identity <- .covariance_group(diag(2))
  alpha_at_statistic <- 1 - stats::pchisq(6, df = 6)
  chart <- cchart.trV(
    list(g_identity),
    Sigma0 = diag(2),
    alpha = alpha_at_statistic,
    plot = FALSE
  )

  expect_equal(chart$statistics, chart$limits$ucl, tolerance = 1e-12)
  expect_length(chart$out.of.control, 0)
})

test_that("cchart.trV can estimate Sigma0 from Phase I", {
  g_identity <- .covariance_group(diag(2))
  chart <- cchart.trV(list(g_identity, g_identity), plot = FALSE)

  expect_s3_class(chart, "cchart.trV")
  expect_equal(chart$Sigma0, diag(2), tolerance = 1e-12)
  expect_equal(chart$covariance_source, "estimated from Phase I subgroups")
})

test_that("trv inputs are validated", {
  expect_error(trv_limits(1, 2), "greater than or equal to 2")
  expect_error(trv_limits(5, 1), "greater than or equal to 2")
  expect_error(trv_limits(5, 2, alpha = 0), "between 0 and 1")
  expect_error(trv_limits(5, 2, type = "simulation", nsim = 999),
               "at least 1000")

  g_identity <- .covariance_group(diag(2))
  expect_error(trv_stat(list(g_identity), Sigma0 = diag(c(1, 0))),
               "positive definite")
  expect_error(
    trv_stat(list(g_identity), Sigma0 = matrix(c(1, 0, 0.1, 1), 2)),
    "symmetric"
  )
  expect_error(trv_stat(list(g_identity), Sigma0 = diag(3)),
               "dimension")
  expect_error(trv_alpha_risk(5, 2, ucl = -1), "nonnegative")
})
