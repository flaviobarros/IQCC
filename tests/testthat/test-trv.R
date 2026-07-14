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
  expect_error(trv_stat(list(g_identity), Sigma0 = diag(3)),
               "dimension")
  expect_error(trv_alpha_risk(5, 2, ucl = -1), "nonnegative")
})
