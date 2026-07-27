# Scientific validation of u-chart Cornish-Fisher limits
#
# Evidence type: independent derivation from Poisson cumulants.
#
# Issue #97. Hardens #89 and contributes to #10/#91.

u_validation_fixture <- function() {
  path <- system.file(
    "extdata", "validation", "u_chart_cf_derivation.csv",
    package = "IQCC"
  )
  if (!nzchar(path))
    stop("u-chart validation fixture was not installed")
  read.csv(path, stringsAsFactors = FALSE)
}

oracle_u_normal <- function(lambda, n, alpha = 0.0027,
                            sides = "two.sided", truncate = TRUE) {
  z_upper <- if (sides == "two.sided") {
    qnorm(1 - alpha / 2)
  } else {
    qnorm(1 - alpha)
  }
  z_lower <- if (sides == "two.sided") -z_upper else NA_real_
  sd <- sqrt(lambda / n)

  lcl <- if (sides == "two.sided") lambda + z_lower * sd else 0
  ucl <- lambda + z_upper * sd
  if (truncate)
    lcl <- max(0, lcl)

  list(lcl = lcl, ucl = ucl)
}

oracle_u_cf1 <- function(lambda, n, alpha = 0.0027,
                         sides = "two.sided", truncate = TRUE) {
  z_upper <- if (sides == "two.sided") {
    qnorm(1 - alpha / 2)
  } else {
    qnorm(1 - alpha)
  }
  z_lower <- if (sides == "two.sided") -z_upper else NA_real_
  sd <- sqrt(lambda / n)

  evaluate <- function(z)
    lambda + z * sd + (z^2 - 1) / (6 * n)

  lcl <- if (sides == "two.sided") evaluate(z_lower) else 0
  ucl <- evaluate(z_upper)
  if (truncate)
    lcl <- max(0, lcl)

  list(lcl = lcl, ucl = ucl)
}

oracle_u_cf2 <- function(lambda, n, alpha = 0.0027,
                         sides = "two.sided", truncate = TRUE) {
  z_upper <- if (sides == "two.sided") {
    qnorm(1 - alpha / 2)
  } else {
    qnorm(1 - alpha)
  }
  z_lower <- if (sides == "two.sided") -z_upper else NA_real_
  sd <- sqrt(lambda / n)

  evaluate <- function(z) {
    lambda + z * sd + (z^2 - 1) / (6 * n) +
      z * (1 - z^2) / (72 * n * sqrt(lambda * n))
  }

  lcl <- if (sides == "two.sided") evaluate(z_lower) else 0
  ucl <- evaluate(z_upper)
  if (truncate)
    lcl <- max(0, lcl)

  list(lcl = lcl, ucl = ucl)
}

oracle_u_limits <- function(lambda, n, alpha, method,
                            sides = "two.sided", truncate = TRUE) {
  switch(
    method,
    normal = oracle_u_normal(lambda, n, alpha, sides, truncate),
    cf1 = oracle_u_cf1(lambda, n, alpha, sides, truncate),
    cf2 = oracle_u_cf2(lambda, n, alpha, sides, truncate),
    stop(sprintf("unsupported method: %s", method))
  )
}

oracle_u_risk <- function(lambda, n, lcl, ucl) {
  mu <- lambda * n
  lower_cut <- ceiling(n * lcl) - 1
  upper_cut <- floor(n * ucl)

  lower <- if (lcl <= 0 || lower_cut < 0) {
    0
  } else {
    ppois(lower_cut, lambda = mu)
  }

  upper <- ppois(upper_cut, lambda = mu, lower.tail = FALSE)
  lower + upper
}

test_that("u-chart derivation fixture is reusable", {
  fixtures <- u_validation_fixture()

  expect_equal(nrow(fixtures), 12)
  expect_true(all(fixtures$evidence_type == "independent_derivation"))

  for (i in seq_len(nrow(fixtures))) {
    row <- fixtures[i, ]
    oracle <- oracle_u_limits(
      row$lambda, row$n, row$alpha, row$method, row$sides
    )
    risk <- oracle_u_risk(
      row$lambda, row$n, oracle$lcl, oracle$ucl
    )

    expect_equal(
      oracle$lcl, row$expected_lcl,
      tolerance = row$tolerance
    )
    expect_equal(
      oracle$ucl, row$expected_ucl,
      tolerance = row$tolerance
    )
    expect_equal(
      risk, row$expected_risk,
      tolerance = row$tolerance
    )
  }
})

test_that("production u-chart limits match full Cornish-Fisher quantiles", {
  fixtures <- u_validation_fixture()

  for (i in seq_len(nrow(fixtures))) {
    row <- fixtures[i, ]
    oracle <- oracle_u_limits(
      row$lambda, row$n, row$alpha, row$method, row$sides
    )
    production <- uchart_limits(
      row$lambda, row$n, alpha = row$alpha,
      type = row$method, sides = row$sides
    )

    expect_equal(production$lcl, oracle$lcl, tolerance = 1e-12)
    expect_equal(production$ucl, oracle$ucl, tolerance = 1e-12)
  }
})

test_that("CF2 lower limit evaluates the correction at the lower quantile", {
  lambda <- 0.5
  n <- 20
  alpha <- 0.0027
  z <- qnorm(1 - alpha / 2)
  sd <- sqrt(lambda / n)

  proper_lcl <- lambda - z * sd + (z^2 - 1) / (6 * n) -
    z * (1 - z^2) / (72 * n * sqrt(lambda * n))
  old_same_sign_lcl <- lambda - z * sd + (z^2 - 1) / (6 * n) +
    z * (1 - z^2) / (72 * n * sqrt(lambda * n))

  production <- uchart_limits(
    lambda, n, alpha = alpha, type = "cf2", truncate = FALSE
  )

  expect_equal(production$lcl, proper_lcl, tolerance = 1e-12)
  expect_gt(abs(production$lcl - old_same_sign_lcl), 1e-8)
})

test_that("CF2 at z=3 recovers both historical tail formulas", {
  alpha3 <- 2 * (1 - pnorm(3))
  grid <- expand.grid(
    lambda = c(0.05, 0.10, 0.50, 1.40),
    n = c(5, 10, 20, 50),
    KEEP.OUT.ATTRS = FALSE
  )

  for (i in seq_len(nrow(grid))) {
    lambda <- grid$lambda[i]
    n <- grid$n[i]
    limits <- uchart_limits(
      lambda, n, alpha = alpha3, type = "cf2", truncate = FALSE
    )

    expected_ucl <- lambda + 3 * sqrt(lambda / n) +
      4 / (3 * n) - 1 / (3 * n * sqrt(lambda * n))
    expected_lcl <- lambda - 3 * sqrt(lambda / n) +
      4 / (3 * n) + 1 / (3 * n * sqrt(lambda * n))

    expect_equal(limits$ucl, expected_ucl, tolerance = 1e-12)
    expect_equal(limits$lcl, expected_lcl, tolerance = 1e-12)
  }
})

test_that("Poisson risk oracle uses exact distribution tails", {
  fixtures <- u_validation_fixture()

  for (i in seq_len(nrow(fixtures))) {
    row <- fixtures[i, ]
    limits <- uchart_limits(
      row$lambda, row$n, alpha = row$alpha,
      type = row$method, sides = row$sides
    )
    oracle <- oracle_u_risk(
      row$lambda, row$n, limits$lcl, limits$ucl
    )
    production <- uchart_alpha_risk(
      row$lambda, row$n, limits$lcl, limits$ucl
    )

    expect_equal(production, oracle, tolerance = 1e-12)
  }
})

test_that("Phase II standardized statistics use the estimated rate", {
  x <- c(5, 6, 7, 8, 9, 10)
  n <- 10
  lambda_hat <- sum(x) / (length(x) * n)
  expected <- (x / n - lambda_hat) / sqrt(lambda_hat / n)

  chart <- cchart.u(x1 = x, n1 = n, type = "standardized")
  expect_equal(as.numeric(chart$statistics), expected, tolerance = 1e-12)
})
