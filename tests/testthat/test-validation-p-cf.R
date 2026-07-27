# Scientific validation of p-chart Cornish-Fisher limits
#
# Published fixtures: Joekes & Barbosa (2013),
# doi:10.1016/j.conengprac.2012.12.005.
#
# Issue #97. Hardens #86 and contributes to #10/#91.

validation_fixture <- function(filename) {
  path <- system.file("extdata", "validation", filename, package = "IQCC")
  if (!nzchar(path))
    stop(sprintf("validation fixture not installed: %s", filename))
  path
}

oracle_p_normal <- function(p, n, alpha = 0.0027) {
  z <- qnorm(1 - alpha / 2)
  sd <- sqrt(p * (1 - p) / n)
  list(
    lcl = max(0, p - z * sd),
    ucl = min(1, p + z * sd)
  )
}

oracle_p_cf1 <- function(p, n, alpha = 0.0027) {
  z <- qnorm(1 - alpha / 2)
  sd <- sqrt(p * (1 - p) / n)
  skew <- (z^2 - 1) * (1 - 2 * p) / (6 * n)
  list(
    lcl = max(0, p - z * sd + skew),
    ucl = min(1, p + z * sd + skew)
  )
}

oracle_p_cf2 <- function(p, n, alpha = 0.0027) {
  z <- abs(qnorm(1 - alpha / 2))
  sd <- sqrt(p * (1 - p) / n)
  skew <- (z^2 - 1) * (1 - 2 * p) / (6 * n)
  second <- (z^3 - 3 * z) * (1 - 6 * p * (1 - p)) /
    (24 * n^2 * sd) -
    (2 * z^3 - 5 * z) * (1 - 2 * p)^2 /
    (36 * n^2 * sd)

  list(
    lcl = max(0, p - z * sd + skew + second),
    ucl = min(1, p + z * sd + skew + second)
  )
}

oracle_p_limits <- function(p, n, alpha, method) {
  switch(
    method,
    normal = oracle_p_normal(p, n, alpha),
    cf1 = oracle_p_cf1(p, n, alpha),
    cf2 = oracle_p_cf2(p, n, alpha),
    stop(sprintf("unsupported method: %s", method))
  )
}

oracle_p_risk <- function(p, n, lcl, ucl) {
  lower_cut <- ceiling(n * lcl) - 1
  upper_cut <- floor(n * ucl)

  lower <- if (lcl <= 0 || lower_cut < 0) {
    0
  } else {
    sum(dbinom(0:lower_cut, size = n, prob = p))
  }

  upper <- if (ucl >= 1 || upper_cut >= n) {
    0
  } else {
    sum(dbinom((upper_cut + 1):n, size = n, prob = p))
  }

  lower + upper
}

test_that("published p-chart fixtures are reusable and non-circular", {
  fixtures <- read.csv(
    validation_fixture("p_chart_joekes_barbosa_2013.csv"),
    stringsAsFactors = FALSE
  )

  expect_equal(nrow(fixtures), 6)
  expect_true(all(fixtures$evidence_type == "published_table"))

  for (i in seq_len(nrow(fixtures))) {
    row <- fixtures[i, ]
    limits <- oracle_p_limits(row$p, row$n, row$alpha, row$method)

    calculated <- c(
      ucl = limits$ucl,
      nucl = row$n * limits$ucl,
      risk = oracle_p_risk(
        row$p, row$n, row$risk_lcl, row$risk_ucl
      )
    )
    published <- c(
      ucl = row$published_ucl,
      nucl = row$published_nucl,
      risk = row$published_risk
    )
    tolerance <- c(
      ucl = row$tol_ucl,
      nucl = row$tol_nucl,
      risk = row$tol_risk
    )
    ratio <- abs(calculated - published) / tolerance

    expect_true(
      all(ratio <= 1),
      info = sprintf(
        "%s %s: ratios UCL=%.3f, nUCL=%.3f, risk=%.3f; risk limits: %s",
        row$table, row$method, ratio["ucl"], ratio["nucl"], ratio["risk"],
        row$risk_limit_origin
      )
    )

    # The nUCL check must be based on the independently calculated UCL.
    expect_equal(calculated["nucl"], row$n * limits$ucl, tolerance = 0)
    expect_gt(abs(calculated["nucl"] - row$n * row$published_ucl), 0)
  }
})

test_that("production p-chart limits match independent oracles", {
  fixtures <- read.csv(
    validation_fixture("p_chart_joekes_barbosa_2013.csv"),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(fixtures))) {
    row <- fixtures[i, ]
    oracle <- oracle_p_limits(row$p, row$n, row$alpha, row$method)
    production <- pchart_limits(
      row$p, row$n, alpha = row$alpha, type = row$method
    )

    expect_equal(production$lcl, oracle$lcl, tolerance = 1e-12)
    expect_equal(production$ucl, oracle$ucl, tolerance = 1e-12)
  }
})

test_that("rounded-table risk and unrounded production risk are distinct checks", {
  fixtures <- read.csv(
    validation_fixture("p_chart_joekes_barbosa_2013.csv"),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(fixtures))) {
    row <- fixtures[i, ]
    limits <- oracle_p_limits(row$p, row$n, row$alpha, row$method)

    rounded_risk <- oracle_p_risk(
      row$p, row$n, row$risk_lcl, row$risk_ucl
    )
    unrounded_oracle_risk <- oracle_p_risk(
      row$p, row$n, limits$lcl, limits$ucl
    )
    production_risk <- pchart_alpha_risk(
      row$p, row$n, limits$lcl, limits$ucl
    )

    expect_equal(
      rounded_risk,
      row$published_risk,
      tolerance = row$tol_risk
    )
    expect_equal(
      production_risk,
      unrounded_oracle_risk,
      tolerance = 1e-12
    )
  }
})

test_that("applicability thresholds follow Joekes and Barbosa recommendations", {
  cfg <- list(
    normal = list(
      threshold = 5,
      cases = list(
        list(p = 0.5, n = 20, expected = TRUE),
        list(p = 0.1, n = 10, expected = FALSE),
        list(p = 0.015, n = 20, expected = FALSE)
      )
    ),
    cf1 = list(
      threshold = 0.25,
      cases = list(
        list(p = 0.015, n = 20, expected = TRUE),
        list(p = 0.004, n = 20, expected = FALSE),
        list(p = 0.01, n = 20, expected = FALSE)
      )
    ),
    cf2 = list(
      threshold = 0.08,
      cases = list(
        list(p = 0.015, n = 20, expected = TRUE),
        list(p = 0.004, n = 20, expected = FALSE),
        list(p = 0.001, n = 50, expected = FALSE)
      )
    )
  )

  for (method in names(cfg)) {
    for (case in cfg[[method]]$cases) {
      result <- pchart_limits(case$p, case$n, type = method)
      npq <- case$p * case$n * (1 - case$p)

      expect_equal(result$npq, npq, tolerance = 1e-15)
      expect_equal(
        result$applicable,
        case$expected,
        info = sprintf(
          "method=%s, p=%.4f, n=%d, npq=%.4f, threshold=%.2f",
          method, case$p, case$n, npq, cfg[[method]]$threshold
        )
      )
    }
  }
})
