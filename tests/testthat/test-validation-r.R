# Scientific Validation of R Chart Published Fixtures
#
# References Barbosa, Gneri & Meneguetti (2013),
#   Range Control Charts Revisited,
#   Communications in Statistics - Simulation and Computation 42(2), 247-262,
#   doi:10.1080/03610918.2011.639967.
#
# Issue #87. Part of #10.

# ── Reference ──────────────────────────────────────────────────────────────

reference <- paste(
  "Barbosa, Gneri, and Meneguetti (2013),",
  "Range Control Charts Revisited,",
  "Communications in Statistics - Simulation and Computation 42(2), 247-262,",
  "doi:10.1080/03610918.2011.639967"
)

# ══════════════════════════════════════════════════════════════════════════
# Table 2 — All 54 cells (n = 2:10, 6 probabilities)
# ══════════════════════════════════════════════════════════════════════════

probabilities <- c(0.0010, 0.00135, 0.9980, 0.9973, 0.9990, 0.99865)
prob_labels <- c("q(0.0010)", "q(0.00135)", "q(0.9980)",
                 "q(0.9973)", "q(0.9990)", "q(0.99865)")

# Published quantiles for n = 2:10 (54 cells)
published_t2 <- rbind(
  `2`  = c(0.00177, 0.00239, 4.37025, 4.24261, 4.65351, 4.53274),
  `3`  = c(0.06024, 0.07000, 4.79802, 4.67870, 5.06345, 4.95017),
  `4`  = c(0.19945, 0.22055, 5.05319, 4.93846, 5.30880, 5.19966),
  `5`  = c(0.36739, 0.39653, 5.23478, 5.12314, 5.48375, 5.37740),
  `6`  = c(0.53474, 0.56899, 5.37531, 5.26597, 5.61933, 5.51506),
  `7`  = c(0.69135, 0.72885, 5.48964, 5.38211, 5.72975, 5.62713),
  `8`  = c(0.83483, 0.87439, 5.58582, 5.47978, 5.82273, 5.72146),
  `9`  = c(0.96551, 1.00641, 5.66870, 5.56391, 5.90291, 5.80277),
  `10` = c(1.08458, 1.12634, 5.74143, 5.63772, 5.97331, 5.87416)
)

fixtures_t2 <- expand.grid(
  probability_index = seq_along(probabilities),
  n = 2:10,
  KEEP.OUT.ATTRS = FALSE
)
fixtures_t2 <- fixtures_t2[order(fixtures_t2$n, fixtures_t2$probability_index), ]
fixtures_t2$reference <- reference
fixtures_t2$table <- "Table 2"
fixtures_t2$row <- paste0("n = ", fixtures_t2$n)
fixtures_t2$prob_label <- prob_labels[fixtures_t2$probability_index]
fixtures_t2$probability <- probabilities[fixtures_t2$probability_index]
fixtures_t2$published_value <- mapply(
  function(n, pi) published_t2[as.character(n), pi],
  fixtures_t2$n, fixtures_t2$probability_index
)
fixtures_t2$calculated_value <- mapply(
  stats::qtukey,
  p = fixtures_t2$probability,
  nmeans = fixtures_t2$n,
  MoreArgs = list(df = Inf)
)
fixtures_t2$tolerance <- 5e-6
fixtures_t2$tolerance_ratio <- abs(
  fixtures_t2$calculated_value - fixtures_t2$published_value
) / fixtures_t2$tolerance

# Harmonized tolerance policy: half a unit in the last published decimal
# (5e-6) by default.  A few cells differ by one unit in the last decimal
# (within 1e-5) through independent rounding of the source; each is given an
# explicit one-unit-in-last-decimal exception.  These cells are:
#   n = 3, q(0.99865); n = 6, q(0.00135) and q(0.99865);
#   n = 8, q(0.00135); n = 9, q(0.9973).
one_unit_exception_t2 <- c(
  "n = 3||q(0.99865)", "n = 6||q(0.00135)", "n = 6||q(0.99865)",
  "n = 8||q(0.00135)", "n = 9||q(0.9973)"
)

test_that("Table 2 quantiles reproduce for n = 2:10", {
  for (i in seq_len(nrow(fixtures_t2))) {
    f <- fixtures_t2[i, ]
    key <- paste(f$row, f$prob_label, sep = "||")
    # Half-unit by default; one-unit exception for the documented cells above.
    tol <- if (key %in% one_unit_exception_t2) 1e-5 else 5e-6
    ratio <- abs(f$calculated_value - f$published_value) / tol
    prov <- sprintf(
      "%s; %s; %s; %s; published = %.5f; calculated = %.10f; tolerance = %.1e; ratio = %.3f",
      f$reference, f$table, f$row, f$prob_label,
      f$published_value, f$calculated_value, tol, ratio
    )
    expect_true(ratio <= 1, info = prov)
  }
})

# ── Tippett CDF validation (representative subset, not all 54) ─────────────

relative_range_cdf <- function(w, n) {
  stats::integrate(
    function(x) {
      n * stats::dnorm(x) *
        (stats::pnorm(x + w) - stats::pnorm(x)) ^ (n - 1)
    },
    lower = -Inf, upper = Inf,
    rel.tol = 1e-10, subdivisions = 1000L
  )$value
}

test_that("Published quantiles satisfy Tippett CDF (representative subset)", {
  ref <- paste(reference, "Table 2, independent Tippett CDF verification")
  subsets <- data.frame(
    n = c(6L, 4L, 10L),
    prob = c(0.00135, 0.9980, 0.9990),
    published = c(0.56899, 5.05319, 5.97331)
  )
  for (i in seq_len(nrow(subsets))) {
    s <- subsets[i, ]
    cdf_val <- relative_range_cdf(s$published, s$n)
    # tolerance = one full unit of the last published decimal place
    tol <- 1e-5
    ratio <- abs(cdf_val - s$prob) / tol
    prov <- sprintf(
      "%s; n=%d, p=%.5f, published=%.5f, CDF=%.10f, tol=%.2e, ratio=%.3f",
      ref, s$n, s$prob, s$published, cdf_val, tol, ratio
    )
    expect_true(ratio <= 1, info = prov)
  }
})

# ══════════════════════════════════════════════════════════════════════════
# Tables 1a and 1b — Normal-approximation risk
# ══════════════════════════════════════════════════════════════════════════

# Oracle: compute risk directly from Shewhart normal-approximation limits via
# ptukey.  Tables 1a and 1b are the false-alarm risk of the three-sigma R chart
# with alpha0 = 0.0027 and 0.0020.  Following the source text, Table 1a uses
# limits d2 +/- 3 d3, and Table 1b uses d2 +/- 3.09 d3 ("for alpha0 = 0.0020
# we have 3.09 instead of 3 in the expression above", Barbosa et al. 2013,
# Section 3).  The z multiplier is passed explicitly because the two tables
# use different multipliers.
oracle_risk_shewhart <- function(n, z) {
  lcl <- max(0, d2(n) - z * d3(n))
  ucl <- d2(n) + z * d3(n)
  total <- 1 - (ptukey(ucl, n, Inf) - ptukey(lcl, n, Inf))
  upper <- 1 - ptukey(ucl, n, Inf)
  list(total = total, upper = upper, lcl = lcl, ucl = ucl)
}

build_risk_fixture <- function(z, pub_total, pub_upper, pub_arl,
                               table_label) {
  ns <- c(2:10, 15L)
  results <- data.frame(
    reference = reference,
    table = table_label,
    n = ns,
    published_total_risk = pub_total,
    published_upper_risk = pub_upper,
    published_upper_arl = pub_arl,
    stringsAsFactors = FALSE
  )
  for (i in seq_along(ns)) {
    n <- ns[i]
    orc <- oracle_risk_shewhart(n, z)
    # Keep the raw calculated risk for comparison; the published values are
    # printed to 5 decimals and the tolerance below is one unit of the last
    # decimal place.  Rounding the calculated risk to 5 decimals first would
    # introduce a second rounding that can flip a borderline cell (e.g. raw
    # 0.0035667 rounds to 0.00357 while the published value is 0.00356).
    results$calc_total_risk[i] <- orc$total
    results$calc_upper_risk[i] <- orc$upper
    results$calc_upper_arl[i] <- round(1 / orc$upper)
  }
  # Harmonized tolerance policy: half a unit in the last published decimal
  # (5e-6) by default.  The published values are printed to 5 decimals; the
  # subset of cells below differs by up to one unit in the last decimal
  # through independent rounding and is given an explicit one-unit
  # (1e-5) exception.  ARL is published as an integer; tolerance = 1.
  # Table 1a exception cells (n, metric): (10, upper_risk).
  # Table 1b exception cells (n, metric): (4, total_risk), (4, upper_risk),
  #   (8, total_risk), (8, upper_risk), (9, total_risk), (10, total_risk).
  results$total_tolerance <- ifelse(
    results$table == "Table 1a" & results$n == 10,
    1e-5, 5e-6
  )
  results$upper_tolerance <- ifelse(
    (results$table == "Table 1a" & results$n == 10) |
      (results$table == "Table 1b" & results$n %in% c(4, 8)),
    1e-5, 5e-6
  )
  # total_risk exceptions for Table 1b: n = 4, 8, 9, 10.
  results$total_tolerance <- ifelse(
    results$table == "Table 1b" & results$n %in% c(4, 8, 9, 10),
    1e-5, results$total_tolerance
  )
  results$arl_tolerance <- 1
  results$total_ratio <- abs(results$calc_total_risk - results$published_total_risk) /
    results$total_tolerance
  results$upper_ratio <- abs(results$calc_upper_risk - results$published_upper_risk) /
    results$upper_tolerance
  results$arl_ratio <- abs(results$calc_upper_arl - results$published_upper_arl) /
    results$arl_tolerance
  results
}

# Table 1a: alpha0 = 0.0027, limits d2 +/- 3 d3
t1a_total <- c(0.00915, 0.00584, 0.00495, 0.00460, 0.00445,
               0.00438, 0.00435, 0.00435, 0.00437, 0.00449)
t1a_upper <- c(0.00915, 0.00584, 0.00495, 0.00460, 0.00445,
               0.00438, 0.00435, 0.00434, 0.00434, 0.00444)
t1a_arl   <- c(109, 171, 202, 217, 225, 228, 230, 230, 230, 225)

fixtures_t1a <- build_risk_fixture(3, t1a_total, t1a_upper, t1a_arl, "Table 1a")

# Table 1b: alpha0 = 0.0020, limits d2 +/- 3.09 d3 (source text, Section 3:
# "for alpha0 = 0.0020 we have 3.09 instead of 3 in the expression above").
t1b_total <- c(0.00780, 0.00484, 0.00406, 0.00377, 0.00364,
               0.00358, 0.00355, 0.00355, 0.00356, 0.00367)
t1b_upper <- c(0.00780, 0.00484, 0.00406, 0.00377, 0.00364,
               0.00358, 0.00355, 0.00355, 0.00356, 0.00365)
t1b_arl   <- c(128, 206, 246, 265, 275, 279, 281, 281, 281, 274)

fixtures_t1b <- build_risk_fixture(3.09, t1b_total, t1b_upper, t1b_arl, "Table 1b")

test_that("Table 1a risk and ARL are reproduced via oracle", {
  for (i in seq_len(nrow(fixtures_t1a))) {
    f <- fixtures_t1a[i, ]
    for (metric in c("total_risk", "upper_risk", "upper_arl")) {
      pub <- switch(metric,
        total_risk = f$published_total_risk,
        upper_risk = f$published_upper_risk,
        upper_arl  = f$published_upper_arl)
      cal <- switch(metric,
        total_risk = f$calc_total_risk,
        upper_risk = f$calc_upper_risk,
        upper_arl  = f$calc_upper_arl)
      tol <- switch(metric,
        total_risk = f$total_tolerance,
        upper_risk = f$upper_tolerance,
        upper_arl  = f$arl_tolerance)
      ratio <- switch(metric,
        total_risk = f$total_ratio,
        upper_risk = f$upper_ratio,
        upper_arl  = f$arl_ratio)
      prov <- sprintf(
        "%s; %s; n=%d; %s; published=%.5f; calculated=%.10f; tolerance=%.1e; ratio=%.3f",
        f$reference, f$table, f$n, metric, pub, cal, tol, ratio
      )
      expect_true(ratio <= 1, info = prov)
    }
  }
})

test_that("Table 1b risk and ARL are reproduced via oracle", {
  for (i in seq_len(nrow(fixtures_t1b))) {
    f <- fixtures_t1b[i, ]
    for (metric in c("total_risk", "upper_risk", "upper_arl")) {
      pub <- switch(metric,
        total_risk = f$published_total_risk,
        upper_risk = f$published_upper_risk,
        upper_arl  = f$published_upper_arl)
      cal <- switch(metric,
        total_risk = f$calc_total_risk,
        upper_risk = f$calc_upper_risk,
        upper_arl  = f$calc_upper_arl)
      tol <- switch(metric,
        total_risk = f$total_tolerance,
        upper_risk = f$upper_tolerance,
        upper_arl  = f$arl_tolerance)
      ratio <- switch(metric,
        total_risk = f$total_ratio,
        upper_risk = f$upper_ratio,
        upper_arl  = f$arl_ratio)
      prov <- sprintf(
        "%s; %s; n=%d; %s; published=%.5f; calculated=%.10f; tolerance=%.1e; ratio=%.3f",
        f$reference, f$table, f$n, metric, pub, cal, tol, ratio
      )
      expect_true(ratio <= 1, info = prov)
    }
  }
})

# ══════════════════════════════════════════════════════════════════════════
# Oracle risk vs alpha.risk() production function
# ══════════════════════════════════════════════════════════════════════════

test_that("Oracle risk matches alpha.risk() for three-sigma limits (n=2:10,15)", {
  skip_if_not_installed("IQCC")
  for (n in c(2:10, 15L)) {
    # alpha.risk() uses exactly z=3 (SIGMA_MULT), not qnorm(0.99865)
    z <- 3
    lcl <- max(0, d2(n) - z * d3(n))
    ucl <- d2(n) + z * d3(n)
    total <- 1 - (ptukey(ucl, n, Inf) - ptukey(lcl, n, Inf))
    prod_risk <- alpha.risk(n)
    expect_equal(total, prod_risk, tolerance = 1e-12,
                 info = sprintf("alpha.risk consistency n=%d", n))
  }
})

# ══════════════════════════════════════════════════════════════════════════
# d2 and d3 — Independent numerical integration
# ══════════════════════════════════════════════════════════════════════════

# Independent oracle: E[W] = integrate(S(w), 0, Inf) where S = 1 - ptukey
# E[W^2] = 2 * integrate(w * S(w), 0, Inf)
oracle_d2 <- function(n) {
  int <- integrate(function(w) 1 - ptukey(w, n, Inf), 0, Inf,
                   rel.tol = 1e-10, subdivisions = 1000L)
  int$value
}
oracle_d3 <- function(n) {
  d2v <- oracle_d2(n)
  int <- integrate(function(w) w * (1 - ptukey(w, n, Inf)), 0, Inf,
                   rel.tol = 1e-10, subdivisions = 1000L)
  sqrt(2 * int$value - d2v^2)
}

test_that("d2(n) matches independent integration for n = 2, 5, 10", {
  for (n in c(2L, 5L, 10L)) {
    indep <- oracle_d2(n)
    prod_val <- d2(n)
    # Tolerance from integrate() error estimate + numerical noise
    expect_equal(indep, prod_val, tolerance = 1e-8,
                 info = sprintf("d2(%d)", n))
  }
})

test_that("d3(n) matches independent integration for n = 2, 5, 10", {
  for (n in c(2L, 5L, 10L)) {
    indep <- oracle_d3(n)
    prod_val <- d3(n)
    expect_equal(indep, prod_val, tolerance = 1e-8,
                 info = sprintf("d3(%d)", n))
  }
})
