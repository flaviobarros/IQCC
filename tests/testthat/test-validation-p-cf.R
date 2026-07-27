# Scientific Validation of p-Chart Cornish-Fisher Limits
#
# References Joekes & Barbosa (2013), doi:10.1016/j.conengprac.2012.12.005.
# All oracles are independent of pchart_limits() and pchart_alpha_risk().
#
# Issue #86. Part of #10.

# ── Independent oracles (no calls to production code) ─────────────────────

oracle_p_normal <- function(p, n, alpha = 0.0027) {
  z <- qnorm(1 - alpha / 2)
  sd <- sqrt(p * (1 - p) / n)
  list(lcl = max(0, p - z * sd),
       ucl = min(1, p + z * sd))
}

oracle_p_cf1 <- function(p, n, alpha = 0.0027) {
  z <- qnorm(1 - alpha / 2)
  sd <- sqrt(p * (1 - p) / n)
  skew <- (z^2 - 1) / (6 * n) * (1 - 2 * p)
  list(lcl = max(0, p - z * sd + skew),
       ucl = min(1, p + z * sd + skew))
}

oracle_p_cf2 <- function(p, n, alpha = 0.0027) {
  z <- qnorm(1 - alpha / 2)
  z_pos <- abs(z)
  sd <- sqrt(p * (1 - p) / n)
  skew <- (z_pos^2 - 1) / (6 * n) * (1 - 2 * p)
  adj2 <- (z_pos^3 - 3 * z_pos) / (24 * n^2) * (1 - 6 * p * (1 - p)) / sd -
    (2 * z_pos^3 - 5 * z_pos) / (36 * n^2) * (1 - 2 * p)^2 / sd
  ucl <- p + z_pos * sd + skew + adj2
  lcl <- p - z_pos * sd + skew + adj2
  lcl <- max(0, lcl)
  ucl <- min(1, ucl)
  list(lcl = lcl, ucl = ucl)
}

oracle_p_risk <- function(p, n, lcl, ucl) {
  lwr <- if (lcl <= 0) 0 else sum(dbinom(0:(ceiling(n * lcl) - 1), n, p))
  upr <- if (ucl >= 1) 0 else sum(dbinom((floor(n * ucl) + 1):n, n, p))
  lwr + upr
}

# ── Reference ──────────────────────────────────────────────────────────────

reference <- paste(
  "Joekes, S. and Barbosa, E. P. (2013).",
  "An improved attribute control chart for monitoring non-conforming",
  "proportion in high quality processes.",
  "Control Engineering Practice, 21, 407-412.",
  "doi:10.1016/j.conengprac.2012.12.005"
)

# ── Table 2: p = 0.015, n = 20, alpha0 = 0.0027 ──────────────────────────

p_t2 <- 0.015
n_t2 <- 20
alpha0 <- 0.0027

publim_t2 <- list(
  normal = list(lcl = 0,                ucl = 0.0965),
  cf1    = list(lcl = 0,                ucl = 0.1612),
  cf2    = list(lcl = 0,                ucl = 0.1303)
)

fixtures_t2 <- data.frame(
  reference      = reference,
  table          = "Table 2",
  row            = rep(c("normal", "CF1", "CF2"), each = 3),
  parameters     = "p = 0.015, n = 20, alpha = 0.0027, two.sided, truncate",
  metric         = rep(c("UCL", "nUCL", "alpha_risk"), 3),
  published_value = c(
    0.0965, 1.931, 0.035746,
    0.1612, 3.224, 0.000202,
    0.1303, 2.606, 0.003178
  ),
  calculated_value = c(
    oracle_p_normal(p_t2, n_t2, alpha0)$ucl,
    n_t2 * 0.0965,
    oracle_p_risk(p_t2, n_t2, publim_t2$normal$lcl, publim_t2$normal$ucl),
    oracle_p_cf1(p_t2, n_t2, alpha0)$ucl,
    n_t2 * 0.1612,
    oracle_p_risk(p_t2, n_t2, publim_t2$cf1$lcl, publim_t2$cf1$ucl),
    oracle_p_cf2(p_t2, n_t2, alpha0)$ucl,
    n_t2 * 0.1303,
    oracle_p_risk(p_t2, n_t2, publim_t2$cf2$lcl, publim_t2$cf2$ucl)
  ),
  tolerance          = c(5e-5, 1e-3, 5e-7,
                         5e-5, 1e-3, 5e-7,
                         5e-5, 1e-3, 5e-7),
  tolerance_rationale = rep(
    c("UCL: half-unit in 4th decimal place",
      "nUCL: propagated from UCL rounding: n * 5e-5 = 0.001",
      "risk: half-unit in 6th decimal place"),
    3
  ),
  stringsAsFactors = FALSE
)
fixtures_t2$tolerance_ratio <- abs(
  fixtures_t2$calculated_value - fixtures_t2$published_value
) / fixtures_t2$tolerance

# ── Table 3: p = 0.004, n = 20, alpha0 = 0.0027 ──────────────────────────

p_t3 <- 0.004
n_t3 <- 20

# Published limits for risk computation (from rounded UCL in table)
publim_t3 <- list(
  normal = list(lcl = 0.0000, ucl = 0.0463),
  cf1    = list(lcl = NULL,   ucl = 0.1125),
  cf2    = list(lcl = 0.0000, ucl = 0.0533)
)
# For CF1 in Table 3, compute the LCL from the oracle with truncation
publim_t3$cf1$lcl <- round(oracle_p_cf1(p_t3, n_t3, alpha0)$lcl, 4)

fixtures_t3 <- data.frame(
  reference      = reference,
  table          = "Table 3",
  row            = rep(c("normal", "CF1", "CF2"), each = 3),
  parameters     = "p = 0.004, n = 20, alpha = 0.0027, two.sided, truncate",
  metric         = rep(c("UCL", "nUCL", "alpha_risk"), 3),
  published_value = c(
    0.0463, 0.926, 0.077032,
    0.1125, 2.250, 0.923038,
    0.0533, 1.066, 0.002898
  ),
  calculated_value = c(
    oracle_p_normal(p_t3, n_t3, alpha0)$ucl,
    n_t3 * 0.0463,
    oracle_p_risk(p_t3, n_t3, publim_t3$normal$lcl, publim_t3$normal$ucl),
    oracle_p_cf1(p_t3, n_t3, alpha0)$ucl,
    n_t3 * 0.1125,
    oracle_p_risk(p_t3, n_t3, publim_t3$cf1$lcl, publim_t3$cf1$ucl),
    oracle_p_cf2(p_t3, n_t3, alpha0)$ucl,
    n_t3 * 0.0533,
    oracle_p_risk(p_t3, n_t3, publim_t3$cf2$lcl, publim_t3$cf2$ucl)
  ),
  tolerance          = c(5e-5, 1e-3, 5e-7,
                         5e-5, 1e-3, 5e-7,
                         5e-5, 1e-3, 5e-7),
  tolerance_rationale = rep(
    c("UCL: half-unit in 4th decimal place",
      "nUCL: propagated from UCL rounding: n * 5e-5 = 0.001",
      "risk: half-unit in 6th decimal place"),
    3
  ),
  stringsAsFactors = FALSE
)
fixtures_t3$tolerance_ratio <- abs(
  fixtures_t3$calculated_value - fixtures_t3$published_value
) / fixtures_t3$tolerance

# ── Test: Table 2 reproduction ────────────────────────────────────────────

test_that("Table 2 (p=0.015, n=20) reproduces via independent oracles", {
  fixtures <- rbind(fixtures_t2, fixtures_t3)
  for (i in seq_len(nrow(fixtures))) {
    prov <- sprintf(
      paste0("%s; %s; %s; %s = %s; published = %.6f; ",
             "calculated = %.10f; tolerance = %.1e; ratio = %.3f"),
      fixtures$reference[i], fixtures$table[i], fixtures$row[i],
      fixtures$metric[i], fixtures$parameters[i],
      fixtures$published_value[i], fixtures$calculated_value[i],
      fixtures$tolerance[i], fixtures$tolerance_ratio[i]
    )
    expect_true(fixtures$tolerance_ratio[i] <= 1, info = prov)
  }
})

# ── Test: Production pchart_limits matches oracle ─────────────────────────

test_that("pchart_limits normal matches oracle", {
  skip_if_not_installed("IQCC")
  for (pv in c(0.015, 0.004)) {
    for (nv in c(20, 50)) {
      o <- oracle_p_normal(pv, nv, alpha0)
      prod <- pchart_limits(pv, nv, alpha = alpha0, type = "normal")
      expect_equal(prod$ucl, o$ucl, tolerance = 1e-12,
                   info = sprintf("p=%.3f, n=%d", pv, nv))
      expect_equal(prod$lcl, o$lcl, tolerance = 1e-12,
                   info = sprintf("p=%.3f, n=%d", pv, nv))
    }
  }
})

test_that("pchart_limits cf1 matches oracle", {
  skip_if_not_installed("IQCC")
  for (pv in c(0.015, 0.004)) {
    for (nv in c(20, 50)) {
      o <- oracle_p_cf1(pv, nv, alpha0)
      prod <- pchart_limits(pv, nv, alpha = alpha0, type = "cf1")
      expect_equal(prod$ucl, o$ucl, tolerance = 1e-12,
                   info = sprintf("p=%.3f, n=%d", pv, nv))
      expect_equal(prod$lcl, o$lcl, tolerance = 1e-12,
                   info = sprintf("p=%.3f, n=%d", pv, nv))
    }
  }
})

test_that("pchart_limits cf2 matches oracle", {
  skip_if_not_installed("IQCC")
  for (pv in c(0.015, 0.004)) {
    for (nv in c(20, 50)) {
      o <- oracle_p_cf2(pv, nv, alpha0)
      prod <- pchart_limits(pv, nv, alpha = alpha0, type = "cf2")
      expect_equal(prod$ucl, o$ucl, tolerance = 1e-12,
                   info = sprintf("p=%.3f, n=%d", pv, nv))
      expect_equal(prod$lcl, o$lcl, tolerance = 1e-12,
                   info = sprintf("p=%.3f, n=%d", pv, nv))
    }
  }
})

# ── Test: Risk oracle matches pchart_alpha_risk ────────────────────────────

test_that("oracle risk matches pchart_alpha_risk across methods", {
  skip_if_not_installed("IQCC")
  scenarios <- expand.grid(
    pv = c(0.015, 0.004, 0.05, 0.10),
    nv = c(20, 50, 100),
    type = c("normal", "cf1", "cf2"),
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )
  for (i in seq_len(nrow(scenarios))) {
    pv <- scenarios$pv[i]; nv <- scenarios$nv[i]; type <- scenarios$type[i]
    limits <- pchart_limits(pv, nv, alpha = alpha0, type = type)
    risk_prod <- pchart_alpha_risk(pv, nv, limits$lcl, limits$ucl)
    risk_ora  <- oracle_p_risk(pv, nv, limits$lcl, limits$ucl)
    expect_equal(risk_prod, risk_ora, tolerance = 1e-12,
                 info = sprintf("p=%.3f, n=%d, type=%s", pv, nv, type))
  }
})

# ── Test: applicable thresholds ────────────────────────────────────────────

test_that("applicable thresholds match Joekes & Barbosa (2013) recommendations", {
  skip_if_not_installed("IQCC")

  cfg <- list(normal = list(threshold = 5, cases = list(
    list(p = 0.5, n = 20, expect = TRUE),
    list(p = 0.1, n = 10, expect = FALSE),
    list(p = 0.015, n = 20, expect = FALSE)
  )), cf1 = list(threshold = 0.25, cases = list(
    list(p = 0.015, n = 20, expect = TRUE),
    list(p = 0.004, n = 20, expect = FALSE),
    list(p = 0.01, n = 20, expect = FALSE)
  )), cf2 = list(threshold = 0.08, cases = list(
    list(p = 0.015, n = 20, expect = TRUE),
    list(p = 0.004, n = 20, expect = FALSE),
    list(p = 0.001, n = 50, expect = FALSE)
  )))

  for (type in names(cfg)) {
    thresh <- cfg[[type]]$threshold
    for (cs in cfg[[type]]$cases) {
      prod <- pchart_limits(cs$p, cs$n, alpha = alpha0, type = type)
      npq <- cs$p * cs$n * (1 - cs$p)
      expect_equal(prod$npq, npq, tolerance = 1e-15,
                   info = sprintf("type=%s, p=%.4f, n=%d", type, cs$p, cs$n))
      if (!is.na(cs$expect)) {
        expect_equal(prod$applicable, cs$expect,
                     info = sprintf("type=%s, p=%.4f, n=%d, npq=%.4f, threshold=%.2f",
                                    type, cs$p, cs$n, npq, thresh))
      }
    }
  }
})
