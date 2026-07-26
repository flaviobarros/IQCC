######################################################################
# IQCC Replication Script - Fast
#
# Reproduces key numerical results from the IQCC base papers.
# Expected runtime: under 5 minutes.
#
# References:
#   Joekes & Barbosa (2013) - J. Applied Statistics, 40(11), 2451-2465
#   Barbosa, Gneri & Meneguetti (2013) - Pesquisa Operacional, 33(2), 285-306
#   Joekes, Smrekar & Barbosa (2015) - J. Applied Statistics, 42(5), 1049-1067
######################################################################

library(IQCC)

options(digits = 6)
set.seed(2026)

cat(rep("=", 72), "\n", sep = "")
cat("IQCC Replication Script\n")
cat(format(Sys.Date(), "%Y-%m-%d"), "\n")
cat(rep("=", 72), "\n\n", sep = "")

######################################################################
# 1. Cornish-Fisher corrected p-chart limits
#    Joekes & Barbosa (2013), Sections 3-4
######################################################################

cat(rep("-", 72), "\n", sep = "")
cat("1. Cornish-Fisher corrected p-chart limits\n")
cat("   Joekes & Barbosa (2013)\n")
cat(rep("-", 72), "\n\n", sep = "")

p <- 0.05
n <- 100

limits_norm <- pchart_limits(p, n, type = "normal")
limits_cf1  <- pchart_limits(p, n, type = "cf1")
limits_cf2  <- pchart_limits(p, n, type = "cf2")

cat(sprintf("p = %.3f, n = %d\n\n", p, n))

cat("Limits:\n")
cat(sprintf("  Normal:  center = %.4f, UCL = %.4f, LCL = %.4f\n",
            limits_norm$center, limits_norm$ucl, limits_norm$lcl))
cat(sprintf("  CF1:     center = %.4f, UCL = %.4f, LCL = %.4f\n",
            limits_cf1$center, limits_cf1$ucl, limits_cf1$lcl))
cat(sprintf("  CF2:     center = %.4f, UCL = %.4f, LCL = %.4f\n",
            limits_cf2$center, limits_cf2$ucl, limits_cf2$lcl))

alpha_norm <- pchart_alpha_risk(p, n, limits_norm$lcl, limits_norm$ucl)
alpha_cf1  <- pchart_alpha_risk(p, n, limits_cf1$lcl, limits_cf1$ucl)
alpha_cf2  <- pchart_alpha_risk(p, n, limits_cf2$lcl, limits_cf2$ucl)

cat("\nActual false-alarm probabilities (target: 0.0027):\n")
cat(sprintf("  Normal: %.6f\n", alpha_norm))
cat(sprintf("  CF1:    %.6f\n", alpha_cf1))
cat(sprintf("  CF2:    %.6f\n\n", alpha_cf2))

# Low-proportion scenario where CF correction matters most
p_low <- 0.01
limits_low_cf2 <- pchart_limits(p_low, n, type = "cf2")
alpha_low_cf2  <- pchart_alpha_risk(p_low, n,
                                    limits_low_cf2$lcl, limits_low_cf2$ucl)

cat(sprintf("Low proportion scenario: p = %.3f, n = %d\n", p_low, n))
cat(sprintf("  CF2 limits: UCL = %.4f, LCL = %.4f\n",
            limits_low_cf2$ucl, limits_low_cf2$lcl))
cat(sprintf("  Actual alpha: %.6f\n\n", alpha_low_cf2))

######################################################################
# 2. Exact R-chart limits
#    Barbosa, Gneri & Meneguetti (2013)
######################################################################

cat(rep("-", 72), "\n", sep = "")
cat("2. Exact R-chart limits\n")
cat("   Barbosa, Gneri & Meneguetti (2013)\n")
cat(rep("-", 72), "\n\n", sep = "")

n_r <- 5
sigma <- 1.0

limits_shewhart <- r_shewhart_limits(sigma, n_r)
limits_exact    <- r_exact_limits(sigma, n_r)

cat(sprintf("n = %d, sigma = %.1f\n\n", n_r, sigma))
cat("Limits:\n")
cat(sprintf("  Shewhart: center = %.4f, UCL = %.4f, LCL = %.4f\n",
            limits_shewhart$center, limits_shewhart$ucl,
            limits_shewhart$lcl))
cat(sprintf("  Exact:    center = %.4f, UCL = %.4f, LCL = %.4f\n\n",
            limits_exact$center, limits_exact$ucl, limits_exact$lcl))

######################################################################
# 3. DS-np double-sampling plan
#    Joekes, Smrekar & Barbosa (2015)
######################################################################

cat(rep("-", 72), "\n", sep = "")
cat("3. DS-np double-sampling plan\n")
cat("   Joekes, Smrekar & Barbosa (2015)\n")
cat(rep("-", 72), "\n\n", sep = "")

n1   <- 10
n2   <- 20
wl   <- 1.5
ucl1 <- 2.5
ucl2 <- 4.5

p_in  <- 0.05
p_out <- 0.20

arl_in  <- dsnp_arl(p_in, n1, n2, wl, ucl1, ucl2)
arl_out <- dsnp_arl(p_out, n1, n2, wl, ucl1, ucl2)

ass_in  <- dsnp_ass(p_in, n1, n2, wl, ucl1)
ass_out <- dsnp_ass(p_out, n1, n2, wl, ucl1)

cat(sprintf("Plan: n1 = %d, n2 = %d, wl = %.1f, ucl1 = %.1f, ucl2 = %.1f\n\n",
            n1, n2, wl, ucl1, ucl2))
cat(sprintf("In-control  (p = %.2f):\n", p_in))
cat(sprintf("  ARL = %.2f,  ASS = %.2f\n", arl_in$arl, ass_in$ass))
cat(sprintf("Out-of-control (p = %.2f):\n", p_out))
cat(sprintf("  ARL = %.2f,  ASS = %.2f\n\n", arl_out$arl, ass_out$ass))

# OC curve
cat("OC curve (probability of acceptance):\n")
for (p_test in c(0.01, 0.05, 0.10, 0.20, 0.30)) {
  pa <- dsnp_prob_accept(p_test, n1, n2, wl, ucl1, ucl2)
  cat(sprintf("  p = %.2f -> Pa = %.4f\n", p_test, pa$pt))
}
cat("\n")

######################################################################
# 4. Generalized variance |S| limits
######################################################################

cat(rep("-", 72), "\n", sep = "")
cat("4. Generalized variance |S| limits\n")
cat(rep("-", 72), "\n\n", sep = "")

n_gv    <- 10
p_gv    <- 2
det_sig <- 1.0

limits_gv_exact <- gv_limits(n_gv, p_gv, det_sigma = det_sig, type = "exact")
limits_gv_cf    <- gv_limits(n_gv, p_gv, det_sigma = det_sig, type = "cf")
limits_gv_norm  <- gv_limits(n_gv, p_gv, det_sigma = det_sig, type = "normal")

cat(sprintf("n = %d, p = %d, |Sigma| = %.0f\n\n", n_gv, p_gv, det_sig))
cat("|S| chart limits (alpha = 0.0027, upper-sided):\n")
cat(sprintf("  Exact: center = %.4f, UCL = %.4f\n",
            limits_gv_exact$center, limits_gv_exact$ucl))
cat(sprintf("  CF:    center = %.4f, UCL = %.4f\n",
            limits_gv_cf$center, limits_gv_cf$ucl))
cat(sprintf("  Normal: center = %.4f, UCL = %.4f\n\n",
            limits_gv_norm$center, limits_gv_norm$ucl))

######################################################################
# 5. Trace statistic tr(V) limits
######################################################################

cat(rep("-", 72), "\n", sep = "")
cat("5. Trace statistic tr(V) limits\n")
cat(rep("-", 72), "\n\n", sep = "")

n_trv  <- 10
p_trv  <- 2

limits_trv <- trv_limits(n_trv, p_trv)
cat(sprintf("n = %d, p = %d\n\n", n_trv, p_trv))
cat("Limits:\n")
cat(sprintf("  center = %.4f\n", limits_trv$center))
cat(sprintf("  UCL = %.4f\n", limits_trv$ucl))
cat(sprintf("  LCL = %.4f\n\n", limits_trv$lcl))

######################################################################
# 6. Hotelling T^2 asymptotic robustness
######################################################################

cat(rep("-", 72), "\n", sep = "")
cat("6. Hotelling T^2 asymptotic robustness\n")
cat(rep("-", 72), "\n\n", sep = "")

cat("Simulated coverage of asymptotic critical values\n")
cat("for T^2 statistic under normality.\n\n")

sim <- sim_t2_asymptotic(n = c(10, 30), p = c(2, 3), nsim = 1000,
                         sig_levels = c(0.90, 0.95))
for (n_val in c(10, 30)) {
  for (p_val in c(2, 3)) {
    sub <- sim[sim$n == n_val & sim$p == p_val, ]
    cat(sprintf("n = %d, p = %d:\n", n_val, p_val))
    print(round(sub[, c("level", "empirical", "chisq")], 4))
    cat("\n")
  }
}

######################################################################
# Session information
######################################################################

cat(rep("=", 72), "\n", sep = "")
cat("Session information\n")
cat(rep("=", 72), "\n\n", sep = "")
sessionInfo()
