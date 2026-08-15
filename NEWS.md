# IQCC 0.8.0

## New and expanded statistical methods

- Add the double-sampling np chart family for high-quality processes:
  `dsnp_prob_accept()`, `dsnp_arl()`, `dsnp_ass()`, `dsnp_limits()`,
  `dsnp_design()`, and `cchart.DSnp()`.
- Add generalized variance monitoring through `gv_stat()`, `gv_limits()`,
  `gv_alpha_risk()`, and `cchart.GV()`, including normal, Cornish-Fisher,
  selected exact, and simulation-based limits.
- Add auxiliary multivariate variability monitoring based on `tr(V)` through
  `trv_stat()`, `trv_limits()`, `trv_alpha_risk()`, and `cchart.trV()`.
- Add pure numerical limit and risk functions for p and u charts:
  `pchart_limits()`, `pchart_alpha_risk()`, `uchart_limits()`, and
  `uchart_alpha_risk()`.
- Add pure numerical functions for exact and conventional range-chart limits:
  `r_exact_limits()` and `r_shewhart_limits()`.
- Add pure numerical functions for exact and conventional S-chart limits:
  `s_exact_limits()` and `s_shewhart_limits()`.

## Statistical corrections and compatibility

- Replace the unweighted mean of subgroup proportions with the pooled binomial
  estimator in `cchart.p()`.
- Replace the unweighted mean of subgroup rates with the pooled Poisson
  estimator in `cchart.u()`.
- Correct standardized p and u charts so standardized statistics are not
  divided by subgroup size a second time.
- Correct and harden Cornish-Fisher p- and u-chart implementations and their
  exact binomial/Poisson false-alarm calculations.
- Correct the DS-np limit search so every candidate second-stage threshold is
  evaluated independently; harden decision regions, ARL, ASS, curtailed
  inspection, and plotting.
- Correct the Hotelling T-squared robustness simulation so the documented
  multivariate-t scenario uses the intended elliptical construction, validates
  admissible dimensions/correlation, preserves RNG state, and reports Monte
  Carlo uncertainty explicitly.
- Preserve the historical qcc-compatible center convention for normalized
  S charts while separating its numerical limits from plotting.
- Preserve documented legacy aliases and positional calls for existing chart
  wrappers.

## Scientific validation

- Reproduce published p-chart values from Joekes and Barbosa (2013) with
  explicit provenance, tolerances, and independent binomial-risk oracles.
- Reproduce the published R-chart quantiles, false-alarm risks, ARLs, and
  `d2()`/`d3()` constants from Barbosa, Gneri, and Meneguetti (2013), including
  independent Tippett-distribution integration checks.
- Reproduce published DS-np ARL and ASS values from Joekes, Smrekar, and
  Barbosa (2015), with independent probability and small-sample enumeration
  checks.
- Validate the u-chart Cornish-Fisher expansion independently from Poisson
  cumulants and exact Poisson false-alarm probabilities.
- Reproduce published generalized-variance tables where the source
  parametrization is identifiable; record unresolved source conventions
  explicitly rather than treating them as implementation errors.
- Add a consolidated executable numerical-validation vignette that classifies
  evidence as published reproduction, independent derivation, exact discrete
  calculation, numerical evaluation of an exact distribution, Monte Carlo,
  property testing, or partial reproduction.
- Expand regression and property tests for scaling, monotonicity, boundary
  cases, invalid inputs, random-number-state preservation, and wrapper/core
  equivalence.

## Documentation

- Add executable vignettes for getting started, package positioning,
  high-quality processes, univariate dispersion, multivariate monitoring,
  statistical foundations, numerical validation, and comparison with the
  R statistical-process-control ecosystem.
- Add `paper/statistical-foundations.md` as article-oriented technical source
  material for future manuscript development.
- Expand the README and pkgdown reference index to reflect the public numerical
  API and validated methods.
- Add or revise documentation for public functions, Phase I/Phase II
  conventions, exact-versus-approximate terminology, and S3 methods.

## Engineering and release quality

- Separate numerical kernels from chart construction and plotting across the
  audited p, u, R, S, DS-np, generalized-variance, and `tr(V)` families.
- Add GitHub Actions for R CMD check on Linux (R-devel, release, oldrel),
  Windows, and macOS, plus pkgdown, URL, spelling/documentation-quality, and
  test-coverage checks.
- Add Codecov reporting and substantially expand the automated test suite.
- Improve input validation and replace silent error-like return values with
  explicit errors where audited.
- Remove `attach()`-based examples from audited interfaces and keep vignettes
  executable in the documentation build.

# IQCC 0.7.1

## Bug Fixes

- Fix `d3()` operator precedence bug: `d2[1]` was always used instead of `d2[i]`, producing wrong results when called with a vector of sample sizes (#2)
- Fix `stats()` operator precedence bug: `1:m-1` evaluated as `(1:m) - 1`, producing index 0 on first iteration (affects all n=1 multivariate calculations)
- Fix `cchart.T2.2()` operator precedence bugs: `is.null(x) == FALSE` patterns were fragile (5 occurrences)
- Fix `cchart.T2.1()` incorrect UCL formula for Phase I with n=1: now uses correct beta distribution parameters per Montgomery 7th ed. (#1)
- Fix `cchart.S()` type parameter mismatch: code checked for `"c"` but documentation specified `"e"` for exact limits
- Fix `cchart.Xbar_R()` typo: `add.stat` corrected to `add.stats`
- Fix standardized p-chart and u-chart: removed erroneous multiplication by sample size
- Fix `cchart.T2.1()` error handling: `sprintf()` replaced with `stop()`
- Fix error messages throughout: `return("msg")` replaced with `stop("msg")`
- Correct spelling in standardized, Shewhart, and approximation documentation.
- Add input validation (`n >= 2`) to `d2()`, `d3()`, `c4()`

## Documentation

- Fix 11 typos across R source files
- Fix 3 broken examples (missing required `sizes` argument)
- Fix 8 inaccurate @param/@return descriptions
- Export previously unexported `cchart.Xbar()` function
- Add comprehensive roxygen documentation for `cchart.Xbar()`

## Code Quality

- Create `constants.R` with `ALPHA`, `SIGMA_MULT`, `Q_LOWER`, `Q_UPPER` constants
- Replace 15+ hardcoded magic numbers across 7 files
- Standardize `T`/`F` to `TRUE`/`FALSE` throughout
- Standardize `=` to `<-` for assignment
- Replace `is.null(x) == FALSE` with `!is.null(x)`
- Fix `table.qtukey()`: remove unnecessary function definitions, add `invisible()` return
- Pre-allocate vectors in `d2()` and `d3()` (was O(n²) with `append()`)
- Replace `matrix(nrow=m, ncol=1)` with `numeric(m)` for control limits

## Infrastructure

- Move `miscTools` from `Depends` to `Imports`
- Replace blanket `import()` with selective `importFrom()` directives
- Update minimum R version to 3.5.0
- Add `Language: en-US` to DESCRIPTION
- Clean up `.Rbuildignore` and `.gitignore`
- Update README with usage examples and feature overview

## Testing

- Create testthat test suite with 8 test files and 53 tests
- Add tests for all exported functions
- Add input validation and edge case tests
