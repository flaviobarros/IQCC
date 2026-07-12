# Changelog

## IQCC 0.8.0

### New statistical methods

- Add the double-sampling np chart family for high-quality processes:
  [`dsnp_prob_accept()`](https://flaviobarros.github.io/IQCC/reference/dsnp_prob_accept.md),
  [`dsnp_arl()`](https://flaviobarros.github.io/IQCC/reference/dsnp_arl.md),
  [`dsnp_ass()`](https://flaviobarros.github.io/IQCC/reference/dsnp_ass.md),
  [`dsnp_limits()`](https://flaviobarros.github.io/IQCC/reference/dsnp_limits.md),
  and
  [`cchart.DSnp()`](https://flaviobarros.github.io/IQCC/reference/cchart.DSnp.md).
- Add generalized variance monitoring through
  [`gv_stat()`](https://flaviobarros.github.io/IQCC/reference/gv_stat.md),
  [`gv_limits()`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md),
  [`gv_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/gv_alpha_risk.md),
  and
  [`cchart.GV()`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md).
- Support normal, Cornish-Fisher, selected exact, and simulation-based
  limits for generalized variance charts.
- Add pure numerical u-chart functions
  [`uchart_limits()`](https://flaviobarros.github.io/IQCC/reference/uchart_limits.md)
  and
  [`uchart_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/uchart_alpha_risk.md).
- Add pure numerical p-chart functions
  [`pchart_limits()`](https://flaviobarros.github.io/IQCC/reference/pchart_limits.md)
  and
  [`pchart_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/pchart_alpha_risk.md).

### Statistical corrections

- Replace the unweighted mean of subgroup proportions with the pooled
  binomial estimator in
  [`cchart.p()`](https://flaviobarros.github.io/IQCC/reference/cchart.p.md).
- Replace the unweighted mean of subgroup rates with the pooled Poisson
  estimator in
  [`cchart.u()`](https://flaviobarros.github.io/IQCC/reference/cchart.u.md).
- Correct standardized p and u charts so standardized statistics are not
  divided by subgroup size a second time.
- Audit the Cornish-Fisher p-chart implementation against the
  operational formulas and published numerical examples.
- Derive and validate CF1 and CF2 u-chart limits from Poisson cumulants.
- Correct the DS-np limit search so every candidate second-stage
  threshold is evaluated independently.
- Harden DS-np decision regions, probability calculations, ARL, ASS, and
  plotting.

### Scientific validation

- Reproduce published p-chart false-alarm examples from Joekes and
  Barbosa (2013).
- Reproduce published DS-np ARL and ASS values from Joekes, Smrekar, and
  Barbosa (2015).
- Reproduce published generalized variance limits for dimensions two and
  three.
- Add independent numerical oracles for discrete DS-np probabilities and
  exact dimension-two generalized variance risk.
- Add regression tests for scaling, monotonicity, boundary cases,
  invalid inputs, random-number-state preservation, and legacy aliases.

### Documentation

- Add package vignettes on IQCC positioning, high-quality processes, and
  the statistical foundations of the audited methods.
- Add `paper/statistical-foundations.md` as article-oriented technical
  source material.
- Expand the README with scientific positioning, implemented methods,
  references, and development roadmap.
- Add documentation for all new public functions and S3 plotting
  methods.

### Engineering and infrastructure

- Add GitHub Actions workflows for R CMD check, pkgdown, and test
  coverage.
- Add Codecov reporting and README badge.
- Expand automated tests substantially across legacy and new
  functionality.
- Separate numerical kernels from chart construction and plotting where
  audited.
- Improve input validation and replace silent string returns with
  errors.
- Remove examples based on
  [`attach()`](https://rdrr.io/r/base/attach.html) from audited
  interfaces.

### Compatibility

- Preserve legacy p-chart aliases `norm`, `CF`, and `std`.
- Preserve legacy u-chart aliases `norm`, `CF`, and `std`; historical
  `CF` maps to the two-term Cornish-Fisher formula.
- Existing chart wrappers remain available while exposing new pure
  numerical APIs for reproducible calculations.

## IQCC 0.7.1

### Bug Fixes

- Fix [`d3()`](https://flaviobarros.github.io/IQCC/reference/d3.md)
  operator precedence bug: `d2[1]` was always used instead of `d2[i]`,
  producing wrong results when called with a vector of sample sizes
  ([\#2](https://github.com/flaviobarros/IQCC/issues/2))
- Fix
  [`stats()`](https://flaviobarros.github.io/IQCC/reference/stats.md)
  operator precedence bug: `1:m-1` evaluated as `(1:m) - 1`, producing
  index 0 on first iteration (affects all n=1 multivariate calculations)
- Fix
  [`cchart.T2.2()`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.2.md)
  operator precedence bugs: `is.null(x) == FALSE` patterns were fragile
  (5 occurrences)
- Fix
  [`cchart.T2.1()`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.1.md)
  incorrect UCL formula for Phase I with n=1: now uses correct beta
  distribution parameters per Montgomery 7th
  ed. ([\#1](https://github.com/flaviobarros/IQCC/issues/1))
- Fix
  [`cchart.S()`](https://flaviobarros.github.io/IQCC/reference/cchart.S.md)
  type parameter mismatch: code checked for `"c"` but documentation
  specified `"e"` for exact limits
- Fix
  [`cchart.Xbar_R()`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar_R.md)
  typo: `add.stat` corrected to `add.stats`
- Fix standardized p-chart and u-chart: removed erroneous multiplication
  by sample size
- Fix
  [`cchart.T2.1()`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.1.md)
  error handling: [`sprintf()`](https://rdrr.io/r/base/sprintf.html)
  replaced with [`stop()`](https://rdrr.io/r/base/stop.html)
- Fix error messages throughout: `return("msg")` replaced with
  `stop("msg")`
- Fix typos: “Stardardized” → “Standardized”, “Shewart” → “Shewhart”,
  “aprroximation” → “approximation”
- Add input validation (`n >= 2`) to
  [`d2()`](https://flaviobarros.github.io/IQCC/reference/d2.md),
  [`d3()`](https://flaviobarros.github.io/IQCC/reference/d3.md),
  [`c4()`](https://flaviobarros.github.io/IQCC/reference/c4.md)

### Documentation

- Fix 11 typos across R source files
- Fix 3 broken examples (missing required `sizes` argument)
- Fix 8 inaccurate [@param](https://github.com/param)/@return
  descriptions
- Export previously unexported
  [`cchart.Xbar()`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar.md)
  function
- Add comprehensive roxygen documentation for
  [`cchart.Xbar()`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar.md)

### Code Quality

- Create `constants.R` with `ALPHA`, `SIGMA_MULT`, `Q_LOWER`, `Q_UPPER`
  constants
- Replace 15+ hardcoded magic numbers across 7 files
- Standardize `T`/`F` to `TRUE`/`FALSE` throughout
- Standardize `=` to `<-` for assignment
- Replace `is.null(x) == FALSE` with `!is.null(x)`
- Fix
  [`table.qtukey()`](https://flaviobarros.github.io/IQCC/reference/table.qtukey.md):
  remove unnecessary function definitions, add
  [`invisible()`](https://rdrr.io/r/base/invisible.html) return
- Pre-allocate vectors in
  [`d2()`](https://flaviobarros.github.io/IQCC/reference/d2.md) and
  [`d3()`](https://flaviobarros.github.io/IQCC/reference/d3.md) (was
  O(n²) with [`append()`](https://rdrr.io/r/base/append.html))
- Replace `matrix(nrow=m, ncol=1)` with `numeric(m)` for control limits

### Infrastructure

- Move `miscTools` from `Depends` to `Imports`
- Replace blanket `import()` with selective `importFrom()` directives
- Update minimum R version to 3.5.0
- Add `Language: en-US` to DESCRIPTION
- Clean up `.Rbuildignore` and `.gitignore`
- Update README with usage examples and feature overview

### Testing

- Create testthat test suite with 8 test files and 53 tests
- Add tests for all exported functions
- Add input validation and edge case tests
