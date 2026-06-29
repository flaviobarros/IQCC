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
- Fix typos: "Stardardized" → "Standardized", "Shewart" → "Shewhart", "aprroximation" → "approximation"
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
