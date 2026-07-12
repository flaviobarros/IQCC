## Test environments

To be completed before submission:

- local R release, `R CMD check --as-cran`;
- GitHub Actions: Ubuntu with R release, devel, and oldrel;
- GitHub Actions: Windows with R release;
- GitHub Actions: macOS with R release;
- win-builder or an equivalent CRAN pre-submission service.

## R CMD check results

The final results will be recorded here after the release candidate has passed the
full CI matrix and external pre-submission checks.

Expected release criterion:

- 0 errors;
- 0 warnings not explicitly explained;
- notes reviewed individually and documented below.

## Release summary

IQCC 0.8.0 is a substantial update that adds and validates:

- pure numerical functions for p- and u-chart limits and exact false-alarm risk;
- double-sampling np charts for high-quality processes;
- generalized variance charts with normal, Cornish-Fisher, selected exact, and
  simulation-based limits;
- published numerical validation cases and independent regression oracles;
- expanded vignettes, pkgdown infrastructure, and test coverage.

The release also corrects pooled parameter estimation and standardized plotting in
the p and u charts while preserving documented legacy aliases.

## Reverse dependencies

To be checked before submission. Known reverse dependencies, if any, will be tested
and summarized here.

## Additional notes

- This file is intentionally a release-candidate draft. It must not be treated as
  final until the CI matrix, URL checks, revdep checks, and CRAN-style checks have
  completed.
- Exact bibliographic metadata for the historical package article should be verified
  before adding a second article-level entry to `inst/CITATION`; the current citation
  records the package itself without inventing unverified journal metadata.
