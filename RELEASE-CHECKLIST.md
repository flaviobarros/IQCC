# IQCC 0.8.0 release candidate

This file is intentionally excluded from the source package only if
added to `.Rbuildignore` before CRAN submission. It records
repository-side release work and is not part of the statistical API.

## Completed in repository

- Version 0.8.0 in DESCRIPTION.
- Authors, maintainer, license, URLs, and package DOI reviewed against
  the current CRAN metadata.
- NEWS and README aligned with the frozen 0.8.0 scope.
- `inst/CITATION` reviewed.
- Cross-platform GitHub Actions matrix green on master before the
  release branch.
- Native R 4.1 pipe removed from tests so the declared R \>= 3.5.0
  syntax floor is not contradicted by the test suite.

## Required before CRAN upload

- Merge the release PR only after all branch checks are green.
- Build the source tarball with current R release/R-patched.
- Run `R CMD check --as-cran` on the tarball with current R-devel if
  possible.
- Run win-builder or an equivalent external CRAN pre-submission check.
- Run a reverse-dependency check for `RcmdrPlugin.UCA`.
- Record final results in `cran-comments.md`.
- Create tag/release only after the final tarball is approved.
- Submit the source tarball through the CRAN submission form and confirm
  the submission email.
