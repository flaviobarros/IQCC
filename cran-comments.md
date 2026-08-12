## Test environments

Release-candidate checks completed on GitHub Actions (2026-08-12):

- Ubuntu Linux, R release;
- Ubuntu Linux, R-devel;
- Ubuntu Linux, R oldrel-1;
- Windows, R release;
- macOS, R release.

The repository also runs independent pkgdown, URL, documentation-quality/spelling,
and test-coverage workflows.

Still to be completed immediately before CRAN submission:

- final source tarball built with current R release or R-patched;
- `R CMD check --as-cran` on that tarball with current R-devel (or an explained
  current release/R-patched environment if R-devel is unavailable);
- win-builder or an equivalent CRAN pre-submission service.

## R CMD check results

The GitHub Actions R-CMD-check matrix passes on all five configured platforms/R
versions listed above. The workflow runs CRAN-style checks with `--as-cran` and
fails on warnings.

Final external pre-submission results will be recorded here before upload to CRAN.
The release criterion is:

- 0 errors;
- 0 unexplained warnings;
- every NOTE reviewed and, if unavoidable, explained here.

## Release summary

IQCC 0.8.0 is a substantial update to the CRAN 0.7 release. It adds and validates:

- pure numerical functions for p, u, R, and S chart limits and false-alarm
  calculations where applicable;
- double-sampling np charts for high-quality processes, including ARL, ASS,
  design search, and curtailed-inspection support;
- generalized variance and auxiliary trace-based multivariate variability
  monitoring;
- systematic published numerical validation, independent derivations/oracles,
  and an executable validation vignette;
- expanded vignettes, pkgdown documentation, cross-platform CI, and test
  coverage.

The release also corrects pooled parameter estimation and standardized p/u chart
calculations, strengthens the Hotelling T-squared robustness simulation, and
preserves documented legacy aliases and chart-wrapper compatibility.

## Reverse dependencies

CRAN currently lists one reverse strong dependency:

- `RcmdrPlugin.UCA`.

A reverse-dependency check against the final release candidate is required before
submission. The public API used by historical IQCC chart wrappers has been kept
available, but this does not replace a formal reverse-dependency check.

## Metadata and citation review

- `Version` is 0.8.0.
- The maintainer remains Flavio Barros at the same email address used by the
  current CRAN release.
- `Authors@R` retains the CRAN 0.7 authors/contributor roles: Flavio Barros
  [aut, cre], Emanuel Barbosa [ctb], Elias Goncalves [ctb], and Daniela Recchia
  [ctb].
- License remains GPL-2.
- Canonical CRAN package DOI `10.32614/CRAN.package.IQCC` is used in
  `inst/CITATION`.
- Repository and bug-report URLs use HTTPS.

## Additional notes

- This is an update to an established CRAN package, not a new submission.
- No license or maintainer change is being made.
- Experimental post-0.8 work (including generic generalized-variance quantiles
  and additional rare-defect chart families) is deliberately excluded from the
  release candidate.
- This file remains a release-candidate record until the final tarball,
  reverse-dependency check, and external CRAN-style check are completed.
