# IQCC 0.8.0 release audit

Date: 2026-07-14

This audit maps the current repository state against the release objective:

> IQCC: exact and corrected control charts for small samples, rare defects and
> multivariate variability.

The audit is intentionally conservative. A method is complete only when its
public numerical API, chart wrapper where applicable, tests, and user-facing
documentation are on `origin/master` or in a reviewable PR with passing checks.
Published validation is distinguished from mathematical property tests. A
published example remains partial when the report omits the raw observations or
simulation seed required to rerun it.

## Current branches and PRs

| Item | Status | Evidence | Release impact |
|---|---|---|---|
| PR #58, DS-np design search | Squash-merged into `origin/master` | Commit `f92e2f5`; `dsnp_design()` and deterministic ranking tests are on master | The implementation portion of #51 is merged |
| PR #60, `tr(V)` chart | Ready for review; all eight GitHub checks pass | Head `ba5178b`; exact and simulated limits, risk, Phase I/II chart, published Table 3 tests, and partial Case B reproduction | Advances #8 but does not close it because the complete published Case B signal sequence cannot be rerun |
| PR #61, release audit | Ready for review after this update; GitHub checks pass | Branch `agent/release-080-audit`; this audit file is the only changed path | Must be reviewed after #60 and before new parallel deliveries |
| Issue #8 | Open | Core API and documentation are in ready PR #60; Table 3 is reproduced; Case B is partial | Review/merge #60, then decide whether missing raw Case B data keeps #8 open |
| Issue #51 | Open | `dsnp_design()` is merged; the DS-np paper is locally available | Still needs published-table reproduction and the published `ASS0` constraint formulation |

## Scientific sources

Eight PDFs supplied for this audit are readable from the local untracked
`sources/papers/` directory. They cover the corrected p chart, DS-np, range
charts, generalized variance/Meijer-G/`tr(V)`, S charts, and Hotelling
robustness. The files are research inputs only: none is tracked by Git or
included in PR #60 or #61.

Source availability is no longer a release blocker. The remaining source-level
limitation is narrower: the generalized-variance report does not publish all 70
Case B simulated subgroups or an RNG seed, so Figure 9c cannot be reproduced
point for point without inventing observations.

## Method map

| Area | Current state | Scientific gap | Release status | Minimum next PR |
|---|---|---|---|---|
| Corrected p chart | `pchart_limits()`, `pchart_alpha_risk()`, `cchart.p()` and source-tagged tests exist | Consolidate additional published fixtures and formula-selection documentation under #10 | Validation work remains, but the source PDF is available | Small validation-only PR using the supplied p-chart papers |
| Corrected u chart | Pure limits/risk and chart wrapper exist | Identify and cite the exact historical CF formula adopted by the package | Provenance remains open; no source-availability claim should be made | Documentation and source-tagged tests only |
| DS-np | Numeric core, chart, limit search, and joint `dsnp_design()` search are on master | Reproduce at least three published design rows; confirm `ARL0`, `ARL1`, `ASS0`; add the published optional `ASS0` constraint | #51 remains a P0 blocker | One focused #51 PR for `ass0_max` plus published fixtures |
| DS-np curtailed inspection | Signal probability and ARL are implemented; current ASS assumes a full second sample in the continuation region | Determine whether the paper requires curtailed expected inspection and, if so, encode it explicitly | Decision pending source audit, not source availability | Separate ASS semantics PR only if required |
| R and S charts | Existing charts, constants, exact-risk diagnostics, tests, and documentation | Add source-tagged published reproduction and thematic documentation | Not an immediate implementation blocker | Validation/docs PR scoped to #45 or #10 |
| Generalized variance `|S|` | `gv_stat()`, `gv_limits()`, `gv_alpha_risk()`, and `cchart.GV()` implement normal, CF, selected exact, and simulated paths | Generic Meijer-G quantiles are not claimed; the primary report is now available | #7 body is substantially outdated | Update/close #7 and split generic Meijer-G into a research issue if still desired |
| Auxiliary `tr(V)` | PR #60 is ready with full API, tests, documentation, exact chi-square contract, RNG preservation, Phase I/II, and plug-in-limit caveat | Table 3 is reproduced for N = 3, 16, 30; Case B matrices, determinants, and UCL are reproduced, but raw Phase II signals are unavailable | Awaiting review; #8 remains open as `Part of #8` | No new implementation PR before #60 review |
| Hotelling T-squared | Legacy Phase I/II functions exist | Audit the available robustness paper before changing assumptions or limits | Documentation/research follow-up, not a current code claim | Theorem/provenance documentation after source review |
| Release engineering | Version 0.8.0, CI matrix, coverage, URL checks, and pkgdown workflows exist | Final release candidate, revdep/CRAN notes, and scientific gate decisions remain | #27 is open and its body is outdated | Release-only PR after #8/#51 decisions |
| Documentation/JSS | README, NEWS, pkgdown, and three executable vignettes exist | Getting-started, thematic dispersion/multivariate material, replication, and long-term QA remain | #50 remains useful as an umbrella but its diagnosis is partly stale | Keep #43/#45/#49 separate and defer them until current PR review |

## Published validation in PR #60

Table 3 is reproduced against printed report values, not only against another
call to `qchisq()`. Rows N = 3, 16, and 30 cover small, intermediate, and large
settings. For each printed column, the test records reference, table/line,
dimension, subgroup size, degrees of freedom, nominal probability, published
and calculated values, tolerance 0.03, and error/tolerance ratio.

The numerical audit confirms two probable report inconsistencies:

1. Section 3.3 defines `N = n - 1`, but Table 3 row values use displayed N as
   subgroup size `n`, giving `df = p * (n - 1)`.
2. Values printed under 0.9980 match the 0.9973 quantile, while values under
   0.9973 match the 0.9980 quantile.

PR #60 keeps the correct `qchisq(1 - alpha, df)` convention and documents the
probable errata. Case B is a partial/qualitative reproduction: the rounded Case
A/B matrices reproduce determinants 69.8438 and 66.1893, and Figure 9c's UCL
72.01 is recovered for `p = 3`, `n = 15`, `df = 42`. The synthetic
same-determinant example remains clearly labeled synthetic.

## Issues requiring update

| Issue | Current assessment | Recommended action |
|---|---|---|
| #7 | Open body still says generalized variance is absent | Mark implemented except for an explicitly separate generic Meijer-G research scope |
| #8 | Acceptance checkboxes do not reflect ready PR #60 | Update after review; keep open unless partial Case B evidence is accepted |
| #10 | Validation epic omits completed/advanced p, R, GV, and `tr(V)` fixtures | Refresh checkboxes and remaining source-tagged validation tasks |
| #13 | Roadmap still schedules #7/#8 after work that is already implemented | Reorder phases and remove completed prerequisites |
| #27 | Body still says version 0.7 and single-platform CI | Rewrite the gate around remaining scientific validation and final CRAN/revdep work |
| #50 | Diagnosis predates executable-vignette and pkgdown improvements | Retain as umbrella, but update completed foundations and current gaps |
| #51 | Objective text reads as if joint design search is absent; comments also say PDFs are unavailable | Record #58 as merged; retain published fixtures and `ass0_max` as the remaining scope |

Issues #43, #45, and #49 remain valid future documentation tasks. They are not
outdated, but they should not start in parallel before PRs #60 and #61 are
reviewed and merged.

## Real release blockers

1. Review and merge PR #60, then decide whether the partial Case B reproduction
   is sufficient for 0.8.0 or whether #8 must remain open pending raw data.
2. Complete one focused #51 follow-up using the available DS-np PDF: published
   table fixtures plus the optional `ass0_max` formulation.
3. Resolve the DS-np curtailed-inspection ASS convention only if the paper audit
   shows that it affects the claimed release method.
4. Update stale release/roadmap issues so #27 reflects actual completed work.
5. Run the final release-only gate: clean checks, revdep assessment, CRAN notes,
   metadata, and published pkgdown verification.

## Minimum next PR sequence

1. Review and merge #60 and #61; do not create the #43/#45/#49/#51 worktrees
   before that review cycle is complete.
2. #51 follow-up: add optional `ass0_max` and reproduce at least three published
   DS-np table rows with explicit source, parameters, values, and tolerances.
3. A documentation-only issue cleanup for #7/#8/#10/#13/#27/#50/#51.
4. A release-candidate PR scoped strictly to #27.

Do not combine these into one PR. Scientific changes, issue governance, and the
release candidate need independent review histories.
