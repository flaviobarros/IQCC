# IQCC 0.8.0 release audit

Date: 2026-07-14

This audit maps the current repository state against the release objective:

> IQCC: exact and corrected control charts for small samples, rare defects and
> multivariate variability.

The audit is intentionally conservative. A method is marked complete only when
the public numerical API, chart wrapper where applicable, tests, and user-facing
documentation are present in `origin/master` or in an open PR with passing
checks. Published-value validation is marked blocked when the primary source is
not available in this environment.

## Current branches and PRs

| Item | Status | Evidence | Release impact |
|---|---|---|---|
| PR #58, DS-np design search | Open, checks passing before this audit | Branch `feat/dsnp-design`; adds `dsnp_design()` and ranking tests | Required before closing the implementation part of #51 |
| PR #60, `tr(V)` chart | Draft, created from this audit session | Branch `agent/trv-chart`; adds `trv_stat()`, `trv_limits()`, `trv_alpha_risk()`, `cchart.trV()` | Advances #8, pending review and CI |
| Issue #51, DS-np scientific validation | Blocked | `/mnt/data` is not available; only one published DS-np plan is present in repo docs/tests | Cannot be completed without the DS-np paper tables |

## Method map

| Area | Implemented | Partially implemented | Not implemented | Outdated issue | Blocker | Priority | Minimum next PR |
|---|---|---|---|---|---|---|---|
| Corrected p chart | `pchart_limits()`, `pchart_alpha_risk()`, `cchart.p()`; pooled estimator; normal/CF1/CF2/standardized modes | Additional published fixtures can still be centralized under #10 | None known for 0.8.0 | Older roadmap references to missing pure API are stale | Primary paper available only through existing encoded tests/docs, not local PDF | P0 validation | Add a validation-fixture table with source/table/row metadata for existing p-chart tests |
| Corrected u chart | `uchart_limits()`, `uchart_alpha_risk()`, `cchart.u()`; pooled Poisson estimator; normal/CF1/CF2/standardized modes | Published-reference status needs clearer documentation | None known for 0.8.0 | Roadmap still treats u chart as unaudited | Need source line for historical CF formula if release note must cite it precisely | P0 validation | Document u-chart formula provenance and add source-tagged tests |
| DS-np numeric core | `dsnp_prob_accept()`, `dsnp_arl()`, `dsnp_ass()`, `dsnp_limits()`, `cchart.DSnp()` | Joint design search exists only in PR #58 until merged | Scientific validation for at least three published design rows | #51 body is partly stale after PR #58 | Missing DS-np PDF/tables; #58 not merged | P0 | After #58 merge, add published validation fixtures from paper tables |
| DS-np curtailed inspection | Signal probability and ARL do not depend on curtailed second-stage inspection, but ASS may | Current `dsnp_ass()` assumes a full second sample whenever the first-stage count is in the continuation region | Expected second-stage inspected count under curtailed inspection | Not yet represented as a dedicated issue | Need published convention or explicit project decision | P0 scientific | Add `curtailed = FALSE/TRUE` or a separate pure ASS function after source review |
| R chart | `cchart.R()`, constants, exact-risk diagnostics, tests, documentation | Central pure limit API could still be extracted under #6 | None known for 0.8.0 if current API is accepted | Roadmap items that say no exact R support are stale | None currently identified | P1 | Extract pure R-chart limit/risk functions only if API stabilization requires it |
| Generalized variance `|S|` | `gv_stat()`, `gv_limits()`, `gv_alpha_risk()`, `cchart.GV()`; normal, CF, selected exact, simulation | Generic exact quantiles beyond implemented p = 2 and selected p = 3 are not claimed | Meijer-G or generic product-chi-square exact quantile engine | #7 is mostly stale; main acceptance criteria are implemented | Missing primary report PDF for full published-example audit | P0/P1 | Update or close #7 as implemented except generic exact quantiles; split Meijer-G into a separate research issue |
| Auxiliary `tr(V)` | Implemented in PR #60 | Published report example not reproduced because the PDF is unavailable | Joint visual wrapper with `|S|` is optional and not included | #8 will be current after PR #60 review | Missing primary report PDF for the published example | P0 | Review PR #60; then add published example once the report is available |
| Hotelling T-squared | Legacy Phase I and Phase II functions and wrappers exist | Asymptotic robustness theorem is not documented or encoded | Explicit robustness assumptions, limits, and numerical study | Roadmap does not reflect existing implementation versus missing robustness result | Missing Hotelling robustness PDF | P0/P1 | Add a documentation-only theorem statement with hypotheses after source review; avoid changing limits without proof |
| Release engineering | Version is `0.8.0`; CI matrix, pkgdown, coverage, URL check workflows exist | `R CMD check --as-cran` has known notes; pkgdown has alt-text notices | Final CRAN submission artifact and revdep notes | #27 prerequisites still list older issue numbers as blockers | Scientific validation gates #51/#10 remain unresolved | P1 | Create release-candidate PR after #58/#60 and validation source availability are resolved |
| Documentation/JSS roadmap | README, NEWS, pkgdown, three vignettes, `paper/statistical-foundations.md` exist | Getting-started and multivariate vignettes remain open work | Replication package and package-comparison article sections | #50 is still a useful umbrella, not stale | Scientific fixtures are incomplete without PDFs | P1/P2 | Split #42/#43 into API-contract and getting-started PRs |

## Issues requiring update

| Issue | Recommended action | Rationale |
|---|---|---|
| #7 | Mark as mostly implemented or close after opening a narrower Meijer-G/generic exact-quantile issue | `gv_stat()`, `gv_limits()`, `gv_alpha_risk()`, `cchart.GV()`, tests, README and vignettes are already present |
| #8 | Keep open until PR #60 is reviewed and merged | PR #60 implements the core exact chi-square trace chart but lacks the published report example because the PDF is unavailable |
| #13 | Update roadmap ordering | The roadmap still places #7/#8 later, while `|S|` is already implemented and `tr(V)` has a PR |
| #27 | Keep open | Release gate still depends on scientific validation, source availability, final CRAN checks, and issue cleanup |
| #51 | Keep open and blocked | The implementation part is covered by PR #58, but the required published-table validation cannot be completed without source data |

## Release blockers

1. Merge or explicitly reject PR #58.
2. Obtain the DS-np paper tables needed for #51.
3. Review PR #60 and decide whether property-based `tr(V)` validation is enough for 0.8.0 or whether the published report example is mandatory before merge.
4. Decide the curtailed-inspection convention for DS-np ASS.
5. Update #7/#13 to avoid reimplementing already merged `|S|` functionality.
6. Run the final release gate from #27 after scientific blockers are resolved.

## Recommended next PR sequence

1. DS-np validation fixtures for #51, based on the primary paper tables.
2. DS-np curtailed ASS convention, if the paper or project decision requires it.
3. Roadmap cleanup for #7/#13/#27 after PR #58 and PR #60 are reviewed.

Do not combine these into a single PR. The first two are scientific changes;
the third is release governance.
