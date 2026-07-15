# Double-Sampling np Chart: Limit Search

Search for feasible fractional control limits for the double-sampling np
chart by enumerating integer threshold combinations and evaluating each
candidate with the shared numerical core used by
[`dsnp_prob_accept()`](https://flaviobarros.github.io/IQCC/reference/dsnp_prob_accept.md),
[`dsnp_arl()`](https://flaviobarros.github.io/IQCC/reference/dsnp_arl.md),
and
[`dsnp_ass()`](https://flaviobarros.github.io/IQCC/reference/dsnp_ass.md).
This function searches limits for fixed sample sizes; it does not
optimize the complete sampling design.

## Usage

``` r
dsnp_limits(
  p0,
  n1,
  n2,
  alpha = 0.0027,
  p1 = NULL,
  conservative = TRUE,
  allow_empty_warning = FALSE,
  max_results = 20
)
```

## Arguments

- p0:

  In-control nonconforming proportion, a finite scalar in \\\[0, 1\]\\.

- n1:

  First-stage sample size, a positive integer.

- n2:

  Second-stage sample size, a positive integer.

- alpha:

  Maximum desired false-alarm probability, a finite scalar in \\(0,
  1)\\.

- p1:

  Optional out-of-control proportion, a finite scalar in \\\[0, 1\]\\.
  When supplied, ARL1 is the primary performance ranking.

- conservative:

  Logical. If `TRUE`, candidates satisfying `p_signal0 <= alpha` are
  ranked before infeasible candidates.

- allow_empty_warning:

  Logical. If `FALSE`, discard candidates with no integer count in the
  warning zone.

- max_results:

  Positive integer maximum number of ranked candidates to retain.

## Value

A list with the following elements:

- best:

  The first row of `candidates`, retained as a data frame.

- candidates:

  A data frame containing up to `max_results` ranked candidates. Columns
  are `wl`, `ucl1`, `ucl2`, `wl_accept`, `ucl1_reject`, `ucl2_accept`,
  `pt0`, `p_signal0`, `arl0`, `ass0`, and `feasible`; when `p1` is
  supplied, `pt1`, `p_signal1`, `arl1`, and `ass1` are also included.

- p0, p1, n1, n2, alpha:

  The validated input parameters.

- conservative, allow_empty_warning:

  The validated input flags.

## Details

The function enumerates valid combinations of `wl_accept`,
`ucl1_reject`, and `ucl2_accept`, then converts them to fractional
limits. A candidate is feasible when its in-control signal probability
is no greater than `alpha`.

With `p1`, candidates are ranked by feasibility when
`conservative = TRUE`, then by increasing ARL1, ASS0, and distance from
`alpha`. Without `p1`, they are ranked by feasibility, distance from
`alpha`, ASS0, and ARL0. Setting `conservative = FALSE` removes
feasibility from the front of the corresponding ordering.

When `ucl1_reject = wl_accept + 1`, the warning zone is empty and the
chart degenerates to a single-sample scheme. Such candidates are
excluded by default; when allowed, their probabilities are computed
directly with
[`stats::pbinom()`](https://rdrr.io/r/stats/Binomial.html).

## References

Joekes, S., Smrekar, M. and Barbosa, E. P. (2015). Extending a double
sampling control chart for non-conforming proportion in high quality
processes to the case of small samples. *Statistical Methodology*, 23,
35–49.

## See also

[`dsnp_prob_accept`](https://flaviobarros.github.io/IQCC/reference/dsnp_prob_accept.md),
[`dsnp_arl`](https://flaviobarros.github.io/IQCC/reference/dsnp_arl.md),
[`dsnp_ass`](https://flaviobarros.github.io/IQCC/reference/dsnp_ass.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r
limits0 <- dsnp_limits(0.05, 5, 10, alpha = 0.05)
limits0$best
#>    wl ucl1 ucl2 wl_accept ucl1_reject ucl2_accept       pt0  p_signal0     arl0
#> 1 0.5  1.5  2.5         0           2           2 0.9598674 0.04013256 24.91743
#>       ass0 feasible
#> 1 7.036266     TRUE
nrow(limits0$candidates)
#> [1] 20

limits1 <- dsnp_limits(0.05, 5, 10, alpha = 0.05, p1 = 0.10)
limits1$best[, c("wl", "ucl1", "ucl2", "arl0", "arl1", "ass0")]
#>    wl ucl1 ucl2     arl0     arl1     ass0
#> 1 0.5  1.5  2.5 24.91743 5.951221 7.036266
```
