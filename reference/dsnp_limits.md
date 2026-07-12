# Double-Sampling np Chart: Limit Search

Search for feasible fractional control limits for the double-sampling np
chart by enumerating integer threshold combinations and evaluating each
candidate using the numeric core functions `dsnp_prob_accept`,
`dsnp_arl`, and `dsnp_ass`.

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

  The in-control nonconforming proportion. Must be in \[0, 1\].

- n1:

  The first-stage sample size (positive integer).

- n2:

  The second-stage sample size (positive integer).

- alpha:

  The maximum desired false alarm probability at p0. Must be in (0, 1).
  When `conservative = TRUE`, candidates with p_signal0 \<= alpha are
  preferred.

- p1:

  Optional out-of-control proportion. When provided, candidates are
  ranked by ARL1 (lower is better).

- conservative:

  Logical. If `TRUE` (default), feasible candidates (p_signal0 \<=
  alpha) are ranked first. If `FALSE`, all candidates are returned
  ordered by proximity to alpha.

- allow_empty_warning:

  Logical. If `FALSE` (default), discard candidates with no integer in
  the warning zone (ucl1_reject \<= wl_accept + 1). This avoids
  single-sample disguised as DS-np.

- max_results:

  Maximum number of candidates to return. Must be a positive integer.

## Value

A list with the following elements:

- best:

  The first row of candidates (a data.frame row).

- candidates:

  A data.frame with all evaluated candidates, sorted by the ranking
  criteria. Columns include wl, ucl1, ucl2, wl_accept, ucl1_reject,
  ucl2_accept, pt0, p_signal0, arl0, ass0, feasible, and (if p1 is
  provided) pt1, p_signal1, arl1, ass1.

- p0, p1, n1, n2, alpha:

  The input parameters.

- conservative, allow_empty_warning:

  The input flags.

## Details

The function enumerates all valid combinations of integer thresholds
(wl_accept, ucl1_reject, ucl2_accept), converts them to fractional
limits compatible with `dsnp_prob_accept`, and evaluates each
candidate's performance at the in-control proportion p0. When p1 is
supplied, out-of-control metrics are also computed and used for ranking.

When the warning zone is empty (ucl1_reject = wl_accept + 1), the chart
degenerates to a single-sample scheme and probabilities are computed
directly via `pbinom` without calling the core functions.

## References

Joekes, S., Smrekar, M. and Barbosa, E. (2015). Extending a double
sampling control chart for non-conforming proportion in high quality
processes to the case of small samples.

## See also

[dsnp_prob_accept](https://flaviobarros.github.io/IQCC/reference/dsnp_prob_accept.md),
[dsnp_arl](https://flaviobarros.github.io/IQCC/reference/dsnp_arl.md),
[dsnp_ass](https://flaviobarros.github.io/IQCC/reference/dsnp_ass.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

# Small example for fast execution
res <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.05)
res$best
#>    wl ucl1 ucl2 wl_accept ucl1_reject ucl2_accept       pt0  p_signal0     arl0
#> 1 0.5  1.5  2.5         0           2           2 0.9598674 0.04013256 24.91743
#>       ass0 feasible
#> 1 7.036266     TRUE
nrow(res$candidates)
#> [1] 20

# With out-of-control proportion for ranking
res <- dsnp_limits(p0 = 0.05, n1 = 5, n2 = 10, alpha = 0.05,
                   p1 = 0.10)
res$best[, c("wl", "ucl1", "ucl2", "arl0", "arl1", "ass0")]
#>    wl ucl1 ucl2     arl0     arl1     ass0
#> 1 0.5  1.5  2.5 24.91743 5.951221 7.036266
```
