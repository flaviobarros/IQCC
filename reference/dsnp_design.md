# Double-Sampling np Chart: Complete Design Search

Perform a joint discrete search over first-stage sample size (n1),
second-stage sample size (n2), and DS-np control limits to find optimal
chart designs. This function performs an exhaustive search over the
Cartesian product of `n1_range` and `n2_range`, calling
[`dsnp_limits`](https://flaviobarros.github.io/IQCC/reference/dsnp_limits.md)
for each pair and collecting the best feasible candidates across all
pairs.

## Usage

``` r
dsnp_design(
  p0,
  p1,
  n1_range,
  n2_range,
  arl0_min = 200,
  alpha = NULL,
  objective = c("arl1", "ass0", "weighted"),
  weights = c(arl1 = 1, ass0 = 1),
  allow_empty_warning = FALSE,
  max_results = 20,
  progress = FALSE
)

# S3 method for class 'dsnp_design'
print(x, ...)
```

## Arguments

- p0:

  In-control nonconforming proportion. Scalar in (0, 1).

- p1:

  Out-of-control nonconforming proportion. Scalar in (0, 1). Must be
  greater than `p0`.

- n1_range:

  Integer vector of first-stage sample sizes to evaluate. Positive
  integers, no duplicates, no NAs.

- n2_range:

  Integer vector of second-stage sample sizes to evaluate. Positive
  integers, no duplicates, no NAs.

- arl0_min:

  Minimum acceptable in-control ARL. Scalar greater than 1, or `NULL` if
  `alpha` is supplied.

- alpha:

  Maximum acceptable false alarm probability. Scalar in (0, 1), or
  `NULL` if `arl0_min` is supplied. When both are provided, candidates
  must satisfy both constraints.

- objective:

  Optimization objective. One of `"arl1"` (minimize out-of-control ARL),
  `"ass0"` (minimize in-control ASS), or `"weighted"` (minimize a
  weighted combination of normalized ARL1 and ASS0).

- weights:

  Named numeric vector with elements `arl1` and `ass0`. Both must be
  non-negative and at least one must be positive. Only used when
  `objective = "weighted"`.

- allow_empty_warning:

  Logical. If `FALSE` (default), discard candidates with no integer in
  the warning zone.

- max_results:

  Maximum number of feasible candidates to return. Positive integer.

- progress:

  Logical. If `TRUE`, print progress messages during the search.

- x:

  An object of class `"dsnp_design"`.

- ...:

  Additional arguments (ignored).

## Value

An object of class `"dsnp_design"` with the following elements:

- best:

  The top-ranked feasible candidate (a one-row data.frame).

- candidates:

  Up to `max_results` feasible candidates, sorted by the chosen
  objective.

- parameters:

  A list of the input parameters.

- search:

  A list with search summary counts:

- failures:

  A data.frame of failed pairs with columns n1, n2, and message. Empty
  if no failures occurred.

## Details

The search is discrete and exhaustive within the supplied ranges; it is
not a continuous optimization. The cost grows with the number of (n1,
n2) pairs and the number of limit candidates evaluated per pair. This
function does not implement curtailed inspection.

When `alpha` is `NULL` and `arl0_min` is provided, an effective alpha of
`1 / arl0_min` is used to guide
[`dsnp_limits()`](https://flaviobarros.github.io/IQCC/reference/dsnp_limits.md),
but final feasibility is checked against the explicit `arl0 >= arl0_min`
condition.

## References

Joekes, S., Smrekar, M. and Barbosa, E. (2015). Extending a double
sampling control chart for non-conforming proportion in high quality
processes to the case of small samples.

## See also

[dsnp_limits](https://flaviobarros.github.io/IQCC/reference/dsnp_limits.md),
[dsnp_prob_accept](https://flaviobarros.github.io/IQCC/reference/dsnp_prob_accept.md),
[dsnp_arl](https://flaviobarros.github.io/IQCC/reference/dsnp_arl.md),
[dsnp_ass](https://flaviobarros.github.io/IQCC/reference/dsnp_ass.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

# Small example for fast execution
res <- dsnp_design(
    p0 = 0.05, p1 = 0.10,
    n1_range = 5:6, n2_range = 8:10,
    arl0_min = 50, objective = "arl1"
)
res$best[, c("n1", "n2", "wl", "ucl1", "ucl2", "arl0", "arl1")]
#>   n1 n2  wl ucl1 ucl2     arl0     arl1
#> 1  6 10 1.5  2.5  2.5 69.03209 12.50785
```
