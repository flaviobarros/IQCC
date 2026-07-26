# Double-Sampling np Chart: Average Sample Size

Compute the average sample size for the double-sampling np chart.

## Usage

``` r
dsnp_ass(p, n1, n2, wl, ucl1, ucl2 = NULL, curtailed = FALSE)
```

## Arguments

- p:

  Nonconforming proportion to evaluate, a finite numeric scalar or
  vector in \\\[0, 1\]\\.

- n1:

  First-stage sample size, a positive integer.

- n2:

  Second-stage sample size, a positive integer.

- wl:

  Finite fractional warning limit.

- ucl1:

  Finite fractional first-stage upper control limit greater than `wl`.

- ucl2:

  Finite fractional second-stage upper control limit. Required when
  `curtailed = TRUE`.

- curtailed:

  Logical. If `FALSE` (default), assume complete inspection of every
  second-stage sample. If `TRUE`, use curtailed (truncated) inspection
  within the second sample.

## Value

A list with:

- ass:

  Average sample size (numeric vector).

- p_second:

  Probability that the second sample is required.

- n1, n2, wl, ucl1, ucl2:

  Validated chart parameters.

- curtailed:

  The convention used.

## Details

By default (`curtailed = FALSE`), every second-stage sample that is
requested is fully inspected. Therefore \$\$ASS(p) = n_1 + n_2
P_p(\text{second stage}).\$\$

When `curtailed = TRUE`, inspection of the second sample stops as soon
as the cumulative count of non-conformities exceeds `ucl2`. For each
warning-zone first-stage count \\d_1\\, define \\r(d_1) = \lfloor ucl_2
\rfloor - d_1 + 1\\ as the number of non-conformances needed to reject.
The expected number of stage-2 items inspected is \$\$E\[M(d_1)\] =
\sum\_{j=0}^{n_2 - 1} P(Bin(j, p) \le r(d_1) - 1),\$\$ with
\\E\[M(d_1)\] = 0\\ when \\r(d_1) \le 0\\. Then
\$\$ASS\_{\text{curtailed}}(p) = n_1 + \sum\_{d_1 = a+1}^{b-1} P(D_1 =
d_1) \\ E\[M(d_1)\].\$\$

Curtailed inspection does not change the signal probability or ARL; it
only reduces the number of items inspected when the eventual decision is
already determined before the full second sample is observed.

## References

Joekes, S., Smrekar, M. and Barbosa, E. P. (2015). Extending a double
sampling control chart for non-conforming proportion in high quality
processes to the case of small samples. *Statistical Methodology*, 23,
35–49.
[doi:10.1016/j.stamet.2014.09.003](https://doi.org/10.1016/j.stamet.2014.09.003)
.

## See also

[dsnp_prob_accept](https://flaviobarros.github.io/IQCC/reference/dsnp_prob_accept.md),
[dsnp_arl](https://flaviobarros.github.io/IQCC/reference/dsnp_arl.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r
dsnp_ass(0.005, 34, 162, 1.5, 2.5)$ass
#> [1] 35.93534

# Curtailed inspection
dsnp_ass(0.005, 34, 162, 1.5, 2.5, ucl2 = 4.5, curtailed = TRUE)$ass
#> [1] 35.90933
```
