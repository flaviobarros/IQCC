# Generalized Variance Control Chart

Construct and optionally plot a Shewhart-type chart for multivariate
process variability based on the generalized variance statistic
\\\|S\|\\, the determinant of each subgroup sample covariance matrix.

## Usage

``` r
cchart.GV(
  x1,
  size = NULL,
  x2 = NULL,
  Sigma = NULL,
  alpha = 0.0027,
  type = c("normal", "cf", "exact", "simulation"),
  side = c("upper", "two.sided"),
  cf_order = 1,
  nsim = 1e+05,
  seed = NULL,
  plot = TRUE,
  ...
)

# S3 method for class 'cchart.GV'
plot(x, ...)
```

## Arguments

- x1:

  Phase I subgroup data in any format accepted by
  [`gv_stat()`](https://flaviobarros.github.io/IQCC/reference/gv_stat.md):
  a list of equal-sized numeric matrices, a
  subgroup-by-observation-by-variable array, or a numeric matrix
  containing consecutive subgroups.

- size:

  Positive integer subgroup size when `x1` or `x2` is a stacked matrix.
  It is ignored for list and array input.

- x2:

  Optional Phase II subgroup data in a format accepted by
  [`gv_stat()`](https://flaviobarros.github.io/IQCC/reference/gv_stat.md).
  Phase II subgroups must have the same number of observations and
  variables as the Phase I subgroups.

- Sigma:

  Optional finite symmetric positive-definite \\p \times p\\ in-control
  covariance matrix. If supplied, its determinant defines the chart
  limits. If omitted, the function averages the Phase I covariance
  matrices and estimates \$\$\|\Sigma\| = \|\bar S\| / b_3,\$\$ where
  \\b_3\\ is the finite-Phase-I determinant bias correction based on the
  number of Phase I subgroups, subgroup size, and dimension.

- alpha:

  Nominal probability of a false alarm per subgroup, strictly between 0
  and 1.

- type:

  Character string selecting `"normal"`, `"cf"`, `"exact"`, or
  `"simulation"` limits. See
  [`gv_limits`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md).

- side:

  Either `"upper"` or `"two.sided"`.

- cf_order:

  Integer 1 or 2 controlling the Cornish-Fisher correction when
  `type = "cf"`.

- nsim:

  Integer number of Monte Carlo draws, at least 1000, when simulated
  limits are requested.

- seed:

  `NULL` or a finite numeric scalar used as the Monte Carlo seed.

- plot:

  Logical scalar. If `TRUE`, draw the chart before returning the result.
  If `FALSE`, construct the object without plotting.

- ...:

  Additional graphical arguments passed to `plot.cchart.GV()` and then
  to [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html).

- x:

  An object of class `"cchart.GV"`.

## Value

`cchart.GV()` returns an object of class `"cchart.GV"`, a list with
components:

- `statistics`:

  Generalized variances for all Phase I followed by all Phase II
  subgroups.

- `phase1`, `phase2`:

  Integer indices identifying the two phases in `statistics`.

- `limits`:

  The complete list returned by
  [`gv_limits()`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md).

- `out.of.control`:

  Indices for which `statistics < lcl` or `statistics > ucl`. Equality
  to a limit does not signal.

- `Sbar`:

  The average Phase I covariance matrix when `Sigma` is estimated,
  otherwise `NULL`.

- `Sigma`:

  The supplied in-control covariance matrix, or `NULL` when the
  determinant is estimated from Phase I.

- `b3`:

  The determinant bias-correction factor when Phase I estimation is
  used, otherwise `NA_real_`.

- `n`, `p`:

  Subgroup size and process dimension.

- `call`:

  The matched function call.

`plot.cchart.GV()` returns `x` invisibly.

## Details

Phase I subgroups define the chart design and, unless `Sigma` is
supplied, estimate the in-control generalized variance. Optional Phase
II subgroups are then evaluated using the same subgroup size, process
dimension, and fixed control limits.

Each subgroup must contain finite observations on at least two
variables, and the subgroup size must exceed the process dimension. When
`Sigma = NULL`, the average Phase I covariance matrix must itself be
positive definite. The control-line center is the exact in-control mean
of \\\|S\|\\, not generally \\\|\Sigma\|\\.

`plot.cchart.GV()` displays subgroup generalized variances, the center
line, lower and upper limits, a vertical separator before Phase II, and
solid points at signaled subgroups.

## Phase I and Phase II convention

Phase I data are used both as historical plotted points and, when
`Sigma` is absent, to estimate the in-control determinant. Phase II data
do not alter the estimate or the limits. For a strict prospective chart
with known parameters, supply `Sigma`.

## Simulated limits

When `type = "simulation"`, `nsim` and `seed` are passed to
[`gv_limits()`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md).
A supplied seed makes the result reproducible and the caller's existing
random-number state is restored.

## Errors

Errors are raised for invalid subgroup representations, unequal subgroup
dimensions, non-finite observations, \\n \<= p\\, an invalid or
non-positive-definite `Sigma`, a singular Phase I average covariance
matrix, and any unsupported limit configuration documented in
[`gv_limits()`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md).

## References

Barbosa, E. P., Gneri, M. A., and Meneguetti, A. *Improving
Shewhart-type Generalized Variance Control Charts for Multivariate
Process Variability Monitoring using Cornish-Fisher Quantile Correction,
Meijer-G Function and Other Tools*. Research report.

Alt, F. B. (1984). Multivariate quality control. In Johnson, N. L. and
Kotz, S. (eds.), *Encyclopedia of Statistical Sciences*, Vol. 6,
110–122. Wiley.

## See also

[`gv_stat`](https://flaviobarros.github.io/IQCC/reference/gv_stat.md),
[`gv_limits`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md),
[`gv_alpha_risk`](https://flaviobarros.github.io/IQCC/reference/gv_alpha_risk.md),
[`print.cchart.GV`](https://flaviobarros.github.io/IQCC/reference/print.cchart.GV.md)

## Examples

``` r
set.seed(123)
phase1 <- array(rnorm(6 * 8 * 2), dim = c(6, 8, 2))

# Known in-control covariance, exact dimension-two limits.
chart <- cchart.GV(
    phase1,
    Sigma = diag(2),
    type = "exact",
    plot = FALSE
)
chart$limits[c("lcl", "center", "ucl")]
#> $lcl
#> [1] 0
#> 
#> $center
#> [1] 0.8571429
#> 
#> $ucl
#> [1] 4.621594
#> 
summary(chart)
#> Generalized Variance Control Chart
#>   Dimension: p = 2 ; subgroup size n = 8 
#>   Subgroups: 6 (Phase I: 6 ; Phase II: 0 )
#>   Limits: exact / upper ; nominal alpha = 0.0027 
#>   Covariance: supplied Sigma 
#>   LCL = 0 ; center = 0.8571 ; UCL = 4.622 
#>   Signals: 0 

# Estimate |Sigma| from Phase I and evaluate two Phase II subgroups.
phase2 <- array(rnorm(2 * 8 * 2), dim = c(2, 8, 2))
estimated <- cchart.GV(
    phase1,
    x2 = phase2,
    type = "cf",
    plot = FALSE
)
estimated$phase2
#> [1] 7 8
```
