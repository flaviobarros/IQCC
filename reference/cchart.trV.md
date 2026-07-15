# Trace-Statistic Control Chart

Construct and optionally plot a Shewhart-type upper control chart for
multivariate process variability using the standardized trace statistic
\\tr(V)\\.

## Usage

``` r
cchart.trV(
  x,
  size = NULL,
  Sigma0 = NULL,
  alpha = 0.0027,
  type = c("chisq", "simulation"),
  nsim = 1e+05,
  seed = NULL,
  newdata = NULL,
  plot = TRUE,
  ...
)

# S3 method for class 'cchart.trV'
plot(x, ...)

# S3 method for class 'cchart.trV'
print(x, digits = max(3L, getOption("digits") - 3L), ...)

# S3 method for class 'cchart.trV'
summary(object, ...)

# S3 method for class 'summary.cchart.trV'
print(x, digits = max(3L, getOption("digits") - 3L), ...)
```

## Arguments

- x:

  An object of class `"summary.cchart.trV"`.

- size:

  Positive integer subgroup size when `x` or `newdata` is a stacked
  matrix.

- Sigma0:

  Optional finite symmetric positive-definite in-control covariance
  matrix. If omitted, the average Phase I covariance matrix is used as a
  plug-in estimate.

- alpha:

  Nominal upper-tail false-alarm probability, strictly between 0 and 1.

- type:

  Character string selecting the calculation:

  `"chisq"`

  :   Exact chi-square quantile with \\p(n - 1)\\ degrees of freedom.

  `"simulation"`

  :   Monte Carlo quantile from the same chi-square distribution. This
      is mainly a diagnostic and teaching option.

- nsim:

  Integer number of Monte Carlo draws, at least 1000. Used only when
  `type = "simulation"`.

- seed:

  `NULL` or a finite numeric scalar used as the Monte Carlo seed. A
  supplied seed makes simulated limits reproducible and restores the
  caller's existing random-number state on exit.

- newdata:

  Optional Phase II subgroup data in any format accepted by
  [`trv_stat`](https://flaviobarros.github.io/IQCC/reference/trv_stat.md).
  Phase II subgroups must have the same subgroup size and dimension as
  Phase I.

- plot:

  Logical scalar. If `TRUE`, draw the chart before returning the result.

- ...:

  Currently unused.

- digits:

  Number of significant digits used for numerical output.

- object:

  An object of class `"cchart.trV"`.

## Value

An object of class `"cchart.trV"`, a list with components:

- `statistics`:

  Trace statistics for all Phase I followed by all Phase II subgroups.

- `phase1`, `phase2`:

  Integer indices identifying the two phases in `statistics`.

- `limits`:

  The complete list returned by
  [`trv_limits()`](https://flaviobarros.github.io/IQCC/reference/trv_limits.md).

- `out.of.control`:

  Indices for which `statistics > ucl`. Equality to the limit does not
  signal.

- `Sigma0`:

  The supplied or estimated in-control covariance matrix.

- `Sbar`:

  The average Phase I covariance matrix when `Sigma0` is estimated,
  otherwise `NULL`.

- `n`, `p`:

  Subgroup size and process dimension.

- `call`:

  The matched function call.

For `plot.cchart.trV()`, `x` invisibly.

`x`, invisibly.

An object of class `"summary.cchart.trV"`.

`x`, invisibly.

## Details

The `tr(V)` chart complements
[`cchart.GV`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md).
The generalized variance chart monitors determinant changes, while the
trace chart monitors the sum of standardized covariance eigenvalues.
When `Sigma0 = NULL`, limits are plug-in limits conditional on the Phase
I estimate and do not include additional Phase I estimation uncertainty.

## Examples

``` r
set.seed(123)
phase1 <- array(rnorm(6 * 8 * 2), dim = c(6, 8, 2))
chart <- cchart.trV(phase1, Sigma0 = diag(2), plot = FALSE)
chart$limits[c("center", "ucl")]
#> $center
#> [1] 14
#> 
#> $ucl
#> [1] 33.19518
#> 
```
