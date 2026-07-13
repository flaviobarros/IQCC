# Generalized Variance Control Limits

Compute one-sided upper or equal-tail two-sided control limits for the
generalized variance statistic \\\|S\|\\ under independent sampling from
a \\p\\-variate normal process with in-control covariance matrix
\\\Sigma\\.

## Usage

``` r
gv_limits(
  n,
  p,
  det_sigma = 1,
  alpha = 0.0027,
  type = c("normal", "cf", "exact", "simulation"),
  side = c("upper", "two.sided"),
  cf_order = 1,
  nsim = 1e+05,
  seed = NULL
)
```

## Arguments

- n:

  Integer subgroup sample size. It must satisfy \\n \> p\\.

- p:

  Integer process dimension, at least 2.

- det_sigma:

  Positive finite scalar equal to \\\|\Sigma\|\\, the determinant of the
  in-control covariance matrix. Control limits scale linearly with this
  value.

- alpha:

  Nominal probability of a false alarm per subgroup, strictly between 0
  and 1. For `side = "upper"`, all probability is placed in the upper
  tail. For `side = "two.sided"`, `alpha / 2` is placed in each tail.

- type:

  Character string selecting the limit calculation:

  `"normal"`

  :   Moment-matched Gaussian quantiles using the exact mean and
      variance of \\\|S\|\\.

  `"cf"`

  :   Cornish-Fisher corrected Gaussian quantiles using skewness, and
      optionally excess kurtosis.

  `"exact"`

  :   Closed-form chi-square quantiles for \\p=2\\, or the published
      upper-limit table for \\p=3\\, \\n=4,\ldots,15\\, and `alpha`
      equal to 0.0020 or 0.0027.

  `"simulation"`

  :   Monte Carlo quantiles from the Bartlett product-of-chi-squares
      representation.

  Partial matching is not used.

- side:

  Either `"upper"` or `"two.sided"`. Since generalized variance is
  nonnegative, the lower limit for an upper chart is zero. Any negative
  approximated lower limit for a two-sided chart is also truncated to
  zero.

- cf_order:

  Integer 1 or 2. Order 1 uses the skewness correction. Order 2
  additionally uses excess kurtosis and the squared-skewness term. It is
  used only when `type = "cf"`.

- nsim:

  Integer number of Monte Carlo draws, at least 1000. It is used by
  `type = "simulation"` and stored in the returned object for all
  methods.

- seed:

  `NULL` or a finite numeric scalar used as the Monte Carlo seed. A
  supplied seed makes simulated limits reproducible and the caller's
  existing `.Random.seed` is restored on exit.

## Value

A list with components:

- `lcl`, `ucl`:

  Lower and upper control limits.

- `center`:

  The exact in-control mean of \\\|S\|\\; this is the center line used
  by
  [`cchart.GV()`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md).

- `type`, `side`, `alpha`:

  The selected method, chart sidedness, and nominal false-alarm
  probability.

- `n`, `p`, `det_sigma`:

  The validated design parameters.

- `moments`:

  A list containing the first four ordinary moments, mean, variance,
  standard deviation, skewness, excess kurtosis, the constants `b1` and
  `b2`, and the design parameters.

- `cf_order`, `nsim`, `seed`:

  The requested numerical settings.

## Details

If \\S\\ is the usual covariance matrix from a subgroup of size \\n\\,
then \\(n-1)S\\ has a Wishart distribution and \\\|S\|\\ can be
represented as a scaled product of independent chi-square variables. The
function uses that representation to obtain moments, approximations,
exact cases, or simulated quantiles.

## Exact-method scope

The \\p=2\\ exact distribution is available for both upper and two-sided
charts. The \\p=3\\ implementation is deliberately limited to the
published one-sided upper quantiles described above. No generic Meijer-G
evaluator is claimed; use `type = "simulation"` outside those cases.

## Decision convention

A plotted subgroup signals when \\\|S\| \< LCL\\ or \\\|S\| \> UCL\\.
Equality to either limit is treated as in control.

## Errors

Errors are raised for invalid dimensions, \\n \<= p\\, a nonpositive
`det_sigma`, invalid `alpha`, unsupported exact cases, invalid
Cornish-Fisher order, fewer than 1000 simulations, or an invalid seed.

## References

Barbosa, E. P., Gneri, M. A., and Meneguetti, A. *Improving
Shewhart-type Generalized Variance Control Charts for Multivariate
Process Variability Monitoring using Cornish-Fisher Quantile Correction,
Meijer-G Function and Other Tools*. Research report.

Anderson, T. W. (1984). *An Introduction to Multivariate Statistical
Analysis*, 2nd ed. Wiley.

Cornish, E. A. and Fisher, R. A. (1960). The percentage points of
distributions having known cumulants. *Technometrics*, 2, 209–225.

## See also

[`gv_stat`](https://flaviobarros.github.io/IQCC/reference/gv_stat.md),
[`gv_alpha_risk`](https://flaviobarros.github.io/IQCC/reference/gv_alpha_risk.md),
[`cchart.GV`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md)

## Examples

``` r
# Published dimension-two illustration: |Sigma| = 0.5320, n = 10.
gv_limits(10, 2, det_sigma = 0.5320, type = "normal")$ucl
#> [1] 1.428685
gv_limits(10, 2, det_sigma = 0.5320, type = "cf")$ucl
#> [1] 2.160305
gv_limits(10, 2, det_sigma = 0.5320, type = "exact")$ucl
#> [1] 2.153629

# Equal-tail exact limits are also available in dimension two.
gv_limits(10, 2, side = "two.sided", type = "exact")
#> $lcl
#> [1] 0.0527836
#> 
#> $ucl
#> [1] 4.538591
#> 
#> $center
#> [1] 0.8888889
#> 
#> $type
#> [1] "exact"
#> 
#> $side
#> [1] "two.sided"
#> 
#> $alpha
#> [1] 0.0027
#> 
#> $n
#> [1] 10
#> 
#> $p
#> [1] 2
#> 
#> $det_sigma
#> [1] 1
#> 
#> $moments
#> $moments$ordinary
#> [1] 0.8888889 1.2071331 2.3248489 6.0273859
#> 
#> $moments$mean
#> [1] 0.8888889
#> 
#> $moments$variance
#> [1] 0.4170096
#> 
#> $moments$sd
#> [1] 0.6457628
#> 
#> $moments$skewness
#> [1] 1.895698
#> 
#> $moments$excess_kurtosis
#> [1] 6.264543
#> 
#> $moments$b1
#> [1] 0.8888889
#> 
#> $moments$b2
#> [1] 0.4170096
#> 
#> $moments$n
#> [1] 10
#> 
#> $moments$p
#> [1] 2
#> 
#> $moments$det_sigma
#> [1] 1
#> 
#> 
#> $cf_order
#> [1] 1
#> 
#> $nsim
#> [1] 1e+05
#> 
#> $seed
#> NULL
#> 

# \donttest{
# A reproducible simulation-based limit for higher dimension.
gv_limits(8, 3, type = "simulation", nsim = 5000, seed = 2026)
#> $lcl
#> [1] 0
#> 
#> $ucl
#> [1] 5.781372
#> 
#> $center
#> [1] 0.6122449
#> 
#> $type
#> [1] "simulation"
#> 
#> $side
#> [1] "upper"
#> 
#> $alpha
#> [1] 0.0027
#> 
#> $n
#> [1] 8
#> 
#> $p
#> [1] 3
#> 
#> $det_sigma
#> [1] 1
#> 
#> $moments
#> $moments$ordinary
#> [1]  0.6122449  0.8996252  2.5965857 12.9904989
#> 
#> $moments$mean
#> [1] 0.6122449
#> 
#> $moments$variance
#> [1] 0.5247813
#> 
#> $moments$sd
#> [1] 0.7244179
#> 
#> $moments$skewness
#> [1] 3.691082
#> 
#> $moments$excess_kurtosis
#> [1] 26.89629
#> 
#> $moments$b1
#> [1] 0.6122449
#> 
#> $moments$b2
#> [1] 0.5247813
#> 
#> $moments$n
#> [1] 8
#> 
#> $moments$p
#> [1] 3
#> 
#> $moments$det_sigma
#> [1] 1
#> 
#> 
#> $cf_order
#> [1] 1
#> 
#> $nsim
#> [1] 5000
#> 
#> $seed
#> [1] 2026
#> 
# }
```
