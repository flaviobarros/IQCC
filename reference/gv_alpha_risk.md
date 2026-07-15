# False-Alarm Risk for Generalized Variance Charts

Evaluate the actual in-control probability that the generalized variance
statistic crosses limits produced by
[`gv_limits()`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md),
and report the corresponding zero-state in-control average run length.

## Usage

``` r
gv_alpha_risk(
  n,
  p,
  det_sigma = 1,
  alpha = 0.0027,
  type = c("normal", "cf", "exact", "simulation"),
  side = c("upper", "two.sided"),
  cf_order = 1,
  nsim = 2e+05,
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

- `alpha`:

  Actual or Monte Carlo estimated false-alarm probability.

- `nominal_alpha`:

  The requested nominal value.

- `arl0`:

  Zero-state in-control average run length, `1 / alpha`; `Inf` if no
  simulated crossing is observed.

- `limits`:

  The complete object returned by
  [`gv_limits()`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md).

- `method`:

  Either `"exact evaluation"` for \\p=2\\ or `"simulation"` for higher
  dimensions.

## Details

The diagnostic is useful because Gaussian and Cornish-Fisher limits are
approximations and therefore need not attain the requested nominal risk
exactly. For dimension \\p=2\\, the crossing probability is evaluated
from the exact chi-square representation of \\\|S\|\\. For \\p\>2\\, it
is estimated independently by Monte Carlo simulation from Bartlett's
product-of-chi-squares representation.

The function first calls
[`gv_limits()`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md)
using the requested `type`. It then evaluates \$\$P(\|S\| \> UCL)\$\$
for an upper chart, or \$\$P(\|S\| \< LCL) + P(\|S\| \> UCL)\$\$ for a
two-sided chart. The inequalities are strict, matching the signaling
rule used by
[`cchart.GV()`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md).

For \\p\>2\\, `nsim` controls both any simulation used to construct the
limits and the independent simulation used to evaluate their risk. If
`seed` is supplied, the limits use that seed and the risk evaluation
uses `seed + 1`; each seeded simulation restores the caller's previous
random-number state on exit. With `seed = NULL`, the global RNG state
advances normally.

## Monte Carlo uncertainty

For \\p\>2\\, the reported risk is a binomial proportion based on `nsim`
draws. Its approximate Monte Carlo standard error is
\\\sqrt{\hat\alpha(1-\hat\alpha)/nsim}\\. Very small false-alarm risks
therefore require substantially more simulations than routine examples.

## Errors

Input and exact-method restrictions are inherited from
[`gv_limits()`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md).
In addition, simulation requires an integer `nsim` of at least 1000 and
a valid optional seed.

## References

Barbosa, E. P., Gneri, M. A., and Meneguetti, A. *Improving
Shewhart-type Generalized Variance Control Charts for Multivariate
Process Variability Monitoring using Cornish-Fisher Quantile Correction,
Meijer-G Function and Other Tools*. Research report.

## See also

[`gv_limits`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md),
[`gv_stat`](https://flaviobarros.github.io/IQCC/reference/gv_stat.md),
[`cchart.GV`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md)

## Examples

``` r
# Exact evaluation shows that exact p = 2 limits attain the nominal risk.
exact_risk <- gv_alpha_risk(10, 2, type = "exact")
exact_risk$alpha
#> [1] 0.0027
exact_risk$arl0
#> [1] 370.3704

# Diagnose the false-alarm inflation of moment-matched normal limits.
gv_alpha_risk(10, 2, type = "normal")
#> $alpha
#> [1] 0.02078953
#> 
#> $nominal_alpha
#> [1] 0.0027
#> 
#> $arl0
#> [1] 48.10115
#> 
#> $limits
#> $limits$lcl
#> [1] 0
#> 
#> $limits$ucl
#> [1] 2.685498
#> 
#> $limits$center
#> [1] 0.8888889
#> 
#> $limits$type
#> [1] "normal"
#> 
#> $limits$side
#> [1] "upper"
#> 
#> $limits$alpha
#> [1] 0.0027
#> 
#> $limits$n
#> [1] 10
#> 
#> $limits$p
#> [1] 2
#> 
#> $limits$det_sigma
#> [1] 1
#> 
#> $limits$moments
#> $limits$moments$ordinary
#> [1] 0.8888889 1.2071331 2.3248489 6.0273859
#> 
#> $limits$moments$mean
#> [1] 0.8888889
#> 
#> $limits$moments$variance
#> [1] 0.4170096
#> 
#> $limits$moments$sd
#> [1] 0.6457628
#> 
#> $limits$moments$skewness
#> [1] 1.895698
#> 
#> $limits$moments$excess_kurtosis
#> [1] 6.264543
#> 
#> $limits$moments$b1
#> [1] 0.8888889
#> 
#> $limits$moments$b2
#> [1] 0.4170096
#> 
#> $limits$moments$n
#> [1] 10
#> 
#> $limits$moments$p
#> [1] 2
#> 
#> $limits$moments$det_sigma
#> [1] 1
#> 
#> 
#> $limits$cf_order
#> [1] 1
#> 
#> $limits$nsim
#> [1] 2e+05
#> 
#> $limits$seed
#> NULL
#> 
#> 
#> $method
#> [1] "exact evaluation"
#> 

# \donttest{
# Reproducible risk evaluation for a higher-dimensional chart.
gv_alpha_risk(8, 3, type = "cf", nsim = 10000, seed = 2026)
#> $alpha
#> [1] 0.0017
#> 
#> $nominal_alpha
#> [1] 0.0027
#> 
#> $arl0
#> [1] 588.2353
#> 
#> $limits
#> $limits$lcl
#> [1] 0
#> 
#> $limits$ucl
#> [1] 5.631511
#> 
#> $limits$center
#> [1] 0.6122449
#> 
#> $limits$type
#> [1] "cf"
#> 
#> $limits$side
#> [1] "upper"
#> 
#> $limits$alpha
#> [1] 0.0027
#> 
#> $limits$n
#> [1] 8
#> 
#> $limits$p
#> [1] 3
#> 
#> $limits$det_sigma
#> [1] 1
#> 
#> $limits$moments
#> $limits$moments$ordinary
#> [1]  0.6122449  0.8996252  2.5965857 12.9904989
#> 
#> $limits$moments$mean
#> [1] 0.6122449
#> 
#> $limits$moments$variance
#> [1] 0.5247813
#> 
#> $limits$moments$sd
#> [1] 0.7244179
#> 
#> $limits$moments$skewness
#> [1] 3.691082
#> 
#> $limits$moments$excess_kurtosis
#> [1] 26.89629
#> 
#> $limits$moments$b1
#> [1] 0.6122449
#> 
#> $limits$moments$b2
#> [1] 0.5247813
#> 
#> $limits$moments$n
#> [1] 8
#> 
#> $limits$moments$p
#> [1] 3
#> 
#> $limits$moments$det_sigma
#> [1] 1
#> 
#> 
#> $limits$cf_order
#> [1] 1
#> 
#> $limits$nsim
#> [1] 10000
#> 
#> $limits$seed
#> [1] 2026
#> 
#> 
#> $method
#> [1] "simulation"
#> 
# }
```
