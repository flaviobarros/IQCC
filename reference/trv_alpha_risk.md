# False-Alarm Risk for Trace-Statistic Charts

Evaluate the actual in-control upper-tail probability of a supplied
`tr(V)` upper control limit.

## Usage

``` r
trv_alpha_risk(
  n,
  p,
  ucl,
  Sigma0 = NULL,
  type = c("chisq", "simulation"),
  nsim = 1e+05,
  seed = NULL
)
```

## Arguments

- n:

  Integer subgroup sample size, at least 2.

- p:

  Integer process dimension, at least 2.

- ucl:

  Positive finite upper control limit on the trace statistic.

- Sigma0:

  Optional in-control covariance matrix. It is accepted for API symmetry
  with
  [`trv_stat`](https://flaviobarros.github.io/IQCC/reference/trv_stat.md)
  and validated when supplied, but the null distribution of the
  standardized trace statistic depends only on `n` and `p`.

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

## Value

A list with components `alpha`, `arl0`, `ucl`, `n`, `p`, `df`, and
`method`.

## Examples

``` r
lim <- trv_limits(8, 2)
trv_alpha_risk(8, 2, lim$ucl)
#> $alpha
#> [1] 0.0027
#> 
#> $arl0
#> [1] 370.3704
#> 
#> $ucl
#> [1] 33.19518
#> 
#> $n
#> [1] 8
#> 
#> $p
#> [1] 2
#> 
#> $df
#> [1] 14
#> 
#> $method
#> [1] "exact chi-square"
#> 
```
