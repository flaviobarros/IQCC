# Trace-Statistic Control Limits

Compute one-sided upper control limits for the auxiliary trace statistic
\\T = (n - 1)\operatorname{tr}(\Sigma_0^{-1}S)\\.

## Usage

``` r
trv_limits(
  n,
  p,
  alpha = 0.0027,
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

## Value

A list with components `lcl`, `ucl`, `center`, `type`, `alpha`, `n`,
`p`, `df`, `nsim`, and `seed`. The lower control limit is zero because
the chart is one-sided upper.

## Details

Table 3 of Barbosa, Gneri, and Meneguetti uses \\N\\ as the row label,
although its numerical values correspond to subgroup size \\n\\ and
\\p(n - 1)\\ degrees of freedom. Its printed probability headings also
appear reversed: the column headed 0.9980 matches the 0.9973 quantile
and vice versa. This function follows the mathematical definition and
evaluates `qchisq(1 - alpha, df = p * (n - 1))`; it does not interchange
tail probabilities to reproduce the apparent table-label error.

## Examples

``` r
trv_limits(n = 8, p = 2)
#> $lcl
#> [1] 0
#> 
#> $ucl
#> [1] 33.19518
#> 
#> $center
#> [1] 14
#> 
#> $type
#> [1] "chisq"
#> 
#> $alpha
#> [1] 0.0027
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
#> $nsim
#> [1] 1e+05
#> 
#> $seed
#> NULL
#> 
trv_limits(n = 8, p = 2, type = "simulation", nsim = 5000, seed = 2026)
#> $lcl
#> [1] 0
#> 
#> $ucl
#> [1] 33.97193
#> 
#> $center
#> [1] 14
#> 
#> $type
#> [1] "simulation"
#> 
#> $alpha
#> [1] 0.0027
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
#> $nsim
#> [1] 5000
#> 
#> $seed
#> [1] 2026
#> 
```
