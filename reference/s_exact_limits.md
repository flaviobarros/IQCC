# Exact Probability Limits for the S Chart

Compute exact probability control limits for subgroup standard
deviations without constructing a plot. Under normal sampling,
\$\$(n-1)S^2/\sigma^2 \sim \chi^2\_{n-1}.\$\$ For a two-sided chart, the
nominal false-alarm probability is divided equally between the two
tails. For an upper chart, all of `alpha` is assigned to the upper tail
and the lower limit is zero.

## Usage

``` r
s_exact_limits(sigma, n, alpha = ALPHA, side = c("two.sided", "upper"))
```

## Arguments

- sigma:

  Positive finite scalar. The in-control process standard deviation.
  When estimated from Phase I data, the returned limits are plug-in
  limits and do not incorporate Phase I estimation uncertainty.

- n:

  Integer subgroup size(s), each at least 2.

- alpha:

  Nominal false-alarm probability per subgroup, strictly between 0
  and 1. Defaults to 0.0027.

- side:

  Either `"two.sided"` or `"upper"`.

## Value

A named list with components `lcl`, `ucl`, `center`, `sigma`, `n`,
`alpha`, `side`, and `method`. The theoretical center is
\\c_4(n)\sigma\\.

## Phase convention

These functions take `sigma` as known. If a Phase I estimate is supplied
instead, the resulting limits are plug-in limits. The historical
[`cchart.S()`](https://flaviobarros.github.io/IQCC/reference/cchart.S.md)
wrapper estimates `sigma` from the data using
[`qcc::sd.S()`](https://rdrr.io/pkg/qcc/man/stats.S.html) before calling
this function for exact limits.

## Decision rule

A subgroup standard deviation signals out of control when it is below
`lcl` or above `ucl`. Equality to a limit is treated as in control.

## References

Montgomery, D. C. (2009). *Introduction to Statistical Quality Control*,
6th ed. Wiley.

## See also

[`s_shewhart_limits`](https://flaviobarros.github.io/IQCC/reference/s_shewhart_limits.md),
[`cchart.S`](https://flaviobarros.github.io/IQCC/reference/cchart.S.md),
[`c4`](https://flaviobarros.github.io/IQCC/reference/c4.md)

## Examples

``` r
s_exact_limits(sigma = 2, n = 5)
#> $lcl
#> [1] 0.3252186
#> 
#> $ucl
#> [1] 4.219054
#> 
#> $center
#> [1] 1.879971
#> 
#> $sigma
#> [1] 2
#> 
#> $n
#> [1] 5
#> 
#> $alpha
#> [1] 0.0027
#> 
#> $side
#> [1] "two.sided"
#> 
#> $method
#> [1] "exact"
#> 
s_exact_limits(sigma = 1, n = 2:6, side = "upper")
#> $lcl
#> [1] 0 0 0 0 0
#> 
#> $ucl
#> [1] 2.999977 2.431975 2.172269 2.015637 1.908148
#> 
#> $center
#> [1] 0.7978846 0.8862269 0.9213177 0.9399856 0.9515329
#> 
#> $sigma
#> [1] 1
#> 
#> $n
#> [1] 2 3 4 5 6
#> 
#> $alpha
#> [1] 0.0027
#> 
#> $side
#> [1] "upper"
#> 
#> $method
#> [1] "exact"
#> 
```
