# Conventional Shewhart Limits for the S Chart

Compute conventional Shewhart limits for subgroup standard deviations
without constructing a plot. Under normality, \\E(S)=c_4(n)\sigma\\ and
\\SD(S)=\sigma\sqrt{1-c_4(n)^2}\\. With the default theoretical center,
the limits are therefore
\$\$LCL=\max\\0,c_4(n)\sigma-k\sigma\sqrt{1-c_4(n)^2}\\\$\$ and
\$\$UCL=c_4(n)\sigma+k\sigma\sqrt{1-c_4(n)^2},\$\$ where \\k\\ is
`nsigmas`.

## Usage

``` r
s_shewhart_limits(
  sigma,
  n,
  nsigmas = SIGMA_MULT,
  side = c("two.sided", "upper"),
  center = NULL
)
```

## Arguments

- sigma:

  Positive finite scalar. The in-control process standard deviation,
  known or estimated separately.

- n:

  Integer subgroup size(s), each at least 2.

- nsigmas:

  Positive finite scalar giving the number of standard-error units used
  for the limits. Defaults to 3.

- side:

  Either `"two.sided"` or `"upper"`. For an upper chart the lower limit
  is zero.

- center:

  Optional finite numeric center. It may be a scalar or have the same
  length as `n`. If omitted, \\c_4(n)\sigma\\ is used.

## Value

A named list with components `lcl`, `ucl`, `center`, `sigma`, `n`,
`nsigmas`, `side`, and `method`.

## Details

The optional `center` argument exists to reproduce the historical `qcc`
S-chart convention, which centers the limits on the observed weighted
mean of subgroup standard deviations while estimating `sigma`
separately. When `center = NULL`, the theoretical center
\\c_4(n)\sigma\\ is used.

## Legacy qcc convention

The current
[`qcc::limits.S()`](https://rdrr.io/pkg/qcc/man/stats.S.html)
implementation computes `std.dev * sqrt(1 - c4(sizes)^2)` as the
standard error of the plotted S statistic and centers conventional
limits on the chart center supplied by `qcc`. Consequently,
`cchart.S(type = "n")` passes its Phase I weighted S center explicitly
to this function. This preserves the historical IQCC/qcc numerical
behavior rather than silently replacing the sample center by
\\c_4(n)\hat\sigma\\.

## References

Montgomery, D. C. (2009). *Introduction to Statistical Quality Control*,
6th ed. Wiley.

Scrucca, L. *qcc: Quality Control Charts*. R package.

## See also

[`s_exact_limits`](https://flaviobarros.github.io/IQCC/reference/s_exact_limits.md),
[`cchart.S`](https://flaviobarros.github.io/IQCC/reference/cchart.S.md),
[`c4`](https://flaviobarros.github.io/IQCC/reference/c4.md)

## Examples

``` r
s_shewhart_limits(sigma = 2, n = 5)
#> $lcl
#> [1] 0
#> 
#> $ucl
#> [1] 3.927256
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
#> $nsigmas
#> [1] 3
#> 
#> $side
#> [1] "two.sided"
#> 
#> $method
#> [1] "shewhart"
#> 
s_shewhart_limits(sigma = 2, n = 5, center = 1.9)
#> $lcl
#> [1] 0
#> 
#> $ucl
#> [1] 3.947285
#> 
#> $center
#> [1] 1.9
#> 
#> $sigma
#> [1] 2
#> 
#> $n
#> [1] 5
#> 
#> $nsigmas
#> [1] 3
#> 
#> $side
#> [1] "two.sided"
#> 
#> $method
#> [1] "shewhart"
#> 
```
