# Probability Limits for p Charts

Compute normal or Cornish-Fisher probability limits for a p chart
without constructing a plot. The Cornish-Fisher formulas retain the
original proportion scale and correct the normal approximation using
binomial cumulants.

## Usage

``` r
pchart_limits(
  p,
  n,
  alpha = ALPHA,
  type = c("normal", "cf1", "cf2"),
  sides = c("two.sided", "upper"),
  truncate = TRUE
)
```

## Arguments

- p:

  In-control nonconforming proportion. A scalar strictly between zero
  and one.

- n:

  Positive integer sample size or vector of sample sizes.

- alpha:

  Nominal false alarm probability. Defaults to 0.0027.

- type:

  Limit method: `"normal"`, `"cf1"`, or `"cf2"`.

- sides:

  Either `"two.sided"` or `"upper"`.

- truncate:

  Logical. If `TRUE`, limits are restricted to \[0, 1\].

## Value

A list containing `center`, `lcl`, `ucl`, `n`, `alpha`, `type`, `sides`,
`npq`, and `applicable`.

## Details

`type = "cf1"` uses the first skewness correction proposed by
Winterbottom (1993). `type = "cf2"` uses the operational two-adjustment
limits in equation (8) of Joekes and Barbosa (2013). In particular, the
second adjustment is evaluated with the positive normal quantile and is
applied with the same sign to both limits. This convention reproduces
the published limits and false-alarm risks in Tables 2 and 3.

The article recommends the normal approximation for approximately \\n p
(1-p) \>= 5\\, CF1 for approximately \\n p (1-p) \>= 0.25\\, and CF2 for
approximately \\n p (1-p) \>= 0.08\\. The returned `applicable` field
reports whether each sample size meets the corresponding recommendation.

## References

Winterbottom, A. (1993). Simple adjustment to improve control limits on
attribute charts. *Quality and Reliability Engineering International*.

Joekes, S. and Barbosa, E. P. (2013). An improved attribute control
chart for monitoring non-conforming proportion in high quality
processes. *Control Engineering Practice*, 21, 407–412.
[doi:10.1016/j.conengprac.2012.12.005](https://doi.org/10.1016/j.conengprac.2012.12.005)
.

## Examples

``` r
pchart_limits(0.015, n = 20, type = "normal")
#> $center
#> [1] 0.015
#> 
#> $lcl
#> [1] 0
#> 
#> $ucl
#> [1] 0.09653924
#> 
#> $n
#> [1] 20
#> 
#> $alpha
#> [1] 0.0027
#> 
#> $type
#> [1] "normal"
#> 
#> $sides
#> [1] "two.sided"
#> 
#> $npq
#> [1] 0.2955
#> 
#> $applicable
#> [1] FALSE
#> 
pchart_limits(0.015, n = 20, type = "cf1")
#> $center
#> [1] 0.015
#> 
#> $lcl
#> [1] 0
#> 
#> $ucl
#> [1] 0.1612048
#> 
#> $n
#> [1] 20
#> 
#> $alpha
#> [1] 0.0027
#> 
#> $type
#> [1] "cf1"
#> 
#> $sides
#> [1] "two.sided"
#> 
#> $npq
#> [1] 0.2955
#> 
#> $applicable
#> [1] TRUE
#> 
pchart_limits(0.015, n = 20, type = "cf2")
#> $center
#> [1] 0.015
#> 
#> $lcl
#> [1] 0
#> 
#> $ucl
#> [1] 0.1303192
#> 
#> $n
#> [1] 20
#> 
#> $alpha
#> [1] 0.0027
#> 
#> $type
#> [1] "cf2"
#> 
#> $sides
#> [1] "two.sided"
#> 
#> $npq
#> [1] 0.2955
#> 
#> $applicable
#> [1] TRUE
#> 
```
