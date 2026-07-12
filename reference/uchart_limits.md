# Probability Limits for u Charts

Compute normal or Cornish-Fisher probability limits for a u chart
without constructing a plot. The model is \\X \sim Poisson(lambda n)\\
and the monitored rate is \\U = X/n\\.

## Usage

``` r
uchart_limits(lambda, n, alpha = ALPHA,
  type = c("normal", "cf1", "cf2"),
  sides = c("two.sided", "upper"), truncate = TRUE)
```

## Arguments

- lambda:

  In-control defect rate per inspection unit. A positive scalar.

- n:

  Positive inspection size or vector of inspection sizes.

- alpha:

  Nominal false-alarm probability. Defaults to 0.0027.

- type:

  Limit method: `"normal"`, `"cf1"`, or `"cf2"`.

- sides:

  Either `"two.sided"` or `"upper"`.

- truncate:

  Logical. If `TRUE`, negative lower limits are set to 0.

## Value

A list containing `center`, `lcl`, `ucl`, `n`, `alpha`, `type`, `sides`,
and `mean_count`.

## Details

For \\U\\, the standardized third and fourth cumulants are \\gamma_1 =
1/sqrt(lambda n)\\ and \\gamma_2 = 1/(lambda n)\\. Consequently, the
first Cornish-Fisher correction is \\(z^2 - 1)/(6n)\\ and the
second-order correction reduces to \\z(1-z^2)/(72 n sqrt(lambda n))\\.
With \\z=3\\, the CF2 upper limit recovers the historical IQCC formula.

## Examples

``` r
uchart_limits(1.4, 10, type = "normal")
#> $center
#> [1] 1.4
#> 
#> $lcl
#> [1] 0.2775114
#> 
#> $ucl
#> [1] 2.522489
#> 
#> $n
#> [1] 10
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
#> $mean_count
#> [1] 14
#> 
uchart_limits(1.4, 10, type = "cf1")
#> $center
#> [1] 1.4
#> 
#> $lcl
#> [1] 0.4108424
#> 
#> $ucl
#> [1] 2.65582
#> 
#> $n
#> [1] 10
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
#> $mean_count
#> [1] 14
#> 
uchart_limits(1.4, 10, type = "cf2")
#> $center
#> [1] 1.4
#> 
#> $lcl
#> [1] 0.4019339
#> 
#> $ucl
#> [1] 2.646911
#> 
#> $n
#> [1] 10
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
#> $mean_count
#> [1] 14
#> 
```
