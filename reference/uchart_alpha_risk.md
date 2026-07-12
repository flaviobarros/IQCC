# Exact False-Alarm Risk for u-Chart Limits

Compute the actual false-alarm probability under \\X \sim Poisson(lambda
n)\\, with a signal when \\X/n \< LCL\\ or \\X/n \> UCL\\.

## Usage

``` r
uchart_alpha_risk(lambda, n, lcl, ucl)
```

## Arguments

- lambda:

  In-control defect rate per inspection unit.

- n:

  Positive inspection size or vector of inspection sizes.

- lcl:

  Lower control limit or vector of limits.

- ucl:

  Upper control limit or vector of limits.

## Value

A numeric vector of exact false-alarm probabilities.

## Examples

``` r
limits <- uchart_limits(1.4, 10, type = "cf2")
uchart_alpha_risk(1.4, 10, limits$lcl, limits$ucl)
#> [1] 0.003113901
```
