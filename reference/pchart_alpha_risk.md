# Exact False-Alarm Risk for p-Chart Limits

Compute the actual false-alarm probability of p-chart limits under the
in-control binomial model. The calculation respects the discreteness of
\\X \sim Binomial(n, p)\\ and treats a signal as \\X/n \< LCL\\ or \\X/n
\> UCL\\.

## Usage

``` r
pchart_alpha_risk(p, n, lcl, ucl)
```

## Arguments

- p:

  In-control nonconforming proportion.

- n:

  Positive integer sample size or vector of sample sizes.

- lcl:

  Lower control limit or vector of limits.

- ucl:

  Upper control limit or vector of limits.

## Value

A numeric vector with the exact false-alarm probability for each sample
size and pair of limits.

## Examples

``` r
limits <- pchart_limits(0.015, 20, type = "cf2")
pchart_alpha_risk(0.015, 20, limits$lcl, limits$ucl)
#> [1] 0.003178083
```
