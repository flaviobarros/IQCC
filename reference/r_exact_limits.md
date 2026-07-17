# Exact Probability Limits for the R Chart

Compute exact equal-tail probability control limits for the R chart
without constructing a plot. The limits are based on the distribution of
the relative range \\W = R / \sigma\\ under normality, using the
Studentized range distribution implemented by
[`stats::qtukey()`](https://rdrr.io/r/stats/Tukey.html).

## Usage

``` r
r_exact_limits(sigma, n, alpha = ALPHA)
```

## Arguments

- sigma:

  Positive finite scalar. The in-control process standard deviation.
  When `sigma` is estimated from Phase I data, the returned limits are
  plug-in estimates and do not incorporate the additional uncertainty of
  Phase I estimation.

- n:

  Integer subgroup size, at least 2.

- alpha:

  Nominal false-alarm probability per subgroup. Defaults to 0.0027. One
  half is placed in each tail.

## Value

A named list with components:

- lcl:

  Lower control limit.

- ucl:

  Upper control limit.

- center:

  \\d_2(n) \sigma\\, the expected value of the range under normality.

- sigma:

  The supplied `sigma`.

- n:

  The supplied subgroup size.

- alpha:

  The supplied nominal false-alarm probability.

## Details

The exact limits are \$\$LCL = \sigma \\ F_W^{-1}(\alpha/2; n)\$\$ and
\$\$UCL = \sigma \\ F_W^{-1}(1 - \alpha/2; n),\$\$ where \\F_W^{-1}\\ is
the quantile function of \\W = R / \sigma\\.

## Phase convention

These limits are computed for a known or separately estimated `sigma`.
When `sigma` is estimated from a Phase I reference sample, the limits
are plug-in limits and do not account for Phase I sampling variability.

## Decision rule

A subgroup range \\R\\ signals out of control when \\R \< LCL\\ or \\R
\> UCL\\. Equality to a limit is treated as in control.

## Errors

An error is raised when `sigma` is `NA`, `NaN`, or non-positive; when
`n` is smaller than 2, non-integer, or non-finite; or when `alpha` is
not between 0 and 1.

## References

Barbosa, E. P., Gneri, M. A., and Meneguetti, A. (2013). Range control
charts revisited: Simpler Tippett-like formulae, its practical
implementation, and the study of false alarm. *Communications in
Statistics - Simulation and Computation*, 42(2), 247–262.
[doi:10.1080/03610918.2011.639967](https://doi.org/10.1080/03610918.2011.639967)
.

## See also

[`r_shewhart_limits`](https://flaviobarros.github.io/IQCC/reference/r_shewhart_limits.md),
[`cchart.R`](https://flaviobarros.github.io/IQCC/reference/cchart.R.md),
[`alpha.risk`](https://flaviobarros.github.io/IQCC/reference/alpha.risk.md),
[`d2`](https://flaviobarros.github.io/IQCC/reference/d2.md)

## Examples

``` r
# Known-sigma limits for subgroup size n = 5, sigma = 2, alpha = 0.0027
r_exact_limits(sigma = 2, n = 5)
#> $lcl
#> [1] 0.7930562
#> 
#> $ucl
#> [1] 10.7548
#> 
#> $center
#> [1] 4.651858
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

# Custom alpha
r_exact_limits(sigma = 1, n = 10, alpha = 0.01)
#> $lcl
#> [1] 1.334927
#> 
#> $ucl
#> [1] 5.417616
#> 
#> $center
#> [1] 3.077505
#> 
#> $sigma
#> [1] 1
#> 
#> $n
#> [1] 10
#> 
#> $alpha
#> [1] 0.01
#> 
```
