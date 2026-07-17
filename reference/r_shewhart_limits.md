# Conventional Three-Sigma Limits for the R Chart

Compute the conventional three-sigma (Shewhart) control limits for the R
chart without constructing a plot. The limits are based on the constants
\\d_2(n)\\ and \\d_3(n)\\, the mean and standard deviation of the
relative range \\W = R / \sigma\\ under normality.

## Usage

``` r
r_shewhart_limits(sigma, n, nsigmas = SIGMA_MULT)
```

## Arguments

- sigma:

  Positive finite scalar. The in-control process standard deviation.
  When estimated from Phase I data, the returned limits are plug-in
  estimates.

- n:

  Integer subgroup size, at least 2.

- nsigmas:

  Positive numeric scalar. The number of sigma units for the control
  limits. Defaults to 3.

## Value

A named list with components:

- lcl:

  Lower control limit (zero-truncated).

- ucl:

  Upper control limit.

- center:

  \\d_2(n) \sigma\\, the expected value of the range under normality.

- nsigmas:

  The supplied multiplier.

- sigma:

  The supplied `sigma`.

- n:

  The supplied subgroup size.

## Details

The conventional limits are \$\$LCL = \max\\0, d_2(n) - k \\ d_3(n)\\ \\
\sigma\$\$ and \$\$UCL = \\d_2(n) + k \\ d_3(n)\\ \\ \sigma,\$\$ where
\\k\\ is the number of sigma units (default 3). Because the distribution
of the relative range is not normal, the actual false-alarm probability
of these limits can be substantially larger than the nominal 0.0027
associated with three-sigma limits for a normal variate. Use
[`alpha.risk`](https://flaviobarros.github.io/IQCC/reference/alpha.risk.md)
to evaluate the exact false-alarm probability.

## Phase convention

These limits are computed for a known or separately estimated `sigma`.
When `sigma` is estimated from a Phase I reference sample, the limits
are plug-in limits.

## Decision rule

A subgroup range \\R\\ signals out of control when \\R \< LCL\\ or \\R
\> UCL\\. Equality to a limit is treated as in control.

## Errors

An error is raised when `sigma` is `NA`, `NaN`, or non-positive; when
`n` is smaller than 2, non-integer, or non-finite; or when `nsigmas` is
non-positive.

## References

Montgomery, D. C. (2009). *Introduction to Statistical Quality Control*,
6th ed. Wiley.

Barbosa, E. P., Gneri, M. A., and Meneguetti, A. (2013). Range control
charts revisited: Simpler Tippett-like formulae, its practical
implementation, and the study of false alarm. *Communications in
Statistics - Simulation and Computation*, 42(2), 247–262.
[doi:10.1080/03610918.2011.639967](https://doi.org/10.1080/03610918.2011.639967)
.

## See also

[`r_exact_limits`](https://flaviobarros.github.io/IQCC/reference/r_exact_limits.md),
[`cchart.R`](https://flaviobarros.github.io/IQCC/reference/cchart.R.md),
[`alpha.risk`](https://flaviobarros.github.io/IQCC/reference/alpha.risk.md),
[`d2`](https://flaviobarros.github.io/IQCC/reference/d2.md),
[`d3`](https://flaviobarros.github.io/IQCC/reference/d3.md)

## Examples

``` r
# Conventional three-sigma limits for n = 5, sigma = 2
r_shewhart_limits(sigma = 2, n = 5)
#> $lcl
#> [1] 0
#> 
#> $ucl
#> [1] 9.83635
#> 
#> $center
#> [1] 4.651858
#> 
#> $nsigmas
#> [1] 3
#> 
#> $sigma
#> [1] 2
#> 
#> $n
#> [1] 5
#> 

# Custom multiplier
r_shewhart_limits(sigma = 1, n = 10, nsigmas = 2)
#> $lcl
#> [1] 1.483404
#> 
#> $ucl
#> [1] 4.671607
#> 
#> $center
#> [1] 3.077505
#> 
#> $nsigmas
#> [1] 2
#> 
#> $sigma
#> [1] 1
#> 
#> $n
#> [1] 10
#> 
```
