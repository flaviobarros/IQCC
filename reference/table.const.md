# Table of Control Chart Constants d2, d3, and c4

Build a matrix with the constants \\d_2\\, \\d_3\\, and \\c_4\\ for
sample sizes \\n = 2, 3, \ldots,\\ the specified maximum.

## Usage

``` r
table.const(n)
```

## Arguments

- n:

  Maximum sample size (must be \\\ge 2\\). The table includes rows for
  \\n = 2, 3, \ldots, n\\.

## Value

A matrix with \\n - 1\\ rows and 3 columns named `"d2"`, `"d3"`, `"c4"`.
Row names are the sample sizes.

## Details

The three constants are those returned by
[`d2`](https://flaviobarros.github.io/IQCC/reference/d2.md),
[`d3`](https://flaviobarros.github.io/IQCC/reference/d3.md), and
[`c4`](https://flaviobarros.github.io/IQCC/reference/c4.md)
respectively. They are the standard Shewhart chart constants for the
relative range and the standard deviation under normality.

## Errors

Stops if `n < 2`, raised by the internal
[`d2`](https://flaviobarros.github.io/IQCC/reference/d2.md) call.

## References

Montgomery, D. C. (2009). *Introduction to Statistical Quality Control*,
6th ed. Wiley.

## See also

[`d2`](https://flaviobarros.github.io/IQCC/reference/d2.md),
[`d3`](https://flaviobarros.github.io/IQCC/reference/d3.md),
[`c4`](https://flaviobarros.github.io/IQCC/reference/c4.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

table.const(10)
#>          d2        d3        c4
#> 2  1.128379 0.8525025 0.7978846
#> 3  1.692569 0.8883680 0.8862269
#> 4  2.058751 0.8798082 0.9213177
#> 5  2.325929 0.8640819 0.9399856
#> 6  2.534413 0.8480397 0.9515329
#> 7  2.704357 0.8332053 0.9593688
#> 8  2.847201 0.8198311 0.9650305
#> 9  2.970026 0.8078343 0.9693107
#> 10 3.077505 0.7970507 0.9726593
table.const(5)
#>         d2        d3        c4
#> 2 1.128379 0.8525025 0.7978846
#> 3 1.692569 0.8883680 0.8862269
#> 4 2.058751 0.8798082 0.9213177
#> 5 2.325929 0.8640819 0.9399856
```
