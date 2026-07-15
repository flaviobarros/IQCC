# Table of values for the constants d2, d3 and c4.

This function is used to build a table of values for the constants d2,
d3 and c4 for successive values of sample size n.

## Usage

``` r
table.const(n)
```

## Arguments

- n:

  The maximum size.

## Value

Return the values of these three constants.

## Details

It builds a table in matrix form with 3 columns (one for each constant)
and one row for each value of n from 2 to a specified value.

## See also

[d2](https://flaviobarros.github.io/IQCC/reference/d2.md),[d3](https://flaviobarros.github.io/IQCC/reference/d3.md),[c4](https://flaviobarros.github.io/IQCC/reference/c4.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

table.const(17)
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
#> 11 3.172873 0.7873146 0.9753501
#> 12 3.258455 0.7784783 0.9775594
#> 13 3.335980 0.7704162 0.9794056
#> 14 3.406763 0.7630231 0.9809714
#> 15 3.471827 0.7562114 0.9823162
#> 16 3.531983 0.7499081 0.9834835
#> 17 3.587884 0.7440518 0.9845064
```
