# Tukey Quantile Table

Builds a table with quantiles of the sample relative range distribution.

## Usage

``` r
table.qtukey(alpha, n)
```

## Arguments

- alpha:

  The probability of type-I error of false alarm , that is equal to 1
  minus the confidence level.

- n:

  The maximum sample size.

## Value

Returns a matrix with 4 columns containing the quantiles, printed to the
console and returned invisibly.

## See also

[table.const](https://flaviobarros.github.io/IQCC/reference/table.const.md),[alpha.risk](https://flaviobarros.github.io/IQCC/reference/alpha.risk.md),[qtukey](https://rdrr.io/r/stats/Tukey.html)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

table.qtukey(0.0027, 15)
#>        alpha/2       alpha  1-alpha 1-alpha/2
#> 2  0.002392814 0.004785635 4.242608  4.532743
#> 3  0.070003631 0.099034660 4.678703  4.950175
#> 4  0.220551642 0.278377858 4.938456  5.199657
#> 5  0.396528087 0.473383768 5.123140  5.377402
#> 6  0.568995827 0.657506350 5.265970  5.515065
#> 7  0.728847025 0.824508326 5.382113  5.627133
#> 8  0.874395939 0.974498343 5.479780  5.721458
#> 9  1.006411220 1.109286010 5.563915  5.802775
#> 10 1.126343057 1.230931805 5.637724  5.874158
#> 11 1.235706831 1.341319304 5.703400  5.937711
#> 12 1.335882897 1.442058423 5.762513  5.994941
#> 13 1.428067918 1.534493165 5.816219  6.046960
#> 14 1.513279523 1.619738053 5.865398  6.094613
#> 15 1.592376820 1.698718124 5.910733  6.138556
```
