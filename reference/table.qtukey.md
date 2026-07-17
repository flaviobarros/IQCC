# Tukey Quantile Table for the Relative Range

Print a table of quantiles of the studentized range distribution \\W = R
/ \sigma\\ for sample sizes \\n = 2\\ through the specified maximum,
using [`qtukey`](https://rdrr.io/r/stats/Tukey.html) with infinite
denominator degrees of freedom.

## Usage

``` r
table.qtukey(alpha, n)
```

## Arguments

- alpha:

  Type-I error probability (\\0 \< \alpha \< 1\\).

- n:

  Maximum sample size (\\\ge 2\\).

## Value

Invisibly, a matrix with \\n - 1\\ rows and 4 columns. The matrix is
also printed to the console.

## Details

Four tail probabilities are tabulated for each \\n\\: \\\alpha/2\\,
\\\alpha\\, \\1 - \alpha\\, and \\1 - \alpha/2\\. The default examples
use \\\alpha = 0.0027\\ (US convention) and \\\alpha = 0.0020\\
(European convention).

## Errors

Stops if `n < 2` or if `alpha` is not strictly between 0 and 1.

## References

Barbosa, E. P., Gneri, M. A. and Meneguetti, A. (2013). On the the
evaluation of the relative range distribution. *Communications in
Statistics - Simulation and Computation*, 42, 1311–1339.
[doi:10.1080/03610918.2011.639967](https://doi.org/10.1080/03610918.2011.639967)
.

## See also

[`alpha.risk`](https://flaviobarros.github.io/IQCC/reference/alpha.risk.md),
[`cchart.R`](https://flaviobarros.github.io/IQCC/reference/cchart.R.md)

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
table.qtukey(0.0020, 10)
#>        alpha/2       alpha  1-alpha 1-alpha/2
#> 2  0.001772454 0.003544911 4.370248  4.653508
#> 3  0.060244727 0.085220362 4.798017  5.063453
#> 4  0.199446016 0.251655687 5.053191  5.308804
#> 5  0.367392008 0.438356130 5.234784  5.483754
#> 6  0.534736270 0.617474738 5.375312  5.619333
#> 7  0.691346609 0.781445264 5.489641  5.729754
#> 8  0.834825690 0.929575684 5.585821  5.822728
#> 9  0.965508168 1.063219041 5.668703  5.902906
#> 10 1.084582650 1.184171044 5.741432  5.973307
```
