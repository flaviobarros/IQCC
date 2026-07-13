# False-Alarm Probability for the Three-Sigma R Chart

Compute the actual in-control false-alarm probability of the
conventional three-sigma range chart under the exact distribution of the
relative range statistic \\W = R / \sigma\\.

## Usage

``` r
alpha.risk(n)
```

## Arguments

- n:

  Numeric vector of integer subgroup sizes, each at least 2.

## Value

A numeric vector with the same length and order as `n`, containing the
exact in-control false-alarm probability of the conventional three-sigma
R chart.

## Details

The conventional limits use \\D_1=\max\\0,d_2(n)-3d_3(n)\\\\ and
\\D_2=d_2(n)+3d_3(n)\\. The returned risk is
\\1-\\F_W(D_2)-F_W(D_1)\\\\, evaluated with
[`stats::ptukey()`](https://rdrr.io/r/stats/Tukey.html). For small
subgroup sizes this risk can be substantially larger than 0.0027.

## Errors

An error is raised when any subgroup size is smaller than 2.

## References

Barbosa, E. P., Gneri, M. A., and Meneguetti, A. (2013). Range control
charts revisited: Simpler Tippett-like formulae, its practical
implementation, and the study of false alarm. *Communications in
Statistics - Simulation and Computation*, 42(2), 247–262.
[doi:10.1080/03610918.2011.639967](https://doi.org/10.1080/03610918.2011.639967)
.

## See also

[`cchart.R`](https://flaviobarros.github.io/IQCC/reference/cchart.R.md),
[`d2`](https://flaviobarros.github.io/IQCC/reference/d2.md),
[`d3`](https://flaviobarros.github.io/IQCC/reference/d3.md),
[`table.qtukey`](https://flaviobarros.github.io/IQCC/reference/table.qtukey.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r
alpha.risk(5)
#> [1] 0.004603048
alpha.risk(c(3, 5, 10, 15))
#> [1] 0.005842954 0.004603048 0.004367441 0.004493839
```
