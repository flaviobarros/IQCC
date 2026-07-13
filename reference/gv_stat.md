# Generalized Variance by Subgroup

Compute the generalized variance, defined as the determinant of the
unbiased sample covariance matrix, for each multivariate subgroup.

## Usage

``` r
gv_stat(x, size = NULL)
```

## Arguments

- x:

  Multivariate subgroup data in one of three forms:

  - a non-empty list whose elements are \\n \times p\\ numeric matrices;

  - a numeric three-dimensional array with dimensions `subgroup` by
    `observation` by `variable`; or

  - a numeric matrix formed by stacking equal-sized subgroups in
    consecutive blocks of rows.

  Every observation must be finite.

- size:

  Positive integer subgroup size. It is required when `x` is a stacked
  matrix and ignored for list or array input. The number of matrix rows
  must be an exact multiple of `size`.

## Value

A numeric vector of length equal to the number of subgroups. Element
\\i\\ is \\\|S_i\|\\. A determinant that is slightly negative only
because of floating-point roundoff is returned as zero.

## Details

For subgroup \\i\\ with \\n\\ observations on \\p\\ variables,
`gv_stat()` calculates \$\$\|S_i\|, \qquad S_i =
\frac{1}{n-1}\sum\_{j=1}^{n}(x\_{ij}-\bar{x}\_i)(x\_{ij}-\bar{x}\_i)^T.\$\$
All subgroups must have the same \\n\\ and \\p\\, with \\p \>= 2\\ and
\\n \> p\\. The latter condition permits a nonsingular sample covariance
matrix under nondegenerate data, although an observed subgroup may still
have determinant zero because of exact linear dependence.

## Errors

An error is raised for unsupported input types, empty or unequal-sized
subgroups, non-finite observations, fewer than two variables, \\n \<=
p\\, an invalid matrix `size`, or a covariance determinant that is
genuinely negative.

## References

Barbosa, E. P., Gneri, M. A., and Meneguetti, A. *Improving
Shewhart-type Generalized Variance Control Charts for Multivariate
Process Variability Monitoring using Cornish-Fisher Quantile Correction,
Meijer-G Function and Other Tools*. Research report.

Anderson, T. W. (1984). *An Introduction to Multivariate Statistical
Analysis*, 2nd ed. Wiley.

## See also

[`gv_limits`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md),
[`gv_alpha_risk`](https://flaviobarros.github.io/IQCC/reference/gv_alpha_risk.md),
[`cchart.GV`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md)

## Examples

``` r
g1 <- cbind(c(0, 1, 2, 3), c(0, 2, 1, 3))
g2 <- cbind(c(1, 2, 4, 7), c(3, 1, 5, 2))

gv_stat(list(g1, g2))
#> [1]  1.00000 20.38889
gv_stat(rbind(g1, g2), size = 4)
#> [1]  1.00000 20.38889

x <- array(c(g1, g2), dim = c(2, 4, 2))
gv_stat(x)
#> [1] 1.500000 4.944444
```
