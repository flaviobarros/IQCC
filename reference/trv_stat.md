# Trace Statistic for Multivariate Variability

Compute the auxiliary trace statistic \$\$T = (n - 1)
\operatorname{tr}(\Sigma_0^{-1} S),\$\$ where \\S\\ is the unbiased
subgroup covariance matrix and \\\Sigma_0\\ is the in-control covariance
matrix.

## Usage

``` r
trv_stat(x, size = NULL, Sigma0)
```

## Arguments

- x:

  Multivariate subgroup data in any format accepted by
  [`gv_stat`](https://flaviobarros.github.io/IQCC/reference/gv_stat.md):
  a list of numeric matrices, a subgroup-by-observation-by-variable
  array, or a stacked numeric matrix.

- size:

  Positive integer subgroup size when `x` is a stacked matrix. It is
  ignored for list and array input.

- Sigma0:

  Finite symmetric positive-definite in-control covariance matrix.

## Value

A numeric vector with one trace statistic per subgroup.

## Details

The statistic complements the generalized variance \\\|S\|\\. Two
covariance matrices can have the same determinant but different trace
after standardization by \\\Sigma_0\\; `trv_stat()` is designed to make
that structural change visible.

Under independent sampling from a multivariate normal process with
covariance \\\Sigma_0\\, \$\$(n - 1)S \sim W_p(n - 1, \Sigma_0)\$\$ and
therefore \$\$(n - 1)\operatorname{tr}(\Sigma_0^{-1}S) \sim
\chi^2\_{p(n - 1)}.\$\$ The implementation evaluates the trace as
`(n - 1) * sum(Sigma0_inverse * S)`, avoiding an explicit matrix square
root while preserving the same value.

## Errors

Errors are raised for invalid subgroup data, non-finite observations,
subgroup sizes smaller than two, a non-square or non-symmetric `Sigma0`,
a dimension mismatch, or a covariance matrix that is not positive
definite.

## References

Barbosa, E. P., Gneri, M. A., and Meneguetti, A. *Improving
Shewhart-type Generalized Variance Control Charts for Multivariate
Process Variability Monitoring using Cornish-Fisher Quantile Correction,
Meijer-G Function and Other Tools*. Research report.

Anderson, T. W. (1984). *An Introduction to Multivariate Statistical
Analysis*, 2nd ed. Wiley.

## See also

[`trv_limits`](https://flaviobarros.github.io/IQCC/reference/trv_limits.md),
[`trv_alpha_risk`](https://flaviobarros.github.io/IQCC/reference/trv_alpha_risk.md),
[`cchart.trV`](https://flaviobarros.github.io/IQCC/reference/cchart.trV.md),
[`gv_stat`](https://flaviobarros.github.io/IQCC/reference/gv_stat.md)

## Examples

``` r
g1 <- cbind(c(0, 1, 2, 3), c(0, 2, 1, 3))
g2 <- cbind(c(1, 2, 4, 7), c(3, 1, 5, 2))
trv_stat(list(g1, g2), Sigma0 = diag(2))
#> [1] 10.00 29.75
```
