# Phase I Hotelling T² Control Chart

Build a Phase I T² control chart. For \\n = 1\\: \\UCL = ((m-1)^2/m)
\times qbeta(1-\alpha, p/2, (m-p-1)/2)\\. For \\n \> 1\\: \\UCL =
(p(m-1)(n-1))/(mn-m-p+1) \times qf(1-\alpha, p, mn-m-p+1)\\. The control
limits are based on the beta and F distributions.

## Usage

``` r
cchart.T2.1(T2, m, n, p)
```

## Arguments

- T2:

  Numeric vector of T² statistics from
  [`T2.1`](https://flaviobarros.github.io/IQCC/reference/T2.1.md).

- m:

  Number of subgroups in Phase I.

- n:

  Subgroup size. Use \\n = 1\\ for individual observations, \\n \> 1\\
  for subgroups.

- p:

  Number of variables (dimension).

## Value

Draws the chart. Returns nothing.

## Phase convention

Phase I retrospective analysis. All points are plotted simultaneously.

## Errors

Stop if \\n \< 1\\.

## References

Montgomery, D.C., (2009). "Introduction to Statistical Quality Control".
Chapter 11. Wiley.

## See also

[`T2.1`](https://flaviobarros.github.io/IQCC/reference/T2.1.md),
[`cchart.T2.2`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.2.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

mu <- c(5.682, 88.22)
Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
datum <- data.1(20, 10, mu, Sigma)
estat <- stats(datum, 20, 10, 2)
T2 <- T2.1(estat, 20, 10)
# estat is a list with the auxiliary statistics. T2 is a matrix with the values of the T2 statistic.
cchart.T2.1(T2, 20, 10, 2)

```
