# Auxiliary Statistics for Hotelling T² Charts

Compute the reference statistics needed by
[`T2.1`](https://flaviobarros.github.io/IQCC/reference/T2.1.md) and
[`T2.2`](https://flaviobarros.github.io/IQCC/reference/T2.2.md) from
Phase I data. Returns a list with: `[[1]]` grand mean vector
(\\\bar{\bar{x}}\\), `[[2]]` pooled covariance matrix (\\\bar{S}\\),
`[[3]]` matrix of subgroup means.

## Usage

``` r
stats(datum, m, n, p)
```

## Arguments

- datum:

  For \\n = 1\\: \\m \times p\\ matrix. For \\n \> 1\\: \\n \times p
  \times m\\ array.

- m:

  Number of subgroups.

- n:

  Subgroup size.

- p:

  Dimension.

## Value

A list with three components: grand mean (vector), pooled covariance
(matrix), subgroup means (\\m \times p\\ matrix).

## Details

To use this function it is necessary to have the information from
[`data.1`](https://flaviobarros.github.io/IQCC/reference/data.1.md).

## Phase convention

Designed for Phase I reference data.

## See also

[`T2.1`](https://flaviobarros.github.io/IQCC/reference/T2.1.md),
[`data.1`](https://flaviobarros.github.io/IQCC/reference/data.1.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

mu <- c(5.682, 88.22)
Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
#Example with individual observations
datum <- data.1(50, 1, mu, Sigma)
estat <- stats(datum, 50, 1, 2)
#Example with sub-group observations
datum <- data.1(20, 10, mu, Sigma)
estat <- stats(datum, 20, 10, 2)
```
