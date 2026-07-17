# Simulate Phase II Multivariate Normal Data

Generate a single new observation from \\N(\mu_0 + \delta, \Sigma)\\
using Phase I reference parameters for Phase II monitoring.

## Usage

``` r
data.2(estat, n, delta = 0, p)
```

## Arguments

- estat:

  List from
  [`stats`](https://flaviobarros.github.io/IQCC/reference/stats.md)
  (Phase I estimates).

- n:

  Subgroup size. Use \\n = 1\\ for individual observations, \\n \> 1\\
  for subgroups.

- delta:

  Optional shift added to mean vector for out-of-control simulation.

- p:

  Dimension.

## Value

For \\n = 1\\: a numeric vector. For \\n \> 1\\: an \\n \times p\\
matrix.

## Details

To use this function it is necessary to have the information about Phase
I given by the functions
[`data.1`](https://flaviobarros.github.io/IQCC/reference/data.1.md) and
[`stats`](https://flaviobarros.github.io/IQCC/reference/stats.md).

## See also

[`data.1`](https://flaviobarros.github.io/IQCC/reference/data.1.md),
[`T2.2`](https://flaviobarros.github.io/IQCC/reference/T2.2.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

mu <- c(5.682, 88.22)
Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
datum <- data.1(20, 10, mu, Sigma)
# estat is the list with the values of the auxiliary statistics.
estat <- stats(datum, 20, 10, 2)
datum2 <- data.2(estat, 10, p = 2)
```
