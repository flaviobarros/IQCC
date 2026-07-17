# Simulate Phase I Multivariate Normal Data

Generate \\m\\ samples from a \\p\\-variate normal distribution for
Phase I Hotelling T² chart development. For \\n = 1\\: returns an \\m
\times p\\ matrix. For \\n \> 1\\: returns an \\n \times p \times m\\
array.

## Usage

``` r
data.1(m, n, mu, Sigma)
```

## Arguments

- m:

  Number of samples.

- n:

  Sample size per subgroup. Use \\n = 1\\ for individual observations,
  \\n \> 1\\ for subgroups.

- mu:

  Mean vector (length \\p\\).

- Sigma:

  \\p \times p\\ covariance matrix.

## Value

For \\n = 1\\: an \\m \times p\\ matrix. For \\n \> 1\\: an \\n \times p
\times m\\ array.

## Simulation

Uses [`MASS::mvrnorm()`](https://rdrr.io/pkg/MASS/man/mvrnorm.html). Set
the seed externally for reproducibility.

## See also

[`data.2`](https://flaviobarros.github.io/IQCC/reference/data.2.md),
[`T2.1`](https://flaviobarros.github.io/IQCC/reference/T2.1.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

mu <- c(5.682, 88.22)
Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
#Simulated data with individual observations
datum <- data.1(50, 1, mu, Sigma)
#Simulated data with sub-group observations
datum <- data.1(20, 10, mu, Sigma)
```
