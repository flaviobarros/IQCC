# Hotelling Control Chart Phase I simulated data.

This function simulates a normal data set to be used in the phase I
Hotelling control charts.

## Usage

``` r
data.1(m, n, mu, Sigma)
```

## Arguments

- m:

  The number of samples to be generated.

- n:

  The size of each sample. If they are individual observations, then use
  n = 1.

- mu:

  The vector with the means of the data to be generated.

- Sigma:

  The variance-covariance matrix of the data to be generated.

## Value

Return an array with the simulated data.

## See also

[data.2](https://flaviobarros.github.io/IQCC/reference/data.2.md)

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
