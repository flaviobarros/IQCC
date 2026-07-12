# Hotelling Control Chart Phase II simulated data.

This function simulate a normal data set to be used in the phase II
Hotelling control charts.

## Usage

``` r
data.2(estat, n, delta = 0, p)
```

## Arguments

- estat:

  The values of the auxiliary statistics. Should be a list with the mean
  of means, the average variance-covariance matrix, and a matrix with
  the means of each sample.

- n:

  The size of each sample. If they are individual observations, use n =
  1.

- delta:

  A value to be added on the vector of means.

- p:

  The dimension.

## Value

Return an array with the simulated data.

## Details

To use this function it is necessary to have the information about the
phase I given by the functions data.1 and stats.

## See also

[data.1](https://flaviobarros.github.io/IQCC/reference/data.1.md)

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
