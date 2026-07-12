# Phase I Hotelling Control Chart.

Builds the phase I Hotelling control chart.

## Usage

``` r
cchart.T2.1(T2, m, n, p)
```

## Arguments

- T2:

  The values of the T2 statistic. Should be a numeric vector.

- m:

  The number of samples generated previously in data.1.

- n:

  The size of each sample used previously in data.1. If they are
  individual observations, then use n = 1.

- p:

  The dimension used previously in function data.1.

## Value

Return a control chart.

## Details

It builds the Hotelling T2 control chart for multivariate normal data (m
samples / samples of size n \> 1), used retrospective / validation
analysis (phase I); the control limits are based on the F distribution.

## References

Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control".
Chapter 11. Wiley

## See also

[cchart.T2.2](https://flaviobarros.github.io/IQCC/reference/cchart.T2.2.md)

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
