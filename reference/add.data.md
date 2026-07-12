# Updates the Hotelling Control Chart.

This function is used to update the phase II control chart with new
observations.

## Usage

``` r
add.data(datum2, estat, T2II, n, j, m = NULL)
```

## Arguments

- datum2:

  The data set for the phase II. Should be a matrix.

- estat:

  The values of the auxiliary statistics. Should be a list with a vector
  with the mean of the mean vectors, a matrix with the average of the
  variance-covariance matrices and a matrix with the means.

- T2II:

  A vector with the value of T2 statistic for one sample.

- n:

  The sample size. For individual observations, use n = 1.

- j:

  The index of the current sample.

- m:

  The number of samples in phase I. Only needed if the phase I data set
  is show on the plot.

## Value

Add the new observation to the current Hotelling control chart for phase
II. Returns the new T2 statistic invisibly.

## Details

To use this function it is necessary to have the output given by the
function T2.2. At every step you should entry with the new data set.

## See also

[T2.2](https://flaviobarros.github.io/IQCC/reference/T2.2.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

mu <- c(5.682, 88.22)
Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
datum <- data.1(20, 10, mu, Sigma)
estat <- stats(datum, 20, 10, 2)
datum2 <- data.2(estat, 10, p = 2)
T2II <- T2.2(datum2, estat, 10)
#Not showing the phase I data set.
cchart.T2.2(T2II, 20, 10, 1, 25, 2)
datum3 <- data.2(estat, 10, p = 2)
add.data(datum3, estat, T2II, 10, 2)

#Showing the phase I data set.
cchart.T2.2(T2II, 20, 10, 1, 25, 2, datum = datum)
datum3 <- data.2(estat, 10, p = 2)
add.data(datum3, estat, T2II, 10, 2, 20)


#Example with individual observations
datum <- data.1(50, 1, mu, Sigma)
estat <- stats(datum, 50, 1, 2)
datum2 <- data.2(estat, 1, p = 2)
T2II <- T2.2(datum2, estat, 1)
#Not showing the phase I data set.
cchart.T2.2(T2II, 50, 1, 1, 25, 2)
datum3 <- data.2(estat, 1, p = 2)
add.data(datum3, estat, T2II, 1, 2)

#Showing the phase I data set.
cchart.T2.2(T2II, 50, 1, 1, 25, 2, datum = datum)
datum3 <- data.2(estat, 1, p = 2)
add.data(datum3, estat, T2II, 1, 2, 50)

```
