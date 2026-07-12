# Remove an undesirable observation.

This function removes an undesirable data that might be out of control
in you data set. It is used at Hotelling T2 control charts for phase I.

## Usage

``` r
remove.data(datum, i)
```

## Arguments

- datum:

  The data set. Should be an array.

- i:

  The index in the matrix of the data to be removed.

## Value

Return the new data set without the observation that was removed.

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

mu <- c(5.682, 88.22)
Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
datum <- data.1(20, 10, mu, Sigma)
# Removing the observatiob 13 from the data set "datum" and updating it:
datum <- remove.data(datum, 13)
```
