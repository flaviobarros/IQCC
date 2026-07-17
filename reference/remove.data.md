# Remove an Observation from Phase I Data

Remove the \\i\\-th subgroup from a Phase I data set (array or matrix).
Used during Phase I retrospective analysis to eliminate out-of-control
signals before recomputing reference limits.

## Usage

``` r
remove.data(datum, i)
```

## Arguments

- datum:

  Phase I data (matrix for \\n = 1\\, 3D array for \\n \> 1\\).

- i:

  Index of the subgroup to remove (1-based).

## Value

The data set without the \\i\\-th subgroup.

## See also

[`data.1`](https://flaviobarros.github.io/IQCC/reference/data.1.md),
[`T2.1`](https://flaviobarros.github.io/IQCC/reference/T2.1.md)

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
