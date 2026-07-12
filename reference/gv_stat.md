# Generalized Variance by Subgroup

Calculate the generalized variance \\\|S\|\\ for each subgroup.

## Usage

``` r
gv_stat(x, size = NULL)
```

## Arguments

- x:

  A list of numeric matrices, a three-dimensional array, or a numeric
  matrix containing consecutive subgroups.

- size:

  Subgroup size when `x` is a matrix.

## Value

A numeric vector containing the determinant of the sample covariance
matrix for each subgroup.

## Examples

``` r
x <- array(rnorm(4 * 6 * 2), dim = c(4, 6, 2))
gv_stat(x)
#> [1] 0.6492781 0.1730728 0.4904174 0.2837441
```
