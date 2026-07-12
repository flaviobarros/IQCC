# False Alarm probability for the 3-sigma R chart.

Used to calculate the real probability of false alarm in the 3-sigma R
chart.

## Usage

``` r
alpha.risk(n)
```

## Arguments

- n:

  The sample size. Can be a vector for multiple sample sizes.

## Value

Return a vector of alpha risk values for the given sample sizes.

## Details

This alpha risk is calculated under the exact R statistics distribution
and its values for small sample sizes will be much larger than the
reference value 0,0027.

## See also

[d2](https://flaviobarros.github.io/IQCC/reference/d2.md),[d3](https://flaviobarros.github.io/IQCC/reference/d3.md),[c4](https://flaviobarros.github.io/IQCC/reference/c4.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

alpha.risk(15)
#> [1] 0.004493839
```
