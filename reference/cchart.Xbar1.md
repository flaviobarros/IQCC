# X-bar Shewhart Control Chart for phase I.

Builds the x-bar control chart for phase I.

## Usage

``` r
cchart.Xbar1(x, sizes)
```

## Arguments

- x:

  The data to be plotted.

- sizes:

  A value or a vector of values specifying the sample sizes associated
  with each group.

## Value

Returns a list with the mean of means (x2bar) and the standard deviation
(sigma), invisibly. Also plots the control chart as a side effect.

## Details

Even if the data is not normal the x-bar statistic will be close to the
normal by the central limit theorem.

## See also

[cchart.Xbar2](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar2.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

data(pistonrings)
cchart.Xbar1(pistonrings[1:25, ], 5)

```
