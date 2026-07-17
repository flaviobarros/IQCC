# Phase I X-bar Shewhart Control Chart

Builds the X-bar control chart for Phase I retrospective analysis. The
control limits are estimated from the supplied data. Even if the data is
not normal, the X-bar statistic is approximately normal by the central
limit theorem when subgroup sizes are sufficiently large.

## Usage

``` r
cchart.Xbar1(x, sizes)
```

## Arguments

- x:

  Phase I data. Matrix or data frame with subgroups in rows.

- sizes:

  Subgroup size(s). A single integer (equal sizes) or a vector.

## Value

A list with components `x2bar` (mean of subgroup means) and `sigma`
(standard deviation), returned invisibly. The control chart is drawn as
a side effect.

## Phase convention

Phase I `---` control limits estimated from the data.

## References

Montgomery, D.C., (2009). "Introduction to Statistical Quality Control".
Chapter 6. Wiley.

## See also

[`cchart.Xbar2`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar2.md),
[`cchart.Xbar`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar.md),
[`cchart.Xbar_R`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar_R.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

data(pistonrings)
cchart.Xbar1(pistonrings[1:25, ], 5)

```
