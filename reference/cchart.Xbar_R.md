# X-bar and R Control Charts

Draws the X-bar control chart and the R (range) control chart side by
side in the same graphics window, using estimated Phase I control
limits. Both charts share the same subgroup data.

## Usage

``` r
cchart.Xbar_R(x, sizes)
```

## Arguments

- x:

  Phase I data. Matrix or data frame with subgroups in rows.

- sizes:

  Subgroup size(s). A single integer (equal sizes) or a vector.

## Value

Draws two control charts side by side. Returns nothing.

## Phase convention

Phase I `---` control limits estimated from the data for both charts.

## References

Montgomery, D.C., (2009). "Introduction to Statistical Quality Control".
Chapter 6. Wiley.

## See also

[`cchart.Xbar1`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar1.md),
[`cchart.Xbar2`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar2.md),
[`cchart.Xbar`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

data(pistonrings)
attach(pistonrings)
cchart.Xbar_R(pistonrings[1:25, ], 5)


#> List of 11
#>  $ call      : language qcc(data = x, type = "R", add.stats = FALSE)
#>  $ type      : chr "R"
#>  $ data.name : chr "x"
#>  $ data      : num [1:25, 1:5] 74 74 74 74 74 ...
#>   ..- attr(*, "dimnames")=List of 2
#>  $ statistics: Named num [1:25] 0.038 0.019 0.036 0.022 0.026 ...
#>   ..- attr(*, "names")= chr [1:25] "1" "2" "3" "4" ...
#>  $ sizes     : Named int [1:25] 5 5 5 5 5 5 5 5 5 5 ...
#>   ..- attr(*, "names")= chr [1:25] "1" "2" "3" "4" ...
#>  $ center    : num 0.0228
#>  $ std.dev   : num 0.00979
#>  $ nsigmas   : num 3
#>  $ limits    : num [1, 1:2] 0 0.0481
#>   ..- attr(*, "dimnames")=List of 2
#>  $ violations:List of 2
#>  - attr(*, "class")= chr "qcc"
```
