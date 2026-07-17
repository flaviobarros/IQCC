# Phase II X-bar Shewhart Control Chart

Builds the X-bar control chart for Phase II monitoring using reference
limits obtained from a Phase I analysis. The Phase I estimates of
`x2bar` (mean of means) and `sigma` (standard deviation) are required,
typically obtained from
[`cchart.Xbar1`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar1.md).

## Usage

``` r
cchart.Xbar2(x, x2bar, sigma, sizes)
```

## Arguments

- x:

  Phase II data. Matrix or data frame with subgroups in rows.

- x2bar:

  Mean of subgroup means from Phase I (center line).

- sigma:

  Standard deviation from Phase I (for control limits).

- sizes:

  Phase II subgroup size(s).

## Value

Draws the X-bar control chart. Returns nothing.

## Phase convention

Phase II `---` tests new data against Phase I reference distribution.

## References

Montgomery, D.C., (2009). "Introduction to Statistical Quality Control".
Chapter 6. Wiley.

## See also

[`cchart.Xbar1`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar1.md),
[`cchart.Xbar`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

data(pistonrings)
stat <- cchart.Xbar1(pistonrings[1:25, ], 5)

cchart.Xbar2(pistonrings[26:40, ], stat[[1]][1], stat[[1]][2], 5)

#> List of 11
#>  $ call      : language qcc(data = x, type = "xbar", center = x2bar, std.dev = sigma)
#>  $ type      : chr "xbar"
#>  $ data.name : chr "x"
#>  $ data      : num [1:15, 1:5] 74 74 74 74 74 ...
#>   ..- attr(*, "dimnames")=List of 2
#>  $ statistics: Named num [1:15] 74 74 74 74 74 ...
#>   ..- attr(*, "names")= chr [1:15] "26" "27" "28" "29" ...
#>  $ sizes     : Named int [1:15] 5 5 5 5 5 5 5 5 5 5 ...
#>   ..- attr(*, "names")= chr [1:15] "26" "27" "28" "29" ...
#>  $ center    : num 74
#>  $ std.dev   : num 0.00979
#>  $ nsigmas   : num 3
#>  $ limits    : num [1, 1:2] 74 74
#>   ..- attr(*, "dimnames")=List of 2
#>  $ violations:List of 2
#>  - attr(*, "class")= chr "qcc"
```
