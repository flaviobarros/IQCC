# X-bar Control Chart for phase I and II.

Builds the x-bar control chart for phase I or phase II.

## Usage

``` r
cchart.Xbar(
  x1 = NULL,
  n1 = NULL,
  x2 = NULL,
  n2 = NULL,
  x2bars = NULL,
  sigma = NULL
)
```

## Arguments

- x1:

  The phase I data to be plotted.

- n1:

  A value or a vector of values specifying the sample sizes associated
  with each group for the phase I data.

- x2:

  The phase II data to be plotted.

- n2:

  A value or a vector of values specifying the sample sizes associated
  with each group for the phase II data.

- x2bars:

  The mean of means from phase I.

- sigma:

  The standard deviation from phase I.

## Value

Return an x-bar control chart.

## See also

[cchart.Xbar1](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar1.md),
[cchart.Xbar2](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar2.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

data(pistonrings)
cchart.Xbar(x1 = pistonrings[1:25, ], n1 = 5)

cchart.Xbar(x1 = pistonrings[1:25, ], n1 = 5, x2 = pistonrings[26:40, ], n2 = 5)

#> List of 11
#>  $ call      : language qcc(data = x2, type = "xbar", center = x2bars, std.dev = sigma)
#>  $ type      : chr "xbar"
#>  $ data.name : chr "x2"
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
