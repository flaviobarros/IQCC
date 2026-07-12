# S Control Chart.

This function builds a S control chart.

## Usage

``` r
cchart.S(x, type = "n", m = NULL)
```

## Arguments

- x:

  The data to be plotted.

- type:

  A character string specifying the type of S control chart to be
  plotted where "n" plots a S chart with normalized probability limits
  and "e" plots a S chart with exact limits.

- m:

  The sample size. Only necessary in the control chart with exact
  (probability) limits.

## Value

Return a S control chart.

## Details

The exact limits are the alpha/2 and 1-alpha/2 quantiles of the S
distribution which is proportional to the square root of a chi-square
distribution.

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

data(softdrink)
#S chart with normalized probability limits
cchart.S(softdrink, type = "n")

#> List of 11
#>  $ call      : language qcc(data = x, type = "S")
#>  $ type      : chr "S"
#>  $ data.name : chr "x"
#>  $ data      : num [1:15, 1:10] 2.5 0 1.5 0 0 1 1 0 -2 -0.5 ...
#>   ..- attr(*, "dimnames")=List of 2
#>  $ statistics: Named num [1:15] 1.333 0.926 1.125 1.174 0.471 ...
#>   ..- attr(*, "names")= chr [1:15] "1" "2" "3" "4" ...
#>  $ sizes     : int [1:15] 10 10 10 10 10 10 10 10 10 10 ...
#>  $ center    : num 1.09
#>  $ std.dev   : num 1.12
#>  $ nsigmas   : num 3
#>  $ limits    : num [1, 1:2] 0.31 1.88
#>   ..- attr(*, "dimnames")=List of 2
#>  $ violations:List of 2
#>  - attr(*, "class")= chr "qcc"
#S chart with exact probability limits
cchart.S(softdrink, type = "e", 10)

#> List of 11
#>  $ call      : language qcc(data = x, type = "S", limits = c((sqrt(qchisq(Q_LOWER, m - 1)/(m -      1))) * sd.S(x), (sqrt(qchisq(Q_UPPER,| __truncated__
#>  $ type      : chr "S"
#>  $ data.name : chr "x"
#>  $ data      : num [1:15, 1:10] 2.5 0 1.5 0 0 1 1 0 -2 -0.5 ...
#>   ..- attr(*, "dimnames")=List of 2
#>  $ statistics: Named num [1:15] 1.333 0.926 1.125 1.174 0.471 ...
#>   ..- attr(*, "names")= chr [1:15] "1" "2" "3" "4" ...
#>  $ sizes     : int [1:15] 10 10 10 10 10 10 10 10 10 10 ...
#>  $ center    : num 1.09
#>  $ std.dev   : num 1.12
#>  $ nsigmas   : num 3
#>  $ limits    : num [1, 1:2] 0.417 1.95
#>   ..- attr(*, "dimnames")=List of 2
#>  $ violations:List of 2
#>  - attr(*, "class")= chr "qcc"
```
