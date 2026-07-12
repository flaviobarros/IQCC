# R control chart

This function builds a R control chart.

## Usage

``` r
cchart.R(x, n, type = "norm", y = NULL)
```

## Arguments

- x:

  The data to be plotted.

- n:

  The sample size.

- type:

  The type of R chart to be plotted. The options are "norm" (traditional
  Shewhart R chart) and "tukey" (exact R chart). If not specified, a
  Shewhart R chart will be plotted.

- y:

  The data used in phase I to estimate the standard deviation. Required
  when type = "tukey".

## Value

Return a R control chart.

## Details

The Shewhart R chart was designed for phase I (at this moment). The
limits of the exact R chart are the alpha/2 and 1-alpha/2 quantiles of
the R distribution that are calculated as estimated process sd times the
quantiles of the relative range (W=R/sigma) distribution.

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

data(pistonrings)
attach(pistonrings)
cchart.R(pistonrings[1:25,], 5)

cchart.R(pistonrings[26:40, ], 5, type = "tukey", pistonrings[1:25, ])

#> List of 11
#>  $ call      : language qcc(data = x, type = "R", limits = c(qtukey(Q_LOWER, n, Inf) * sd.R(y),      qtukey(Q_UPPER, n, Inf) * sd.R(y)))
#>  $ type      : chr "R"
#>  $ data.name : chr "x"
#>  $ data      : num [1:15, 1:5] 74 74 74 74 74 ...
#>   ..- attr(*, "dimnames")=List of 2
#>  $ statistics: Named num [1:15] 0.044 0.025 0.015 0.019 0.017 ...
#>   ..- attr(*, "names")= chr [1:15] "26" "27" "28" "29" ...
#>  $ sizes     : Named int [1:15] 5 5 5 5 5 5 5 5 5 5 ...
#>   ..- attr(*, "names")= chr [1:15] "26" "27" "28" "29" ...
#>  $ center    : num 0.0245
#>  $ std.dev   : num 0.0105
#>  $ nsigmas   : num 3
#>  $ limits    : num [1, 1:2] 0.00388 0.05262
#>   ..- attr(*, "dimnames")=List of 2
#>  $ violations:List of 2
#>  - attr(*, "class")= chr "qcc"
```
