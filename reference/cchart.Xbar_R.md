# X-bar and R control charts

This function builds the X-bar and R control charts in the same window.

## Usage

``` r
cchart.Xbar_R(x, sizes)
```

## Arguments

- x:

  The data to be plotted.

- sizes:

  A value or a vector of values specifying the sample sizes associated
  with each group.

## Value

Return the two control charts.

## Author

Daniela R. Recchia, Emanuel P. Barbosa.

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
