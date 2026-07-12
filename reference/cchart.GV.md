# Generalized Variance Control Chart

Construct and plot a generalized variance control chart for multivariate
process variability.

## Usage

``` r
cchart.GV(
  x1,
  size = NULL,
  x2 = NULL,
  Sigma = NULL,
  alpha = 0.0027,
  type = c("normal", "cf", "exact", "simulation"),
  side = c("upper", "two.sided"),
  cf_order = 1,
  nsim = 1e+05,
  seed = NULL,
  plot = TRUE,
  ...
)

# S3 method for class 'cchart.GV'
plot(x, ...)
```

## Arguments

- x1:

  Phase I subgroups accepted by `gv_stat`.

- size:

  Subgroup size when matrix input is used.

- x2:

  Optional Phase II subgroups.

- Sigma:

  Optional in-control covariance matrix.

- alpha:

  False-alarm probability.

- type:

  Limit method.

- side:

  Upper or two-sided chart.

- cf_order:

  Cornish-Fisher order.

- nsim:

  Number of simulations.

- seed:

  Optional simulation seed.

- plot:

  Logical; draw the chart.

- ...:

  Additional graphical arguments.

- x:

  A `cchart.GV` object.

## Value

An object of class `cchart.GV`.
