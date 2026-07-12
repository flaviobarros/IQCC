# False-Alarm Risk for Generalized Variance Charts

Evaluate the actual false-alarm risk of a generalized variance chart
exactly for dimension two or by simulation for higher dimensions.

## Usage

``` r
gv_alpha_risk(
  n,
  p,
  det_sigma = 1,
  alpha = 0.0027,
  type = c("normal", "cf", "exact", "simulation"),
  side = c("upper", "two.sided"),
  cf_order = 1,
  nsim = 2e+05,
  seed = NULL
)
```

## Arguments

- n:

  Subgroup sample size.

- p:

  Process dimension.

- det_sigma:

  Determinant of the in-control covariance matrix.

- alpha:

  Nominal false-alarm probability.

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

## Value

A list with nominal and actual false-alarm probabilities, ARL0, and
limits.
