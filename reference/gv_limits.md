# Generalized Variance Control Limits

Compute generalized variance control limits from exact moments,
Cornish-Fisher correction, available exact distributions, or simulation.

## Usage

``` r
gv_limits(
  n,
  p,
  det_sigma = 1,
  alpha = 0.0027,
  type = c("normal", "cf", "exact", "simulation"),
  side = c("upper", "two.sided"),
  cf_order = 1,
  nsim = 1e+05,
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

  False-alarm probability.

- type:

  Limit method: normal, Cornish-Fisher, exact, or simulation.

- side:

  Upper or two-sided chart.

- cf_order:

  Cornish-Fisher order, 1 or 2.

- nsim:

  Number of simulations for simulated limits.

- seed:

  Optional simulation seed.

## Value

A list containing lower and upper control limits and distribution
details.
