# Double-Sampling np Control Chart

Build and optionally plot a double-sampling np (DS-np) control chart for
monitoring the nonconforming proportion in high-quality processes.

## Usage

``` r
cchart.DSnp(
  x1,
  n1,
  n2,
  p0,
  x2 = NULL,
  wl = NULL,
  ucl1 = NULL,
  ucl2 = NULL,
  limits = NULL,
  alpha = 0.0027,
  p1 = NULL,
  plot = TRUE,
  ...
)
```

## Arguments

- x1:

  Integer vector of nonconforming counts from the first sample.

- n1:

  First-stage sample size (positive integer).

- n2:

  Second-stage sample size (positive integer).

- p0:

  In-control nonconforming proportion.

- x2:

  Optional integer vector of nonconforming counts from the second
  sample. Must have the same length as `x1` when provided. Use `NA` for
  observations where no second sample was taken. Required for
  observations where `x1` falls in the intermediate (warning) zone.

- wl:

  Fractional warning limit. Must be less than `ucl1`.

- ucl1:

  Fractional upper control limit for the first stage. Must be greater
  than `wl`.

- ucl2:

  Fractional upper control limit for the combined samples.

- limits:

  Optional object returned by
  [`dsnp_limits()`](https://flaviobarros.github.io/IQCC/reference/dsnp_limits.md).
  When provided, `wl`, `ucl1`, and `ucl2` are taken from `limits$best`.

- alpha:

  Maximum desired false alarm probability at `p0`. Used only when limits
  need to be computed via
  [`dsnp_limits()`](https://flaviobarros.github.io/IQCC/reference/dsnp_limits.md).

- p1:

  Optional out-of-control proportion. When provided, performance metrics
  at `p1` are included in the returned object.

- plot:

  Logical. If `TRUE` (default), draws the control chart. If `FALSE`,
  only returns the result object.

- ...:

  Additional arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

An object of class `"cchart.DSnp"`, which is a list with the following
elements:

- call:

  The matched call.

- data:

  A data.frame with columns `index`, `x1`, `x2`, `total`, `stage`, and
  `signal`.

- limits:

  A list with the fractional and integer thresholds: `wl`, `ucl1`,
  `ucl2`, `wl_accept`, `ucl1_reject`, `ucl2_accept`.

- parameters:

  A list with `n1`, `n2`, `p0`, `alpha`, and `p1`.

- performance:

  A list with in-control performance metrics (`arl0`, `ass0`,
  `p_signal0`), and optionally out-of-control metrics (`arl1`, `ass1`,
  `p_signal1`) when `p1` is provided.

## Details

The DS-np chart uses two sampling stages. At the first stage, a sample
of size `n1` is inspected. If the count `x1` is at or below the warning
limit, the process is accepted. If `x1` exceeds the first upper control
limit, the process signals out-of-control immediately. Otherwise, a
second sample of size `n2` is inspected and the combined count `x1 + x2`
is compared to the second upper control limit.

Limits can be supplied manually via `wl`, `ucl1`, and `ucl2`, obtained
from a pre-computed
[`dsnp_limits()`](https://flaviobarros.github.io/IQCC/reference/dsnp_limits.md)
object via the `limits` argument, or computed automatically inside the
function when neither is provided.

The fractional limits are converted to integer thresholds using the
convention from the numerical core functions:

- `wl_accept = floor(wl)`: accept at first stage if `x1 <= wl_accept`.

- `ucl1_reject = floor(ucl1) + 1`: signal at first stage if
  `x1 >= ucl1_reject`.

- `ucl2_accept = floor(ucl2)`: accept at second stage if
  `x1 + x2 <= ucl2_accept`.

## References

Joekes, S., Smrekar, M. and Barbosa, E. (2015). Extending a double
sampling control chart for non-conforming proportion in high quality
processes to the case of small samples. *Statistical Methodology*.

## See also

[`dsnp_limits`](https://flaviobarros.github.io/IQCC/reference/dsnp_limits.md),
[`dsnp_prob_accept`](https://flaviobarros.github.io/IQCC/reference/dsnp_prob_accept.md),
[`dsnp_arl`](https://flaviobarros.github.io/IQCC/reference/dsnp_arl.md),
[`dsnp_ass`](https://flaviobarros.github.io/IQCC/reference/dsnp_ass.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

# Small example with manual limits
x1 <- c(0, 1, 2, 3, 1, 0, 2, 4, 1, 0)
x2 <- c(NA, NA, 2, NA, NA, NA, 3, NA, NA, NA)
res <- cchart.DSnp(x1, n1 = 10, n2 = 20, p0 = 0.05,
                   x2 = x2, wl = 1.5, ucl1 = 2.5, ucl2 = 4.5,
                   plot = FALSE)
res$limits
#> $wl
#> [1] 1.5
#> 
#> $ucl1
#> [1] 2.5
#> 
#> $ucl2
#> [1] 4.5
#> 
#> $wl_accept
#> [1] 1
#> 
#> $ucl1_reject
#> [1] 3
#> 
#> $ucl2_accept
#> [1] 4
#> 
res$performance
#> $arl0
#> [1] 58.35236
#> 
#> $ass0
#> [1] 11.4927
#> 
#> $p_signal0
#> [1] 0.01713727
#> 
```
