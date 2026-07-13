# Range Control Chart

Build a control chart for subgroup ranges using either the conventional
three-sigma approximation or exact probability limits from the
distribution of the relative range \\W = R / \sigma\\.

## Usage

``` r
cchart.R(x, n, type = c("norm", "tukey"), y = NULL)
```

## Arguments

- x:

  Phase II subgroup data accepted by
  [`qcc::qcc()`](https://rdrr.io/pkg/qcc/man/qcc.html) for an `"R"`
  chart. Rows represent subgroups and columns observations within
  subgroups.

- n:

  Integer subgroup size, at least 2.

- type:

  Either `"norm"` for the conventional three-sigma chart or `"tukey"`
  for exact equal-tail probability limits.

- y:

  Phase I subgroup data used to estimate \\\sigma\\ through
  [`qcc::sd.R()`](https://rdrr.io/pkg/qcc/man/stats.R.html) when
  `type = "tukey"`.

## Value

Invisibly, the `"qcc"` object returned by
[`qcc::qcc()`](https://rdrr.io/pkg/qcc/man/qcc.html). The function also
draws the chart.

## Details

For `type = "norm"`, limits are delegated to `qcc`. For
`type = "tukey"`, exact limits are computed from
[`stats::qtukey()`](https://rdrr.io/r/stats/Tukey.html) and the Phase I
scale estimate `qcc::sd.R(y)`. The conventional chart annotates the
actual false-alarm risk from `alpha.risk(n)`.

## Phase convention

The exact chart treats `y` as Phase I reference data and `x` as the
plotted monitoring data.

## Errors

An error is raised for an unsupported `type`, for `n < 2`, or when `y`
is omitted for the exact chart.

## References

Barbosa, E. P., Gneri, M. A., and Meneguetti, A. (2013). Range control
charts revisited: Simpler Tippett-like formulae, its practical
implementation, and the study of false alarm. *Communications in
Statistics - Simulation and Computation*, 42(2), 247–262.
[doi:10.1080/03610918.2011.639967](https://doi.org/10.1080/03610918.2011.639967)
.

## See also

[`alpha.risk`](https://flaviobarros.github.io/IQCC/reference/alpha.risk.md),
[`cchart.S`](https://flaviobarros.github.io/IQCC/reference/cchart.S.md),
[`table.qtukey`](https://flaviobarros.github.io/IQCC/reference/table.qtukey.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r
data(pistonrings)
conventional <- cchart.R(pistonrings[1:25, ], 5)

exact <- cchart.R(pistonrings[26:40, ], 5, type = "tukey", y = pistonrings[1:25, ])
```
