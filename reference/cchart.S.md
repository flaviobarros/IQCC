# Standard-Deviation Control Chart

Build a control chart for subgroup standard deviations using either the
conventional Shewhart limits used historically by `qcc` or exact
probability limits derived from the chi-square distribution of the
sample variance. Numerical limits are delegated to
[`s_shewhart_limits`](https://flaviobarros.github.io/IQCC/reference/s_shewhart_limits.md)
and
[`s_exact_limits`](https://flaviobarros.github.io/IQCC/reference/s_exact_limits.md).

## Usage

``` r
cchart.S(x, type = c("n", "e"), m = NULL)
```

## Arguments

- x:

  Subgroup data accepted by
  [`qcc::qcc()`](https://rdrr.io/pkg/qcc/man/qcc.html) for an `"S"`
  chart. Rows represent subgroups and columns observations within
  subgroups.

- type:

  Either `"n"` for the normalized qcc-compatible limits or `"e"` for
  exact equal-tail probability limits.

- m:

  Integer subgroup size, at least 2. It is required when `type = "e"`.
  If omitted, a warning is issued and the normalized chart is drawn
  instead. This argument is retained unchanged for compatibility with
  historical positional calls.

## Value

Invisibly, the `"qcc"` object returned by
[`qcc::qcc()`](https://rdrr.io/pkg/qcc/man/qcc.html). The function also
draws the chart.

## Details

For exact limits, \\(m-1)S^2/\sigma^2\\ follows a chi-square
distribution with \\m-1\\ degrees of freedom under normality. The
historical wrapper estimates \\\sigma\\ from `x` using
[`qcc::sd.S()`](https://rdrr.io/pkg/qcc/man/stats.S.html) and supplies
that estimate to
[`s_exact_limits()`](https://flaviobarros.github.io/IQCC/reference/s_exact_limits.md);
these are therefore plug-in probability limits.

For `type = "n"`, the legacy qcc convention is deliberately preserved:
the center is the weighted mean of the observed subgroup standard
deviations, while \\\sigma\\ is estimated separately by
[`qcc::sd.S()`](https://rdrr.io/pkg/qcc/man/stats.S.html). The resulting
standard error is \\\hat\sigma\sqrt{1-c_4(n)^2}\\. This differs in
finite Phase I samples from replacing the observed center by the
theoretical \\c_4(n)\hat\sigma\\; `cchart.S()` does not make that
replacement.

## Phase convention

The existing API has no separate Phase I argument. Both the chart center
and \\\sigma\\ estimate are obtained from `x`; exact limits additionally
use the supplied historical design size `m`. Thus both normalized and
exact limits are plug-in limits when used through this wrapper.

## Errors and warnings

An error is raised for an unsupported `type` or an invalid supplied `m`.
If exact limits are requested without `m`, the function warns and falls
back to the normalized chart. Additional data validation is performed by
[`qcc::qcc()`](https://rdrr.io/pkg/qcc/man/qcc.html),
[`qcc::stats.S()`](https://rdrr.io/pkg/qcc/man/stats.S.html), and
[`qcc::sd.S()`](https://rdrr.io/pkg/qcc/man/stats.S.html).

## References

Montgomery, D. C. (2009). *Introduction to Statistical Quality Control*,
6th ed. Wiley.

## See also

[`s_shewhart_limits`](https://flaviobarros.github.io/IQCC/reference/s_shewhart_limits.md),
[`s_exact_limits`](https://flaviobarros.github.io/IQCC/reference/s_exact_limits.md),
[`cchart.R`](https://flaviobarros.github.io/IQCC/reference/cchart.R.md),
[`c4`](https://flaviobarros.github.io/IQCC/reference/c4.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r
data(softdrink)
normalized <- cchart.S(softdrink, type = "n")

exact <- cchart.S(softdrink, type = "e", m = 10)
```
