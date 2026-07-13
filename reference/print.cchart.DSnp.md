# Print and Summarize a Double-Sampling np Control Chart

Display or construct a compact summary of a double-sampling np chart,
including sample sizes, decision thresholds, operating characteristics,
stage outcomes, and signaled observations.

## Usage

``` r
# S3 method for class 'cchart.DSnp'
print(x, digits = max(3L, getOption("digits") - 3L), ...)

# S3 method for class 'cchart.DSnp'
summary(object, ...)

# S3 method for class 'summary.cchart.DSnp'
print(x, digits = max(3L, getOption("digits") - 3L), ...)
```

## Arguments

- x:

  An object of class `"cchart.DSnp"` or `"summary.cchart.DSnp"`.

- digits:

  Number of significant digits used for numerical output.

- ...:

  Additional arguments passed to the summary print method; otherwise
  currently unused.

- object:

  An object of class `"cchart.DSnp"`.

## Value

`summary.cchart.DSnp()` returns an object of class
`"summary.cchart.DSnp"`. The returned list contains `call`,
`observations`, `parameters`, `limits`, `performance`, `stage_counts`,
and `signals`. The print methods return their input invisibly.

## See also

[`cchart.DSnp`](https://flaviobarros.github.io/IQCC/reference/cchart.DSnp.md),
[`plot.cchart.DSnp`](https://flaviobarros.github.io/IQCC/reference/plot.cchart.DSnp.md)
