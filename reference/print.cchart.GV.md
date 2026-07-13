# Print and Summarize a Generalized Variance Control Chart

Display or construct a compact statistical summary of a generalized
variance chart, including design dimensions, Phase I and Phase II
subgroup counts, control-limit method, covariance source, descriptive
statistics, and signaled subgroups.

## Usage

``` r
# S3 method for class 'cchart.GV'
print(x, digits = max(3L, getOption("digits") - 3L), ...)

# S3 method for class 'cchart.GV'
summary(object, ...)

# S3 method for class 'summary.cchart.GV'
print(x, digits = max(3L, getOption("digits") - 3L), ...)
```

## Arguments

- x:

  An object of class `"cchart.GV"` or `"summary.cchart.GV"`.

- digits:

  Number of significant digits used for numerical output.

- ...:

  Additional arguments passed to the summary print method; otherwise
  currently unused.

- object:

  An object of class `"cchart.GV"`.

## Value

`summary.cchart.GV()` returns an object of class `"summary.cchart.GV"`.
The returned list contains `call`, `dimensions`, `subgroups`, `method`,
`limits`, `covariance`, `statistics`, and `signals`. The print methods
return their input invisibly.

## See also

[`cchart.GV`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md),
[`plot.cchart.GV`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md)
