# Plot a DS-np Control Chart

Plot first-stage and second-stage DS-np decisions on separate panels so
that the first-stage count and combined count are shown on their proper
scales.

## Usage

``` r
# S3 method for class 'cchart.DSnp'
plot(x, ...)
```

## Arguments

- x:

  An object of class `"cchart.DSnp"`.

- ...:

  Additional graphical parameters passed to the first panel.

## Value

Invisible `x`.

## Details

The upper panel displays `x1`, the warning limit, and the first-stage
upper control limit. The lower panel displays `x1 + x2` only for
observations that reached the second stage, together with the
second-stage upper control limit.
