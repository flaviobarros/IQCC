# Double-Sampling np Chart: Average Sample Size

Compute the Average Sample Size (ASS) for the double-sampling np chart
at a given nonconforming proportion p.

## Usage

``` r
dsnp_ass(p, n1, n2, wl, ucl1)
```

## Arguments

- p:

  The nonconforming proportion to evaluate. Can be a scalar or numeric
  vector, with values in \[0, 1\].

- n1:

  The first-stage sample size (positive integer).

- n2:

  The second-stage sample size (positive integer).

- wl:

  The fractional warning limit. Must be less than ucl1.

- ucl1:

  The fractional upper control limit for the first stage.

## Value

A list with the following elements:

- ass:

  The Average Sample Size: n1 + n2 \* p_second.

- p_second:

  Probability of requiring the second sample.

- n1, n2, wl, ucl1:

  The input chart parameters.

## Details

The ASS is the expected total number of items inspected per signal
decision. If the process is accepted or signals at the first stage, n1
items are inspected. If the second stage is required, n1 + n2 items are
inspected. ASS = n1 + n2 \* P(second stage is required).

## References

Joekes, S., Smrekar, M. and Barbosa, E. (2015). Extending a double
sampling control chart for non-conforming proportion in high quality
processes to the case of small samples.

## See also

[dsnp_prob_accept](https://flaviobarros.github.io/IQCC/reference/dsnp_prob_accept.md),
[dsnp_arl](https://flaviobarros.github.io/IQCC/reference/dsnp_arl.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

# Published example from Joekes et al. (2015)
res <- dsnp_ass(0.005, n1 = 34, n2 = 162,
                wl = 1.5, ucl1 = 2.5)
res$ass  # approximately 35.94
#> [1] 35.93534
```
