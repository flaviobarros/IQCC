# Double-Sampling np Chart: Average Run Length

Compute the Average Run Length (ARL) for the double-sampling np chart at
a given nonconforming proportion p.

## Usage

``` r
dsnp_arl(p, n1, n2, wl, ucl1, ucl2)
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

- ucl2:

  The fractional upper control limit for the combined samples.

## Value

A list with the following elements:

- arl:

  The Average Run Length: 1 / p_signal.

- pt:

  Total acceptance probability from `dsnp_prob_accept`.

- p_signal:

  Total signaling probability.

- n1, n2, wl, ucl1, ucl2:

  The input chart parameters.

## Details

The ARL is defined as the expected number of samples before the chart
signals. For a given proportion p, ARL = 1 / (1 - PT(p)), where PT(p) is
the total acceptance probability computed by `dsnp_prob_accept`.

## References

Joekes, S., Smrekar, M. and Barbosa, E. (2015). Extending a double
sampling control chart for non-conforming proportion in high quality
processes to the case of small samples.

## See also

[dsnp_prob_accept](https://flaviobarros.github.io/IQCC/reference/dsnp_prob_accept.md),
[dsnp_ass](https://flaviobarros.github.io/IQCC/reference/dsnp_ass.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

# Published example from Joekes et al. (2015)
res0 <- dsnp_arl(0.005, n1 = 34, n2 = 162,
                 wl = 1.5, ucl1 = 2.5, ucl2 = 4.5)
res0$arl  # approximately 803.41
#> [1] 803.4114

res1 <- dsnp_arl(0.0075, n1 = 34, n2 = 162,
                 wl = 1.5, ucl1 = 2.5, ucl2 = 4.5)
res1$arl  # approximately 193.22
#> [1] 193.2229
```
