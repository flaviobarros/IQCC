# Double-Sampling np Chart: Average Run Length

Compute the average run length for the double-sampling np chart as the
reciprocal of the signal probability at each supplied nonconforming
proportion.

## Usage

``` r
dsnp_arl(p, n1, n2, wl, ucl1, ucl2)
```

## Arguments

- p:

  Nonconforming proportion to evaluate, a finite numeric scalar or
  vector in \\\[0, 1\]\\.

- n1:

  First-stage sample size, a positive integer.

- n2:

  Second-stage sample size, a positive integer.

- wl:

  Finite fractional warning limit.

- ucl1:

  Finite fractional first-stage upper control limit greater than `wl`.

- ucl2:

  Finite fractional combined upper control limit greater than `wl`.

## Value

A list with `arl`, the average run length; `pt`, total acceptance
probability; `p_signal`, signal probability; and the validated chart
parameters `n1`, `n2`, `wl`, `ucl1`, and `ucl2`.

## Details

For signal probability \\1 - P_T(p)\\, the ARL is \\1 / (1 - P_T(p))\\.
It is infinite when the signal probability is zero.

## References

Joekes, S., Smrekar, M. and Barbosa, E. P. (2015). Extending a double
sampling control chart for non-conforming proportion in high quality
processes to the case of small samples. *Statistical Methodology*, 23,
35–49.

## See also

[dsnp_prob_accept](https://flaviobarros.github.io/IQCC/reference/dsnp_prob_accept.md),
[dsnp_ass](https://flaviobarros.github.io/IQCC/reference/dsnp_ass.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r
arl0 <- dsnp_arl(0.005, 34, 162, 1.5, 2.5, 4.5)$arl
arl1 <- dsnp_arl(0.0075, 34, 162, 1.5, 2.5, 4.5)$arl
c(arl0 = arl0, arl1 = arl1)
#>     arl0     arl1 
#> 803.4114 193.2229 
```
