# Double-Sampling np Chart: Average Sample Size

Compute the average sample size for the double-sampling np chart under
complete inspection of every second-stage sample that is requested.

## Usage

``` r
dsnp_ass(p, n1, n2, wl, ucl1)
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

## Value

A list with `ass`, the average sample size; `p_second`, the probability
that the second sample is required; and the validated chart parameters
`n1`, `n2`, `wl`, and `ucl1`.

## Details

If a decision is reached at the first stage, \\n_1\\ items are
inspected; otherwise all \\n_2\\ additional items are inspected.
Therefore \$\$ASS(p) = n_1 + n_2 P_p(\text{second stage}).\$\$ This
function does not use curtailed inspection within the second sample.

## References

Joekes, S., Smrekar, M. and Barbosa, E. P. (2015). Extending a double
sampling control chart for non-conforming proportion in high quality
processes to the case of small samples. *Statistical Methodology*, 23,
35–49.

## See also

[dsnp_prob_accept](https://flaviobarros.github.io/IQCC/reference/dsnp_prob_accept.md),
[dsnp_arl](https://flaviobarros.github.io/IQCC/reference/dsnp_arl.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r
dsnp_ass(0.005, 34, 162, 1.5, 2.5)$ass
#> [1] 35.93534
```
