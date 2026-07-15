# Double-Sampling np Chart: Acceptance Probability

Compute the total probability that the double-sampling np chart accepts
(does not signal) at a given nonconforming proportion.

## Usage

``` r
dsnp_prob_accept(p, n1, n2, wl, ucl1, ucl2)
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

A list with the following elements:

- pa1:

  First-stage acceptance probability.

- pa2:

  Second-stage acceptance probability.

- pt:

  Total acceptance probability, `pa1 + pa2`.

- p_signal:

  Total signal probability, `1 - pt`.

- p_decision_first:

  Probability of either accepting or signaling at the first stage.

- p_second:

  Probability that the second sample is required.

- n1, n2, wl, ucl1, ucl2:

  The validated chart parameters.

- wl_accept:

  Integer threshold `floor(wl)`; accept at stage one when \\D_1\\ does
  not exceed this value.

- ucl1_reject:

  Integer threshold `floor(ucl1) + 1`; signal at stage one when \\D_1\\
  is at least this value.

- ucl2_accept:

  Integer threshold `floor(ucl2)`; accept at stage two when \\D_1 +
  D_2\\ does not exceed this value.

## Details

A subgroup is accepted immediately when the first-stage count \\D_1\\ is
at or below `floor(wl)` and signals immediately when \\D_1\\ is at or
above `floor(ucl1) + 1`. Counts between those thresholds continue to the
second stage. A continued subgroup is accepted when \\D_1 + D_2\\ is at
or below `floor(ucl2)` and signals otherwise.

## References

Joekes, S., Smrekar, M. and Barbosa, E. P. (2015). Extending a double
sampling control chart for non-conforming proportion in high quality
processes to the case of small samples. Statistical Methodology, 23,
35-49.

## See also

[`dsnp_arl`](https://flaviobarros.github.io/IQCC/reference/dsnp_arl.md),
[`dsnp_ass`](https://flaviobarros.github.io/IQCC/reference/dsnp_ass.md),
[`dsnp_limits`](https://flaviobarros.github.io/IQCC/reference/dsnp_limits.md)

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r
result <- dsnp_prob_accept(
    0.005, n1 = 34, n2 = 162, wl = 1.5, ucl1 = 2.5, ucl2 = 4.5
)
result$pt
#> [1] 0.9987553
result$p_signal
#> [1] 0.001244692
```
