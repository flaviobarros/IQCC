# Double-Sampling np Chart: Acceptance Probability

Compute the total probability that the double-sampling np chart accepts
(does not signal) at a given nonconforming proportion p.

## Usage

``` r
dsnp_prob_accept(p, n1, n2, wl, ucl1, ucl2)
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

  The fractional upper control limit for the first stage. Must be
  greater than wl.

- ucl2:

  The fractional upper control limit for the combined samples. Must be
  greater than wl.

## Value

A list with the following elements:

- pa1:

  Probability of acceptance at the first stage: P(D1 \<= floor(wl)).

- pa2:

  Probability of acceptance at the second stage.

- pt:

  Total acceptance probability: pa1 + pa2.

- p_signal:

  Total signaling probability: 1 - pt.

- p_decision_first:

  Probability that a decision (accept or signal) is made on the first
  sample.

- p_second:

  Probability of requiring the second sample: 1 - p_decision_first.

- n1, n2, wl, ucl1, ucl2:

  The input chart parameters.

- wl_accept:

  Integer threshold: floor(wl). Accept if D1 \<= wl_accept.

- ucl1_reject:

  Integer threshold: floor(ucl1) + 1. Signal if D1 \>= ucl1_reject.

- ucl2_accept:

  Integer threshold: floor(ucl2). Accept if D1 + D2 \<= ucl2_accept.

## Details

The chart uses two sampling stages. At the first stage, a sample of size
n1 is inspected. If the count d1 is below the warning limit (wl), the
process is accepted. If d1 exceeds the upper control limit (ucl1), the
process signals out-of-control. If d1 falls between wl and ucl1, a
second sample of size n2 is inspected and the combined count d1 + d2 is
compared to ucl2.

## References

Joekes, S., Smrekar, M. and Barbosa, E. (2015). Extending a double
sampling control chart for non-conforming proportion in high quality
processes to the case of small samples.

## Author

Daniela R. Recchia, Emanuel P. Barbosa

## Examples

``` r

# Published example from Joekes et al. (2015)
res <- dsnp_prob_accept(0.005, n1 = 34, n2 = 162,
                        wl = 1.5, ucl1 = 2.5, ucl2 = 4.5)
res$pt
#> [1] 0.9987553
res$p_signal
#> [1] 0.001244692
```
