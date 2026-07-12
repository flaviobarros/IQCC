# When to use IQCC: exact and corrected control charts

## Purpose

IQCC implements *Improved Quality Control Charts*: control charts with
exact, corrected, standardized, or simulation-based limits for
situations where classical Shewhart-type approximations may be poorly
calibrated.

The package is not intended to replace general-purpose statistical
process control packages. Instead, it is useful when the monitoring
statistic is bounded, discrete, skewed, strongly non-normal, or based on
small samples. In those settings, the usual three-sigma limits can
produce a false-alarm probability that differs meaningfully from the
nominal value.

## Positioning in the R ecosystem

A practical way to position IQCC is:

> IQCC is a specialized package for exact and corrected control-chart
> limits, especially for small samples, rare nonconformities, asymmetric
> statistics, and selected multivariate monitoring problems.

Use a general SPC package when the goal is a broad control-chart
workflow. Use IQCC when the main concern is whether classical limits are
statistically well calibrated for the distribution and sample size at
hand.

## When IQCC is especially useful

| Monitoring problem | Classical difficulty | IQCC direction |
|----|----|----|
| Range-based dispersion monitoring | Normal approximations can inflate the actual false-alarm risk, especially for small subgroups. | Use exact range-chart limits based on the relative range distribution. |
| Standard deviation monitoring | The distribution of the sample standard deviation is asymmetric for small samples. | Use exact limits derived from the chi-square distribution of the sample variance. |
| Nonconforming proportion in high-quality processes | The binomial distribution is discrete, bounded, and strongly skewed when the nonconforming proportion is very small. | Use CF1 or CF2 limits, exact binomial risk evaluation, or a double-sampling np chart. |
| Nonconformities per unit | Poisson counts are discrete and skewed when the expected count is small. | Use CF1 or CF2 limits and exact Poisson risk evaluation. |
| Multivariate process mean monitoring | Hotelling T-squared requires phase-specific calibration. | Use the implemented Phase I and Phase II Hotelling T-squared functions. |
| Multivariate process variability | A normal approximation for generalized variance may be poorly calibrated. | Use normal, Cornish-Fisher, selected exact, or simulation-based generalized variance limits. |

## Current implemented methods

The current package includes several families of functions:

- [`cchart.R()`](https://flaviobarros.github.io/IQCC/reference/cchart.R.md)
  for range charts, including exact Tukey-based limits.
- [`cchart.S()`](https://flaviobarros.github.io/IQCC/reference/cchart.S.md)
  for standard deviation charts, including exact chi-square-based
  limits.
- [`pchart_limits()`](https://flaviobarros.github.io/IQCC/reference/pchart_limits.md),
  [`pchart_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/pchart_alpha_risk.md),
  and
  [`cchart.p()`](https://flaviobarros.github.io/IQCC/reference/cchart.p.md)
  for normal, CF1, CF2, and standardized p charts.
- [`uchart_limits()`](https://flaviobarros.github.io/IQCC/reference/uchart_limits.md),
  [`uchart_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/uchart_alpha_risk.md),
  and
  [`cchart.u()`](https://flaviobarros.github.io/IQCC/reference/cchart.u.md)
  for normal, CF1, CF2, and standardized u charts.
- [`dsnp_prob_accept()`](https://flaviobarros.github.io/IQCC/reference/dsnp_prob_accept.md),
  [`dsnp_arl()`](https://flaviobarros.github.io/IQCC/reference/dsnp_arl.md),
  [`dsnp_ass()`](https://flaviobarros.github.io/IQCC/reference/dsnp_ass.md),
  [`dsnp_limits()`](https://flaviobarros.github.io/IQCC/reference/dsnp_limits.md),
  and
  [`cchart.DSnp()`](https://flaviobarros.github.io/IQCC/reference/cchart.DSnp.md)
  for double-sampling np design and monitoring.
- [`T2.1()`](https://flaviobarros.github.io/IQCC/reference/T2.1.md),
  [`T2.2()`](https://flaviobarros.github.io/IQCC/reference/T2.2.md),
  [`cchart.T2.1()`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.1.md),
  and
  [`cchart.T2.2()`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.2.md)
  for Hotelling T-squared monitoring.
- [`gv_stat()`](https://flaviobarros.github.io/IQCC/reference/gv_stat.md),
  [`gv_limits()`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md),
  [`gv_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/gv_alpha_risk.md),
  and
  [`cchart.GV()`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md)
  for generalized variance monitoring.
- [`alpha.risk()`](https://flaviobarros.github.io/IQCC/reference/alpha.risk.md)
  for studying the actual false-alarm risk of classical range charts.

## Example: using a corrected p chart

The example below constructs a p chart using the explicit two-adjustment
Cornish-Fisher method.

``` r

library(IQCC)

data(binomdata)

cchart.p(
  x1 = binomdata$Di[1:12],
  n1 = binomdata$ni[1:12],
  type = "cf2",
  x2 = binomdata$Di[13:25],
  n2 = binomdata$ni[13:25]
)
```

Use the explicit names `"cf1"` and `"cf2"` in new code. The historical
alias `"CF"` is retained for compatibility, but it maps to `"cf1"` in
[`cchart.p()`](https://flaviobarros.github.io/IQCC/reference/cchart.p.md)
and to `"cf2"` in
[`cchart.u()`](https://flaviobarros.github.io/IQCC/reference/cchart.u.md).

## Example: DS-np chart with automatic limit search

The double-sampling np chart uses two sampling stages to monitor the
nonconforming proportion in high-quality processes.
[`dsnp_limits()`](https://flaviobarros.github.io/IQCC/reference/dsnp_limits.md)
searches for feasible fractional limits, and
[`cchart.DSnp()`](https://flaviobarros.github.io/IQCC/reference/cchart.DSnp.md)
builds and plots the chart.

``` r

library(IQCC)

lim <- dsnp_limits(
  p0 = 0.005,
  n1 = 34,
  n2 = 162,
  alpha = 0.0027,
  p1 = 0.0075
)
lim$best[, c("wl", "ucl1", "ucl2", "arl0", "arl1", "ass0")]

x1 <- c(0, 0, 1, 0, 0, 1, 0, 0, 0, 2, 0, 0, 1, 0, 0)
x2 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, NA)

chart <- cchart.DSnp(
  x1,
  n1 = 34,
  n2 = 162,
  p0 = 0.005,
  x2 = x2,
  limits = lim
)
chart$performance
```

## Example: generalized variance limits

``` r

gv_limits(
  n = 10,
  p = 2,
  det_sigma = 0.5320,
  type = "exact"
)
```

Exact limits are available for dimension two and for selected published
upper-limit cases in dimension three. Simulation is available for more
general higher-dimensional cases.

## Development principles

Future extensions should follow three principles.

First, numerical calculations should be separated from plotting
functions. A limit, probability, average run length, or false-alarm risk
shown in a chart should be reproducible by a small numerical function.

Second, new methods should be validated against published tables or
examples when possible. This is particularly important for charts based
on exact or corrected distributional calculations.

Third, IQCC should remain complementary to the broader R SPC ecosystem.
Its strongest role is to provide statistically calibrated alternatives
for cases where classical approximations are simple but inaccurate.

## Related roadmap items

The principal remaining research and documentation extensions are:

- an auxiliary trace chart for complementary multivariate variability
  monitoring;
- full DS-np joint optimization over sample sizes and limits;
- generic exact generalized variance quantiles beyond the explicitly
  supported cases;
- a centralized catalogue of published validation fixtures;
- expanded executable vignettes and replication materials for a future
  article.

These items are tracked in the GitHub issue roadmap and should be
implemented in small, reviewable pull requests.
