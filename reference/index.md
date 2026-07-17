# Package index

## Package overview

- [`IQCC-package`](https://flaviobarros.github.io/IQCC/reference/IQCC-package.md)
  [`IQCC`](https://flaviobarros.github.io/IQCC/reference/IQCC-package.md)
  : IQCC: Improved Quality Control Charts

## Univariate location and dispersion

Shewhart charts and distribution-aware limits for continuous
measurements.

- [`cchart.Xbar()`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar.md)
  : X-bar Control Chart for phase I and II.
- [`cchart.Xbar1()`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar1.md)
  : X-bar Shewhart Control Chart for phase I.
- [`cchart.Xbar2()`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar2.md)
  : X-bar Shewhart Control Chart for phase II.
- [`cchart.Xbar_R()`](https://flaviobarros.github.io/IQCC/reference/cchart.Xbar_R.md)
  : X-bar and R control charts
- [`cchart.R()`](https://flaviobarros.github.io/IQCC/reference/cchart.R.md)
  : Range Control Chart
- [`r_shewhart_limits()`](https://flaviobarros.github.io/IQCC/reference/r_shewhart_limits.md)
  : Conventional Three-Sigma Limits for the R Chart
- [`r_exact_limits()`](https://flaviobarros.github.io/IQCC/reference/r_exact_limits.md)
  : Exact Probability Limits for the R Chart
- [`cchart.S()`](https://flaviobarros.github.io/IQCC/reference/cchart.S.md)
  : Standard-Deviation Control Chart
- [`alpha.risk()`](https://flaviobarros.github.io/IQCC/reference/alpha.risk.md)
  : False-Alarm Probability for the Three-Sigma R Chart
- [`c4()`](https://flaviobarros.github.io/IQCC/reference/c4.md) : Bias
  Correction Constant for Sample Standard Deviation
- [`d2()`](https://flaviobarros.github.io/IQCC/reference/d2.md) : Mean
  of the Relative Range
- [`d3()`](https://flaviobarros.github.io/IQCC/reference/d3.md) :
  Standard Deviation of the Relative Range

## Attribute control charts

Limits and false-alarm calculations for binomial and Poisson monitoring.

- [`pchart_limits()`](https://flaviobarros.github.io/IQCC/reference/pchart_limits.md)
  : Probability Limits for p Charts
- [`pchart_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/pchart_alpha_risk.md)
  : Exact False-Alarm Risk for p-Chart Limits
- [`cchart.p()`](https://flaviobarros.github.io/IQCC/reference/cchart.p.md)
  : p-chart
- [`uchart_limits()`](https://flaviobarros.github.io/IQCC/reference/uchart_limits.md)
  : Probability Limits for u Charts
- [`uchart_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/uchart_alpha_risk.md)
  : Exact False-Alarm Risk for u-Chart Limits
- [`cchart.u()`](https://flaviobarros.github.io/IQCC/reference/cchart.u.md)
  : u-chart

## Double-sampling np charts

Exact-binomial design, performance evaluation, and operational
monitoring.

- [`dsnp_prob_accept()`](https://flaviobarros.github.io/IQCC/reference/dsnp_prob_accept.md)
  : Double-Sampling np Chart: Acceptance Probability
- [`dsnp_arl()`](https://flaviobarros.github.io/IQCC/reference/dsnp_arl.md)
  : Double-Sampling np Chart: Average Run Length
- [`dsnp_ass()`](https://flaviobarros.github.io/IQCC/reference/dsnp_ass.md)
  : Double-Sampling np Chart: Average Sample Size
- [`dsnp_limits()`](https://flaviobarros.github.io/IQCC/reference/dsnp_limits.md)
  : Double-Sampling np Chart: Limit Search
- [`dsnp_design()`](https://flaviobarros.github.io/IQCC/reference/dsnp_design.md)
  [`print(`*`<dsnp_design>`*`)`](https://flaviobarros.github.io/IQCC/reference/dsnp_design.md)
  : Double-Sampling np Chart: Complete Design Search
- [`cchart.DSnp()`](https://flaviobarros.github.io/IQCC/reference/cchart.DSnp.md)
  : Double-Sampling np Control Chart
- [`print(`*`<cchart.DSnp>`*`)`](https://flaviobarros.github.io/IQCC/reference/print.cchart.DSnp.md)
  [`summary(`*`<cchart.DSnp>`*`)`](https://flaviobarros.github.io/IQCC/reference/print.cchart.DSnp.md)
  [`print(`*`<summary.cchart.DSnp>`*`)`](https://flaviobarros.github.io/IQCC/reference/print.cchart.DSnp.md)
  : Print and Summarize a Double-Sampling np Control Chart
- [`plot(`*`<cchart.DSnp>`*`)`](https://flaviobarros.github.io/IQCC/reference/plot.cchart.DSnp.md)
  : Plot a DS-np Control Chart

## Multivariate monitoring

Hotelling T-squared and generalized variance charts.

- [`T2.1()`](https://flaviobarros.github.io/IQCC/reference/T2.1.md) :
  Hotelling T² Statistic for Phase I
- [`T2.2()`](https://flaviobarros.github.io/IQCC/reference/T2.2.md) :
  Hotelling T² Statistic for Phase II
- [`cchart.T2.1()`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.1.md)
  : Phase I Hotelling T² Control Chart
- [`cchart.T2.2()`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.2.md)
  : Phase II Hotelling T² Control Chart
- [`gv_stat()`](https://flaviobarros.github.io/IQCC/reference/gv_stat.md)
  : Generalized Variance by Subgroup
- [`gv_limits()`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md)
  : Generalized Variance Control Limits
- [`gv_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/gv_alpha_risk.md)
  : False-Alarm Risk for Generalized Variance Charts
- [`cchart.GV()`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md)
  [`plot(`*`<cchart.GV>`*`)`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md)
  : Generalized Variance Control Chart
- [`trv_stat()`](https://flaviobarros.github.io/IQCC/reference/trv_stat.md)
  : Trace Statistic for Multivariate Variability
- [`trv_limits()`](https://flaviobarros.github.io/IQCC/reference/trv_limits.md)
  : Trace-Statistic Control Limits
- [`trv_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/trv_alpha_risk.md)
  : False-Alarm Risk for Trace-Statistic Charts
- [`cchart.trV()`](https://flaviobarros.github.io/IQCC/reference/cchart.trV.md)
  [`plot(`*`<cchart.trV>`*`)`](https://flaviobarros.github.io/IQCC/reference/cchart.trV.md)
  [`print(`*`<cchart.trV>`*`)`](https://flaviobarros.github.io/IQCC/reference/cchart.trV.md)
  [`summary(`*`<cchart.trV>`*`)`](https://flaviobarros.github.io/IQCC/reference/cchart.trV.md)
  [`print(`*`<summary.cchart.trV>`*`)`](https://flaviobarros.github.io/IQCC/reference/cchart.trV.md)
  : Trace-Statistic Control Chart
- [`print(`*`<cchart.GV>`*`)`](https://flaviobarros.github.io/IQCC/reference/print.cchart.GV.md)
  [`summary(`*`<cchart.GV>`*`)`](https://flaviobarros.github.io/IQCC/reference/print.cchart.GV.md)
  [`print(`*`<summary.cchart.GV>`*`)`](https://flaviobarros.github.io/IQCC/reference/print.cchart.GV.md)
  : Print and Summarize a Generalized Variance Control Chart

## Data preparation and utilities

- [`data.1()`](https://flaviobarros.github.io/IQCC/reference/data.1.md)
  : Simulate Phase I Multivariate Normal Data
- [`data.2()`](https://flaviobarros.github.io/IQCC/reference/data.2.md)
  : Simulate Phase II Multivariate Normal Data
- [`add.data()`](https://flaviobarros.github.io/IQCC/reference/add.data.md)
  : Add a New Observation to a Phase II T² Chart
- [`remove.data()`](https://flaviobarros.github.io/IQCC/reference/remove.data.md)
  : Remove an Observation from Phase I Data
- [`stats()`](https://flaviobarros.github.io/IQCC/reference/stats.md) :
  Auxiliary Statistics for Hotelling T² Charts
- [`table.const()`](https://flaviobarros.github.io/IQCC/reference/table.const.md)
  : Table of Control Chart Constants d2, d3, and c4
- [`table.qtukey()`](https://flaviobarros.github.io/IQCC/reference/table.qtukey.md)
  : Tukey Quantile Table for the Relative Range

## Example datasets

- [`binomdata`](https://flaviobarros.github.io/IQCC/reference/binomdata.md)
  : Binomial Process Data for p-Charts
- [`moonroof`](https://flaviobarros.github.io/IQCC/reference/moonroof.md)
  : Moonroof Installation Defect Data
- [`pistonrings`](https://flaviobarros.github.io/IQCC/reference/pistonrings.md)
  : Piston Rings Diameter Data
- [`softdrink`](https://flaviobarros.github.io/IQCC/reference/softdrink.md)
  : Soft Drink Bottling Volume Data
