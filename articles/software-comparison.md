# IQCC in the R SPC Ecosystem: A Comparison

## Introduction

The R environment hosts a mature collection of statistical process
control (SPC) packages, each with distinct design goals, methodological
coverage, and target audiences. General-purpose packages such as `qcc`
and `qcr` provide broad charting workflows for Shewhart variables and
attribute data, while specialized packages address areas like average
run length computation, healthcare charting, or Six Sigma project
frameworks. Choosing among them depends on the monitoring statistic, the
distributional assumptions one is willing to make, and whether the
primary concern is producing a chart or evaluating whether the chart’s
limits are statistically well calibrated.

IQCC enters this landscape with a focused mandate: provide exact or
distributionally corrected control chart limits where classical
three-sigma or normal approximations are known to be poor. The package
does not attempt to replicate every chart type available elsewhere.
Instead, it targets specific pain points – bounded statistics, discrete
counts with small denominators, rare nonconformities, asymmetric
dispersion measures, and multivariate variability – where conventional
SPC software either does not offer an alternative to the normal
approximation or buries the numerical design behind a plotting function.

This vignette surveys the major R SPC packages, compares their features
across a common set of dimensions, and identifies the conditions under
which IQCC adds value beyond what the broader ecosystem provides. The
goal is to help practitioners decide when IQCC is the right tool and
when a general-purpose package is sufficient.

## Comparable Packages

**`qcc`** (v2.7, last CRAN update 2017) is the most widely known SPC
package in R. It implements Shewhart variables charts (xbar, R, S),
attribute charts (p, np, c, u), CUSUM, EWMA, and a basic form of
Hotelling T-squared. The package offers an integrated plotting system
based on `stats::qcc` objects and has served as the foundation for
several downstream packages. Its limits are exclusively three-sigma
normal approximations.

**`shewhartr`** (v1.3.0, last CRAN update 2026) is a more recent and
methodologically ambitious package that covers Shewhart variables
charts, EWMA, CUSUM, Hotelling T-squared, MCUSUM, MEWMA, and
regression-adjusted charts. It provides both three-sigma and exact
Poisson limits for count data and explicitly separates Phase I
calibration via `calibrate()` and Phase II monitoring via `monitor()`.
The package ships seven vignettes.

**`qcr`** (v1.4, last CRAN update 2022) extends the SPC ecosystem beyond
parametric charts by including Shewhart, EWMA, CUSUM, Hotelling
T-squared, nonparametric depth-based control charts, and functional data
monitoring. Its breadth is notable, but the package has no vignettes to
guide users through its extensive functionality.

**`spc`** (v0.7.2, last CRAN update 2025) is a pure numerical package
that computes average run lengths (ARL) for a wide range of control
charts using integral equations. It does not draw charts, does not
provide limit-finding functions for practitioners, and ships no
vignettes. Its value lies in the research community that studies chart
performance rather than in routine monitoring.

**`qicharts2`** (v0.8.1, last CRAN update 2025) focuses on healthcare
quality improvement and provides I, MR, Xbar, S, T, C, U, P, and G
charts. It uses three-sigma limits alongside Anhoej decision rules and
includes one vignette oriented toward clinical settings.

**`SixSigma`** (v0.11.1, last CRAN update 2023) provides a DMAIC
(Define, Measure, Analyze, Improve, Control) project framework with
basic control charting through `ss.cc()`. The package is oriented toward
Six Sigma practitioners rather than methodological comparison or limit
calibration.

**`IQCC`** (v0.7, active development 2026) provides Xbar, R, and S
charts with exact distributional limits; p, np, c, and u charts with
Cornish-Fisher corrected limits; Hotelling T-squared with asymptotically
robust limits; generalized variance \|S\| limits; trace tr(V) limits;
and the DS-np double-sampling plan, which is unique among R SPC
packages. It separates pure numerical kernels from plotting wrappers,
ships six vignettes and a full pkgdown site, and remains under active
development.

## Feature Comparison

| Feature | qcc | shewhartr | qcr | spc | qicharts2 | SixSigma | IQCC |
|:---|:---|:---|:---|:---|:---|:---|:---|
| Shewhart variables (R, S, xbar) | Yes | Yes | Yes | No | Yes | Yes | Yes |
| Attribute charts (p, np, c, u) | Yes | Yes | Yes | No | Yes | Yes | Yes |
| EWMA / CUSUM | Yes | Yes | Yes | ARL only | No | No | No |
| Exact distributional limits | No | Poisson only | No | No | No | No | Yes |
| CF-corrected limits | No | No | No | No | No | No | Yes |
| Phase I / Phase II separation | No | Yes | No | No | No | No | T² only |
| DS-np double-sampling | No | No | No | No | No | No | Yes |
| Rare defects charts | No | No | No | No | G chart | No | Yes |
| Multivariate T-squared | Basic | Yes | Yes | No | No | No | Yes |
| Multivariate | S | , tr(V) | No | No | No | No | No |
| ARL evaluation | No | No | No | Yes | No | No | Yes |
| Pure numerical kernels | No | Partial | No | Yes | No | No | Yes |
| Number of vignettes | 1 | 7 | 0 | 0 | 1 | 0 | 6 |
| Last CRAN update | 2017 | 2026 | 2022 | 2025 | 2025 | 2023 | active |

### Limit Methodology

The most important distinction among SPC packages is how each computes
its control limits. Every package listed above can produce a control
chart, but the statistical properties of those limits differ
dramatically. `qcc` and `qcr` use three-sigma normal limits throughout:
the upper and lower control limits are placed at three standard
deviations above and below the center line, regardless of the underlying
distribution. For large subgroup sizes and approximately normal data,
this approach is adequate. For small subgroups, bounded proportions,
rare counts, or skewed distributions, the actual false-alarm probability
can deviate substantially from the nominal 0.0027.

`shewhartr` improves on this by offering exact Poisson limits for c and
u charts. IQCC goes further by providing exact limits for R and S charts
(based on the relative range distribution and the chi-square
distribution, respectively), exact generalized variance limits in
dimension two, and Cornish-Fisher corrected limits for p, np, c, and u
charts. The Cornish-Fisher expansion adjusts the normal quantile using
the skewness and kurtosis of the binomial or Poisson distribution,
producing asymmetric limits that reflect the true shape of the
monitoring statistic. IQCC also reports the actual false-alarm
probability of any set of limits via
[`pchart_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/pchart_alpha_risk.md)
and
[`uchart_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/uchart_alpha_risk.md),
enabling the practitioner to evaluate the calibration of either IQCC or
third-party limits.

### High-Quality Processes and Rare Defects

Conventional attribute charts perform poorly when the process proportion
of nonconforming items is very small. A p chart with p = 0.001 and n =
100 has a lower control limit that is negative under the normal
approximation, and the actual false-alarm probability of the three-sigma
upper limit may be an order of magnitude smaller than intended. IQCC
addresses this regime in two ways. First, the Cornish-Fisher correction
produces asymmetric limits that remain non-negative and better reflect
the binomial tail. Second, the DS-np double-sampling plan offers a
specialized design for high-quality processes in which inspection is
costly: a small first sample is inspected, and a second sample is drawn
only when the first count falls in a warning zone. This reduces the
average sample size under the in-control state while maintaining power
against moderate shifts. No other R SPC package implements a
double-sampling attribute plan.

`qicharts2` offers a G chart for geometric monitoring of rare events,
which captures a different use case (time-between-events rather than
counts per sample). IQCC and `qicharts2` are therefore complementary:
the G chart suits continuous monitoring of interarrival times, while
IQCC’s DS-np and CF-corrected p charts suit attribute sampling with
fixed subgroup sizes.

### Multivariate Monitoring

Multivariate SPC is an area of sharp differentiation among the packages.
`qcc` provides a basic Hotelling T-squared chart with three-sigma
limits. `shewhartr` provides Hotelling T-squared, MCUSUM, and MEWMA with
Phase I and Phase II separation. `qcr` extends to nonparametric
depth-based multivariate charts.

IQCC provides Hotelling T-squared with asymptotically robust limits, but
its distinctive contribution is in multivariate variability monitoring.
The generalized variance \|S\| chart detects changes in the determinant
of the covariance matrix, and the trace statistic tr(V) chart detects
changes in the trace of the scaled covariance matrix. These two
statistics capture different aspects of covariance structure: \|S\| is
sensitive to changes in the volume of the covariance ellipsoid, while
tr(V) is sensitive to changes in the average variance across variables.
Both statistics have exact or asymptotically justified limits in IQCC,
and both are unavailable in the other packages surveyed. For a
practitioner monitoring multivariate dispersion in addition to
multivariate location, IQCC fills a gap that no other R package
currently addresses.

### Software Architecture

The SPC packages differ substantially in how they structure computation
and plotting. `qcc` integrates both into a single S3 object with a
unified `qcc()` function. `qcr` follows a similar monolithic design.
This approach is convenient for quick use but obscures the numerical
details of limit calculation and makes it difficult to reuse the
statistical kernel without the plotting machinery.

IQCC adopts a deliberately separated architecture. Pure numerical
functions such as
[`pchart_limits()`](https://flaviobarros.github.io/IQCC/reference/pchart_limits.md),
[`gv_limits()`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md),
[`dsnp_arl()`](https://flaviobarros.github.io/IQCC/reference/dsnp_arl.md),
and
[`trv_limits()`](https://flaviobarros.github.io/IQCC/reference/trv_limits.md)
compute limits or operating characteristics without producing any
graphical output. Chart wrappers such as
[`cchart.p()`](https://flaviobarros.github.io/IQCC/reference/cchart.p.md),
[`cchart.GV()`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md),
and
[`cchart.DSnp()`](https://flaviobarros.github.io/IQCC/reference/cchart.DSnp.md)
call these numerical kernels and pass the results to `qcc` for plotting.
This separation allows the practitioner to inspect the limits and their
diagnostic properties before committing to a chart, and it enables
programmatic use of IQCC’s numerical methods in simulation studies or
automated reporting pipelines.

`shewhartr` separates Phase I and Phase II monitoring through distinct
`calibrate()` and `monitor()` functions, a design that parallels IQCC’s
separation but at the workflow level rather than at the
computation-versus-plotting level. `spc` operates entirely at the
numerical kernel level with no charting at all, making it IQCC’s closest
analogue in terms of pure computation but completely lacking any data
analysis workflow.

### Documentation

Documentation depth varies widely across the ecosystem. `qcc` ships one
vignette that covers the basic chart types. `qcr` ships no vignettes
despite its broad functionality, leaving users to navigate the help
pages unaided. `qicharts2` ships one vignette focused on healthcare
applications. `SixSigma` and `spc` ship no vignettes.

`shewhartr` ships seven vignettes, making it the most thoroughly
documented package among IQCC’s peers. IQCC ships six vignettes covering
getting started, positioning, statistical foundations, high-quality
processes, univariate dispersion monitoring, and multivariate
monitoring, supplemented by a full pkgdown site with cross-referenced
articles and function references.

## When to Choose IQCC

IQCC is the appropriate choice when the monitoring statistic has a
bounded or discrete support, when subgroup sizes are small enough to
make normal approximations unreliable, when the process proportion or
defect rate is very low, or when the goal is to monitor multivariate
covariance structure rather than location alone. Specific scenarios
include: attribute charting with p \< 0.05 or small n, where the normal
approximation produces negative limits or inaccurate false-alarm
probabilities; R and S charting with n \< 10, where the distribution of
the range or standard deviation is markedly non-normal; double-sampling
plans for high-quality processes where reducing inspection cost matters;
and multivariate dispersion monitoring with the generalized variance or
trace statistic.

IQCC is also the right choice when the practitioner needs to evaluate
the actual false-alarm probability of a set of control limits rather
than accepting the nominal 0.0027 value. The
[`pchart_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/pchart_alpha_risk.md)
and
[`uchart_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/uchart_alpha_risk.md)
functions compute exact binomial or Poisson probabilities for any
supplied limits, making them useful as an audit tool even when the
limits themselves come from another package.

## Limitations of IQCC

IQCC does not implement EWMA or CUSUM charts, which are available in
`qcc`, `shewhartr`, and `qcr`. For detecting small persistent shifts in
location, these chart types are often more powerful than Shewhart
charts, and the practitioner should use one of the general-purpose
packages for that purpose.

IQCC does not offer nonparametric control charts of the kind provided by
`qcr`’s depth-based methods. The package is entirely parametric in its
current design. IQCC does not support functional data monitoring,
regression-adjusted charts, or MCUSUM and MEWMA multivariate location
schemes. Its multivariate coverage is limited to Hotelling T-squared,
generalized variance, and the trace statistic.

IQCC’s exact distributional results are available only for specific
combinations of statistic and dimension. Exact R chart limits are based
on the relative range distribution; exact S chart limits are based on
the chi-square distribution; exact generalized variance limits are
available in dimension two. For higher dimensions, IQCC falls back to
Cornish-Fisher corrections or simulation. The practitioner should
consult the function documentation to determine whether an exact method
exists for a given chart and parameter set.

IQCC remains under active development on GitHub but does not have the
user base or CRAN download volume of established packages such as `qcc`
or `shewhartr`. Community support, third-party extensions, and
integration with other workflows are correspondingly more limited.

## References

Scrucca, L. (2004). qcc: an R package for quality control charting and
statistical process control. *R News*, 4(1), 11-17.

Cano, E. L., Moguerza, J. M., and Corcoba, M. P. (2015). *Quality
Control with R*. Springer.

Santos-Fernandez, E. (2013). *Multivariate Statistical Quality Control
Using R*. Springer.

Flores, M., Naya, S., Fernandez-Casal, R., Zaragoza, S., Roca-Pardinas,
J., and Oviedo de la Fuente, M. (2022). qcr: quality control review. R
package version 1.4.

Knoth, S. (2025). spc: statistical process control – collection of some
SPC tools. R package version 0.7.2.

Jacob, A. (2025). qicharts2: quality improvement charts. R package
version 0.8.1.

Canovas, L. (2023). SixSigma: Six Sigma tools for quality improvement. R
package version 0.11.1.

Flores, M., Fernandez-Casal, R., and Oviedo de la Fuente, M. (2026).
shewhartr: Shewhart control charts. R package version 1.3.0.
