# Monitoring multivariate processes with IQCC

## Scope

Multivariate process monitoring separates two questions that are often
mixed in practice. Hotelling T-squared charts monitor shifts in a
process mean vector, while generalized-variance and trace charts monitor
changes in the covariance structure. IQCC contains all three workflows:

| Question | Statistic | Main functions |
|:---|:---|:---|
| Has the mean vector shifted? | Hotelling T-squared | [`T2.1()`](https://flaviobarros.github.io/IQCC/reference/T2.1.md), [`T2.2()`](https://flaviobarros.github.io/IQCC/reference/T2.2.md), [`cchart.T2.1()`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.1.md), [`cchart.T2.2()`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.2.md) |
| Has total generalized variance changed? | `|S|` | [`gv_stat()`](https://flaviobarros.github.io/IQCC/reference/gv_stat.md), [`gv_limits()`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md), [`gv_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/gv_alpha_risk.md), [`cchart.GV()`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md) |
| Has standardized covariance structure changed? | `tr(V)` | [`trv_stat()`](https://flaviobarros.github.io/IQCC/reference/trv_stat.md), [`trv_limits()`](https://flaviobarros.github.io/IQCC/reference/trv_limits.md), [`trv_alpha_risk()`](https://flaviobarros.github.io/IQCC/reference/trv_alpha_risk.md), [`cchart.trV()`](https://flaviobarros.github.io/IQCC/reference/cchart.trV.md) |

The covariance charts are complementary. The determinant `|S|`
summarizes the volume of the subgroup covariance matrix. The trace
statistic `(n - 1) tr(Sigma0^-1 S)` can change even when the determinant
is nearly unchanged.

## Input formats

The generalized-variance and trace-statistic functions use the
multivariate subgroup formats accepted by
[`gv_stat()`](https://flaviobarros.github.io/IQCC/reference/gv_stat.md)
and
[`cchart.GV()`](https://flaviobarros.github.io/IQCC/reference/cchart.GV.md):
a list of subgroup matrices, a subgroup-by-observation-by-variable
array, or a stacked matrix with the subgroup size supplied separately.
The examples below use a list because it makes the subgroup boundaries
explicit.

``` r

g1 <- cbind(
  x = c(-1, 0, 1, -1, 1),
  y = c(-1, 1, 0, 1, -1)
)
g2 <- sweep(g1, 2, c(0.2, -0.1), "+")
g3 <- g1 %*% matrix(c(1.1, 0.1, 0, 0.9), nrow = 2)
groups <- list(g1, g2, g3)

data.frame(
  subgroup = seq_along(groups),
  generalized_variance = gv_stat(groups),
  trace_statistic = trv_stat(groups, Sigma0 = diag(2))
)
#>   subgroup generalized_variance trace_statistic
#> 1        1            0.9375000             8.0
#> 2        2            0.9375000             8.0
#> 3        3            0.9188438             7.9
```

The legacy Hotelling helpers use a different array convention:
observation-by-variable-by-subgroup. Check
[`dim()`](https://rdrr.io/r/base/dim.html) before moving data between
the Hotelling helpers and the generalized-variance or trace workflows.

## Hotelling T-squared

The Hotelling workflow estimates Phase I location and covariance,
computes Phase I T-squared statistics, and then evaluates Phase II
observations against the Phase II reference distribution.

``` r

set.seed(2026)
phase1 <- data.1(m = 8, n = 5, mu = c(0, 0), Sigma = diag(2))
reference <- IQCC::stats(phase1, m = 8, n = 5, p = 2)
t2_phase1 <- T2.1(reference, m = 8, n = 5)

phase2 <- data.2(reference, n = 5, p = 2)
t2_phase2 <- T2.2(phase2, reference, n = 5)

data.frame(
  phase = c(rep("Phase I", length(t2_phase1)), "Phase II"),
  statistic = c(t2_phase1, t2_phase2)
)
#>      phase statistic
#> 1  Phase I 6.5124530
#> 2  Phase I 2.8083634
#> 3  Phase I 8.1145908
#> 4  Phase I 0.5432524
#> 5  Phase I 1.5197014
#> 6  Phase I 4.1999052
#> 7  Phase I 1.6098032
#> 8  Phase I 3.6227786
#> 9 Phase II 3.8565799
```

[`cchart.T2.1()`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.1.md)
and
[`cchart.T2.2()`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.2.md)
draw the two chart types. Their control limits are not interchangeable
because Phase I and Phase II use different reference distributions.

## Generalized variance

For each subgroup,
[`gv_stat()`](https://flaviobarros.github.io/IQCC/reference/gv_stat.md)
computes the determinant of the sample covariance matrix.
[`gv_limits()`](https://flaviobarros.github.io/IQCC/reference/gv_limits.md)
exposes normal, Cornish-Fisher, selected exact, and simulation-based
limits. The exact implementation is deliberately scoped; generic
Meijer-G quantiles are not claimed by the package.

``` r

gv_methods <- do.call(
  rbind,
  lapply(c("normal", "cf", "exact"), function(method) {
    limits <- gv_limits(n = 5, p = 2, det_sigma = 1, type = method)
    data.frame(
      method = method,
      center = limits$center,
      lcl = limits$lcl,
      ucl = limits$ucl
    )
  })
)

gv_sim <- gv_limits(
  n = 5,
  p = 2,
  det_sigma = 1,
  type = "simulation",
  nsim = 2000,
  seed = 2026
)

gv_methods
#>   method center lcl      ucl
#> 1 normal   0.75   0 3.305568
#> 2     cf   0.75   0 6.769365
#> 3  exact   0.75   0 6.288749
gv_sim[c("type", "ucl", "nsim", "seed")]
#> $type
#> [1] "simulation"
#> 
#> $ucl
#> [1] 5.379544
#> 
#> $nsim
#> [1] 2000
#> 
#> $seed
#> [1] 2026
```

The chart wrapper records the phase split, limits, covariance source,
and signaled subgroups. Use
[`summary()`](https://rdrr.io/r/base/summary.html) to inspect the fitted
object instead of reading values from a plot.

``` r

gv_chart <- cchart.GV(
  x1 = groups[1:2],
  x2 = groups[3],
  type = "cf",
  plot = FALSE
)
summary(gv_chart)
#> Generalized Variance Control Chart
#>   Dimension: p = 2 ; subgroup size n = 5 
#>   Subgroups: 3 (Phase I: 2 ; Phase II: 1 )
#>   Limits: cf / upper ; nominal alpha = 0.0027 
#>   Covariance: estimated from Phase I subgroups 
#>   LCL = 0 ; center = 0.8036 ; UCL = 7.253 
#>   Signals: 0
```

If `Sigma` is omitted, IQCC uses Phase I covariance information as a
plug-in reference. The nominal known-parameter calibration does not
include the extra uncertainty introduced by estimating the reference
from a finite Phase I sample.

## Auxiliary trace chart

The trace statistic is

``` math
T = (n - 1)\operatorname{tr}(\Sigma_0^{-1} S).
```

Under multivariate normality and known in-control covariance `Sigma0`,
`T` has a chi-square distribution with `p * (n - 1)` degrees of freedom.

``` r

trv_limits(n = 5, p = 2, alpha = 0.0027)
#> $lcl
#> [1] 0
#> 
#> $ucl
#> [1] 23.57439
#> 
#> $center
#> [1] 8
#> 
#> $type
#> [1] "chisq"
#> 
#> $alpha
#> [1] 0.0027
#> 
#> $n
#> [1] 5
#> 
#> $p
#> [1] 2
#> 
#> $df
#> [1] 8
#> 
#> $nsim
#> [1] 1e+05
#> 
#> $seed
#> NULL
trv_alpha_risk(n = 5, p = 2, ucl = trv_limits(5, 2)$ucl)
#> $alpha
#> [1] 0.0027
#> 
#> $arl0
#> [1] 370.3704
#> 
#> $ucl
#> [1] 23.57439
#> 
#> $n
#> [1] 5
#> 
#> $p
#> [1] 2
#> 
#> $df
#> [1] 8
#> 
#> $method
#> [1] "exact chi-square"
```

The trace chart is most useful beside `|S|`, not as a replacement for
it. The following deterministic example has two covariance matrices with
the same determinant but different standardized trace.

``` r

base <- sqrt(3 / 4) * rbind(
  c(1, 1), c(1, -1), c(-1, 1), c(-1, -1)
)
same_volume <- list(
  base %*% chol(diag(2)),
  base %*% chol(diag(c(4, 0.25)))
)

data.frame(
  generalized_variance = gv_stat(same_volume),
  trace_statistic = trv_stat(same_volume, Sigma0 = diag(2))
)
#>   generalized_variance trace_statistic
#> 1                    1            6.00
#> 2                    1           12.75
```

``` r

trv_chart <- cchart.trV(
  x = same_volume[1],
  newdata = same_volume[2],
  Sigma0 = diag(2),
  alpha = 0.05,
  type = "chisq",
  plot = FALSE
)
summary(trv_chart)
#> Trace-Statistic Control Chart
#>   Dimension: p = 2 ; subgroup size n = 4 
#>   Subgroups: 2 (Phase I: 1 ; Phase II: 1 )
#>   Limits: chisq ; nominal alpha = 0.05 ; df = 6 
#>   Covariance: supplied Sigma0 
#>   Center = 6 ; UCL = 12.59 
#>   Signals: 1 
#>  index phase statistic
#>      2    II     12.75
```

The published generalized-variance report contains a Case B where the
printed summary matrices keep determinants close while the trace chart
signals in Phase II. IQCC reproduces the printed determinants and the
published trace UCL, but the full signal sequence cannot be recomputed
because the report does not print all simulated subgroups or the random
seed.

## Practical sequence

1.  Use Hotelling T-squared when the primary question is a shift in the
    mean vector.
2.  Use `|S|` when the primary question is a change in generalized
    variance or covariance volume.
3.  Add `tr(V)` when covariance structure may change without a large
    determinant change.
4.  State whether covariance references are known parameters or Phase I
    plug-in estimates.
5.  Use simulated limits as Monte Carlo approximations and report
    `nsim`, `seed`, and simulation error when they support a scientific
    claim.

``` r

cat("<!-- IQCC_EXECUTED_MULTIVARIATE_MONITORING -->\n")
```
