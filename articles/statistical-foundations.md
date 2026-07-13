# Statistical Foundations of IQCC

## Overview

IQCC focuses on control charts whose classical normal limits may be
poorly calibrated because the monitored statistic is discrete, skewed,
bounded, or strongly non-normal. The package combines three ideas:

1.  compute limits on the original measurement scale;
2.  evaluate actual false-alarm probabilities whenever possible;
3.  validate formulas against independent calculations and published
    examples.

## Binomial p chart

For

``` math
X\sim\operatorname{Binomial}(n,p),\qquad \hat p=X/n,
```

``` math
E(\hat p)=p,\qquad
\operatorname{Var}(\hat p)=\frac{p(1-p)}{n}.
```

The normal limits use

``` math
p\pm z\sqrt{\frac{p(1-p)}{n}}.
```

The first Cornish-Fisher correction adds

``` math
\frac{(z^2-1)(1-2p)}{6n}
```

to the normal quantile on the original p scale.

``` r

pchart_table <- do.call(
  rbind,
  lapply(c("normal", "cf1", "cf2"), function(method) {
    lim <- pchart_limits(p = 0.015, n = 20, type = method)
    risk <- pchart_alpha_risk(
      p = 0.015,
      n = 20,
      lcl = lim$lcl,
      ucl = lim$ucl
    )
    data.frame(
      method = method,
      lcl = lim$lcl,
      center = lim$center,
      ucl = lim$ucl,
      alpha = risk,
      arl0 = ifelse(risk == 0, Inf, 1 / risk)
    )
  })
)
pchart_table
#>   method lcl center        ucl        alpha       arl0
#> 1 normal   0  0.015 0.09653924 0.0357458712   27.97526
#> 2    cf1   0  0.015 0.16120479 0.0002023458 4942.03542
#> 3    cf2   0  0.015 0.13031923 0.0031780828  314.65511
```

For unequal subgroup sizes, IQCC estimates the in-control proportion
using the pooled estimator

``` math
\hat p_0=\frac{\sum_i X_i}{\sum_i n_i}.
```

## Poisson u chart

Let

``` math
X\sim\operatorname{Poisson}(\lambda n),\qquad U=X/n.
```

Then

``` math
E(U)=\lambda,\qquad
\operatorname{Var}(U)=\frac{\lambda}{n}.
```

``` r

u_table <- do.call(
  rbind,
  lapply(c("normal", "cf1", "cf2"), function(method) {
    lim <- uchart_limits(lambda = 0.15, n = 20, type = method)
    risk <- uchart_alpha_risk(
      lambda = 0.15,
      n = 20,
      lcl = lim$lcl,
      ucl = lim$ucl
    )
    data.frame(
      method = method,
      lcl = lim$lcl,
      center = lim$center,
      ucl = lim$ucl,
      alpha = risk,
      arl0 = ifelse(risk == 0, Inf, 1 / risk)
    )
  })
)
u_table
#>   method lcl center       ucl       alpha     arl0
#> 1 normal   0   0.15 0.4098056 0.003802992 262.9509
#> 2    cf1   0   0.15 0.4764711 0.001102488 907.0392
#> 3    cf2   0   0.15 0.4668489 0.001102488 907.0392
```

## Double-sampling np chart

Let

``` math
D_1\sim\operatorname{Binomial}(n_1,p),\qquad
D_2\sim\operatorname{Binomial}(n_2,p),
```

independently. Define

``` math
a=\lfloor WL\rfloor,\qquad
b=\lfloor UCL_1\rfloor+1,\qquad
c=\lfloor UCL_2\rfloor.
```

The decision rule is:

- accept at stage 1 when $`D_1\leq a`$;
- signal at stage 1 when $`D_1\geq b`$;
- continue when $`a<D_1<b`$;
- accept at stage 2 when $`D_1+D_2\leq c`$.

Thus

``` math
ARL(p)=\frac{1}{1-P_T(p)},
\qquad
ASS(p)=n_1+n_2P_2(p).
```

``` r

prob <- dsnp_prob_accept(
  p = 0.005,
  n1 = 34,
  n2 = 162,
  wl = 1.5,
  ucl1 = 2.5,
  ucl2 = 4.5
)

data.frame(
  pt = prob$pt,
  p_signal = prob$p_signal,
  arl0 = dsnp_arl(0.005, 34, 162, 1.5, 2.5, 4.5)$arl,
  arl1 = dsnp_arl(0.0075, 34, 162, 1.5, 2.5, 4.5)$arl,
  ass0 = dsnp_ass(0.005, 34, 162, 1.5, 2.5)$ass
)
#>          pt    p_signal     arl0     arl1     ass0
#> 1 0.9987553 0.001244692 803.4114 193.2229 35.93534
```

## Generalized variance chart

Let $`S`$ be the sample covariance matrix from a p-variate normal
subgroup of size $`n`$. Then

``` math
(n-1)S\sim W_p(n-1,\Sigma).
```

The monitored statistic is

``` math
G=|S|.
```

Bartlett’s decomposition gives

``` math
G\overset{d}{=}
\frac{|\Sigma|}{(n-1)^p}
\prod_{j=1}^{p}Y_j,
\qquad
Y_j\sim\chi^2_{n-j},
```

with independent factors.

``` r

gv_table <- do.call(
  rbind,
  lapply(c("normal", "cf", "exact"), function(method) {
    lim <- gv_limits(
      n = 10,
      p = 2,
      det_sigma = 0.5320,
      type = method
    )
    risk <- gv_alpha_risk(
      n = 10,
      p = 2,
      det_sigma = 0.5320,
      type = method
    )
    data.frame(
      method = method,
      lcl = lim$lcl,
      center = lim$center,
      ucl = lim$ucl,
      alpha = risk$alpha,
      arl0 = risk$arl0
    )
  })
)
gv_table
#>   method lcl    center      ucl       alpha      arl0
#> 1 normal   0 0.4728889 1.428685 0.020789525  48.10115
#> 2     cf   0 0.4728889 2.160305 0.002651853 377.09479
#> 3  exact   0 0.4728889 2.153629 0.002700000 370.37037
```

``` r

set.seed(321)
x <- array(rnorm(10 * 8 * 2), dim = c(10, 8, 2))
cchart.GV(x, Sigma = diag(2), type = "exact", plot = TRUE)
```

![](statistical-foundations_files/figure-html/gv-figure-1.png)

    #> Generalized Variance Control Chart
    #>   Dimension: p = 2 ; subgroup size n = 8 
    #>   Subgroups: 10 (Phase I: 10 ; Phase II: 0 )
    #>   Limits: exact / upper ; nominal alpha = 0.0027 
    #>   Covariance: supplied Sigma 
    #>   LCL = 0 ; center = 0.8571 ; UCL = 4.622 
    #>   Signals: 0

For dimension two, the exact distribution simplifies to

``` math
G\overset{d}{=}
\frac{|\Sigma|}{4(n-1)^2}
\left(\chi^2_{2n-4}\right)^2.
```

For higher dimensions, IQCC offers simulation using the Bartlett
factors. The package does not claim a generic Meijer-G implementation.
Published exact quantiles are used only in explicitly supported
dimension-three cases.

``` r

cat("<!-- IQCC_EXECUTED_FOUNDATIONS -->\n")
```

## Validation strategy

IQCC combines algebraic property tests, independently coded numerical
oracles, and reproduction of published numerical examples. This is
particularly important because a plausible formula may still use a
different tail convention, integer threshold, or parameter-estimation
rule than the intended method.

## References

- Joekes, S. and Barbosa, E. P. (2013). An improved attribute control
  chart for monitoring non-conforming proportion in high quality
  processes. *Control Engineering Practice*, 21, 407-412.
- Joekes, S., Smrekar, M. and Barbosa, E. P. (2015). Extending a double
  sampling control chart for non-conforming proportion in high quality
  processes to the case of small samples. *Statistical Methodology*, 23,
  35-49.
- Barbosa, E. P., Gneri, M. A. and Meneguetti, A. *Improving
  Shewhart-type Generalized Variance Control Charts for Multivariate
  Process Variability Monitoring using Cornish-Fisher Quantile
  Correction, Meijer-G Function and Other Tools*.
