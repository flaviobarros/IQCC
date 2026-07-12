# Statistical Foundations of IQCC

## Overview

IQCC focuses on control charts whose classical normal limits may be
poorly calibrated because the monitored statistic is discrete, skewed,
bounded, or strongly non-normal. The package combines three ideas:

1.  compute limits on the original measurement scale;
2.  evaluate actual false-alarm probabilities whenever possible;
3.  validate formulas against independent calculations and published
    examples.

This vignette summarizes the probability models behind the audited p, u,
double-sampling np, and generalized-variance charts.

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

to the normal quantile on the original p scale. For $`z=3`$, this
becomes $`4(1-2p)/(3n)`$.

The second corrected p chart follows the operational formula in Joekes
and Barbosa (2013). This is important because a generic signed
lower-tail Cornish-Fisher substitution does not reproduce the paper’s
reported limits.

``` r

pchart_limits(p = 0.015, n = 20, type = "normal")
pchart_limits(p = 0.015, n = 20, type = "cf1")
pchart_limits(p = 0.015, n = 20, type = "cf2")

pchart_alpha_risk(p = 0.015, n = 20, type = "cf2")
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

The standardized skewness and excess kurtosis are

``` math
\gamma_1=\frac{1}{\sqrt{\lambda n}},\qquad
\gamma_2=\frac{1}{\lambda n}.
```

The first Cornish-Fisher addition on the original scale is

``` math
\frac{z^2-1}{6n}.
```

The second-order term simplifies to

``` math
\frac{z(1-z^2)}{72n\sqrt{\lambda n}}.
```

For $`z=3`$, the upper limit is

``` math
\lambda+3\sqrt{\frac{\lambda}{n}}
+\frac{4}{3n}
-\frac{1}{3n\sqrt{\lambda n}}.
```

The actual risk is evaluated under the Poisson count model, respecting
strict signaling inequalities.

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

The first-stage acceptance probability is

``` math
P_{aI}(p)=P(D_1\leq a).
```

The second-stage acceptance probability is

``` math
P_{aII}(p)=
\sum_{d_1=a+1}^{b-1}
P(D_1=d_1)P(D_2\leq c-d_1).
```

Thus

``` math
P_T(p)=P_{aI}(p)+P_{aII}(p),
\qquad
ARL(p)=\frac{1}{1-P_T(p)}.
```

The probability of requiring a second sample is

``` math
P_2(p)=P(a<D_1<b),
```

and

``` math
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

arl0 <- dsnp_arl(0.005, 34, 162, 1.5, 2.5, 4.5)$arl
arl1 <- dsnp_arl(0.0075, 34, 162, 1.5, 2.5, 4.5)$arl
ass0 <- dsnp_ass(0.005, 34, 162, 1.5, 2.5)$ass
```

The published values are approximately

``` math
ARL_0=803.41,\qquad ARL_1=193.22,\qquad ASS_0=35.94.
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

Consequently,

``` math
E(G^r)=
|\Sigma|^r
\left(\frac{2}{n-1}\right)^{pr}
\prod_{j=1}^{p}
\frac{\Gamma(r+(n-j)/2)}{\Gamma((n-j)/2)}.
```

These moments provide the mean, variance, skewness, and kurtosis
required by normal and Cornish-Fisher limits.

``` r

gv_limits(
  n = 10,
  p = 2,
  det_sigma = 0.5320,
  type = "normal"
)

gv_limits(
  n = 10,
  p = 2,
  det_sigma = 0.5320,
  type = "cf"
)

gv_limits(
  n = 10,
  p = 2,
  det_sigma = 0.5320,
  type = "exact"
)
```

For dimension two, the exact distribution simplifies to

``` math
G\overset{d}{=}
\frac{|\Sigma|}{4(n-1)^2}
\left(\chi^2_{2n-4}\right)^2.
```

This gives exact quantiles and exact false-alarm probabilities.

For higher dimensions, IQCC offers simulation using the Bartlett
factors. The package does not claim a generic Meijer-G implementation.
Published exact quantiles are used only in explicitly supported
dimension-three cases.

When $`|\Sigma|`$ is unknown, the package estimates it from the average
Phase I covariance matrix. If $`m`$ subgroups are used and
$`\nu=m(n-1)`$, the determinant correction is

``` math
b_3=\frac{\prod_{j=1}^{p}(\nu-j+1)}{\nu^p},
\qquad
\widehat{|\Sigma|}=\frac{|\bar S|}{b_3}.
```

## Validation strategy

IQCC combines:

- algebraic property tests;
- exact or independently coded numerical oracles;
- reproduction of published numerical examples.

This strategy is particularly important for control charts because a
plausible formula may still use a different tail convention, integer
threshold, or parameter-estimation rule than the intended method.

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
