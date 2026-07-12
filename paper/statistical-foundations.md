# Statistical foundations and reproducible derivations for IQCC

## Purpose and status

This document records the mathematical derivations, implementation conventions,
validation examples, and unresolved methodological questions developed during the
2026 scientific audit of IQCC. It is intended to serve as technical source material
for a future article about the package.

The document distinguishes four kinds of statements:

1. **published results**, reproduced from the cited papers;
2. **algebraic derivations**, reconstructed from probability models;
3. **software conventions**, adopted to make discrete decision rules unambiguous;
4. **scope boundaries**, where IQCC deliberately avoids claiming more than has
   actually been implemented and validated.

The principal themes documented here are:

- corrected binomial p charts for high-quality processes;
- corrected Poisson u charts;
- double-sampling np charts with exact binomial probabilities;
- generalized variance charts based on the determinant of a sample covariance
  matrix.

---

# 1. General design principles

## 1.1 Separate numerical kernels from plotting

Every chart should be decomposed into at least two layers:

- a pure numerical layer that computes estimates, limits, probabilities, ARL, and
  other diagnostics;
- a presentation layer that draws the chart and classifies observations.

This separation supports direct numerical testing, independent validation, and
future reuse in simulations or comparative studies.

## 1.2 Evaluate the actual discrete false-alarm probability

For attribute charts, nominal Gaussian tail probability and actual signaling
probability are generally different because counts are discrete. Therefore IQCC
computes, whenever possible,

\[
\alpha_{\mathrm{actual}} = P(\text{signal}\mid\text{process in control})
\]

under the corresponding binomial or Poisson model.

Strict inequalities matter. If a chart signals when the statistic exceeds an upper
limit, then for an integer count variable the exact event must be converted to the
correct integer threshold rather than approximated informally.

## 1.3 Preserve the original measurement scale

Cornish-Fisher corrections are used to improve quantiles without transforming the
reported statistic. This is central to the research identity of IQCC: the chart
remains interpretable on the original p, u, or generalized-variance scale.

---

# 2. Binomial p charts

## 2.1 Model and conventional limits

Let

\[
X \sim \operatorname{Binomial}(n,p), \qquad \hat p = X/n.
\]

Then

\[
E(\hat p)=p, \qquad
\operatorname{Var}(\hat p)=\frac{p(1-p)}{n}.
\]

The usual normal-approximation limits are

\[
LCL_N = p-z\sqrt{\frac{p(1-p)}{n}}, \qquad
UCL_N = p+z\sqrt{\frac{p(1-p)}{n}}.
\]

For the traditional three-sigma chart, \(z=3\) and the nominal two-sided risk is
approximately 0.0027.

## 2.2 First Cornish-Fisher correction

The standardized skewness of \(\hat p\) is

\[
\gamma_1 = \frac{1-2p}{\sqrt{np(1-p)}}.
\]

The first Cornish-Fisher correction to a standardized normal quantile \(z\) is

\[
q_{CF1}=z+\frac{\gamma_1}{6}(z^2-1).
\]

Multiplying by the standard deviation of \(\hat p\) gives

\[
q_{CF1}\sqrt{\frac{p(1-p)}{n}}
=
 z\sqrt{\frac{p(1-p)}{n}}
 + \frac{(z^2-1)(1-2p)}{6n}.
\]

For \(z=3\), the additive correction is

\[
\frac{4(1-2p)}{3n}.
\]

Thus the published one-term corrected limits are

\[
UCL_1 = UCL_N + \frac{4(1-2p)}{3n},
\qquad
LCL_1 = LCL_N + \frac{4(1-2p)}{3n}.
\]

The same additive term appears in both published operational limits.

## 2.3 Second correction and the sign convention

The general second-order Cornish-Fisher expression contains skewness and excess
kurtosis terms. A direct mechanical substitution of a negative normal quantile for
the lower tail does **not** reproduce the operational limits reported by Joekes and
Barbosa (2013).

The audited implementation therefore follows the article's explicit operational
formula, in which the second correction is evaluated using the positive magnitude
of the reference normal quantile and is subtracted from both first-correction
limits.

For the three-sigma case, the article gives

\[
UCL_2 = UCL_1 -
\frac{p(1-p)+2}
     {6n^2\sqrt{p(1-p)/n}},
\]

\[
LCL_2 = LCL_1 -
\frac{p(1-p)+2}
     {6n^2\sqrt{p(1-p)/n}}.
\]

This convention is a substantive methodological point and should be stated in any
article about the implementation. It is not equivalent to blindly applying the
general signed quantile expansion to both tails.

## 2.4 Applicability diagnostics

The paper's practical recommendations may be summarized approximately as:

- normal approximation: \(np(1-p)\geq 5\);
- one-term Cornish-Fisher: \(np(1-p)\geq 0.25\);
- two-term Cornish-Fisher: \(np(1-p)\geq 0.08\).

These are diagnostics, not mathematical guarantees. The exact discrete risk should
still be examined.

## 2.5 Exact false-alarm probability

For an upper limit \(U\), the signal event is

\[
\hat p > U
\iff
X > nU.
\]

Hence the integer threshold is

\[
k_U = \lfloor nU\rfloor + 1,
\]

and

\[
P(\text{upper signal})=P(X\geq k_U).
\]

For a lower limit \(L\),

\[
\hat p < L
\iff
X < nL,
\]

so the largest signaling integer is

\[
k_L = \lceil nL\rceil -1.
\]

The total two-sided risk is the sum of the disjoint lower- and upper-tail
probabilities.

## 2.6 Published numerical checks

For \(p=0.015\) and \(n=20\), the audited code reproduces the published values:

| Method | UCL | exact alpha risk |
|---|---:|---:|
| Normal | 0.0965 | 0.035746 |
| CF1 | 0.1612 | 0.000202 |
| CF2 | 0.1303 | 0.003178 |

For \(p=0.004\) and \(n=20\):

| Method | UCL | exact alpha risk |
|---|---:|---:|
| Normal | 0.0463 | 0.077032 |
| CF1 | 0.1125 | 0.923038 |
| CF2 | 0.0533 | 0.002898 |

The second table is especially informative: a correction can be theoretically
motivated but still be badly calibrated when used outside its practical domain.

## 2.7 Phase I estimation

When subgroup sizes vary, the in-control proportion is estimated by the pooled
binomial estimator

\[
\hat p_0 = \frac{\sum_i X_i}{\sum_i n_i},
\]

not by the unweighted mean of subgroup proportions. The pooled estimator follows
directly from the joint binomial likelihood.

---

# 3. Poisson u charts

## 3.1 Model

Let

\[
X\sim\operatorname{Poisson}(\lambda n),
\qquad U=X/n,
\]

where \(n\) is the inspection opportunity or exposure size. Then

\[
E(U)=\lambda,
\qquad
\operatorname{Var}(U)=\frac{\lambda}{n}.
\]

The pooled Phase I estimator is

\[
\hat\lambda = \frac{\sum_i X_i}{\sum_i n_i}.
\]

## 3.2 Standardized cumulants

For a Poisson variable, all ordinary cumulants equal its mean. After scaling by
\(n\), the standardized skewness and excess kurtosis of \(U\) are

\[
\gamma_1=\frac{1}{\sqrt{\lambda n}},
\qquad
\gamma_2=\frac{1}{\lambda n}.
\]

## 3.3 First Cornish-Fisher term

The first correction on the original u scale is

\[
\sqrt{\frac{\lambda}{n}}
\frac{\gamma_1}{6}(z^2-1)
=
\frac{z^2-1}{6n}.
\]

Thus the first corrected upper quantile is

\[
UCL_{CF1}
=
\lambda + z\sqrt{\frac{\lambda}{n}}
+
\frac{z^2-1}{6n}.
\]

For \(z=3\), the additive term is \(4/(3n)\).

## 3.4 Second Cornish-Fisher term

Using the standard second-order expansion,

\[
q_{CF2}
=
 z
 + \frac{\gamma_1}{6}(z^2-1)
 + \frac{\gamma_2}{24}(z^3-3z)
 - \frac{\gamma_1^2}{36}(2z^3-5z).
\]

Substituting the Poisson cumulants and multiplying by
\(\sqrt{\lambda/n}\), the second-order addition simplifies to

\[
\frac{z(1-z^2)}{72n\sqrt{\lambda n}}.
\]

For \(z=3\),

\[
UCL_{CF2}
=
\lambda
+3\sqrt{\frac{\lambda}{n}}
+\frac{4}{3n}
-\frac{1}{3n\sqrt{\lambda n}}.
\]

This exactly reproduces the historical formula already present in IQCC and provides
its probabilistic derivation.

## 3.5 Exact risk

For a proposed upper limit \(U\), the exact upper-tail signal probability is

\[
P\left(\frac{X}{n}>U\right)
=
P\left(X\geq \lfloor nU\rfloor+1\right),
\]

with \(X\sim\operatorname{Poisson}(\lambda n)\). Lower-tail risk is handled by the
corresponding strict inequality.

---

# 4. Double-sampling np charts

## 4.1 Parameters and decision regions

The double-sampling chart uses:

- first sample size \(n_1\);
- second sample size \(n_2\);
- warning limit \(WL\);
- first-stage upper control limit \(UCL_1\);
- combined upper control limit \(UCL_2\).

Let

\[
D_1\sim\operatorname{Binomial}(n_1,p),
\qquad
D_2\sim\operatorname{Binomial}(n_2,p),
\]

independently.

Define the operational integer thresholds

\[
a=\lfloor WL\rfloor,
\qquad
b=\lfloor UCL_1\rfloor+1,
\qquad
c=\lfloor UCL_2\rfloor.
\]

Then:

- accept after stage 1 if \(D_1\leq a\);
- signal after stage 1 if \(D_1\geq b\);
- continue to stage 2 if \(a<D_1<b\);
- after stage 2, accept if \(D_1+D_2\leq c\), otherwise signal.

For the canonical half-integer limits of the paper,
\(b=\lceil UCL_1\rceil\). The more general expression
\(\lfloor UCL_1\rfloor+1\) correctly represents the strict inequality
\(D_1>UCL_1\) even if a user supplies an integer limit.

## 4.2 Acceptance at stage 1

\[
P_{aI}(p)=P(D_1\leq a)
=
\sum_{d_1=0}^{a}
\binom{n_1}{d_1}p^{d_1}(1-p)^{n_1-d_1}.
\]

## 4.3 Acceptance at stage 2

For each first-stage count in the continuation region, acceptance requires
\(D_2\leq c-d_1\). Therefore

\[
P_{aII}(p)
=
\sum_{d_1=a+1}^{b-1}
P(D_1=d_1)P(D_2\leq c-d_1).
\]

This representation is computationally efficient because it combines a binomial
PMF for the first stage with a binomial CDF for the second stage.

## 4.4 Total acceptance, signal probability, and ARL

\[
P_T(p)=P_{aI}(p)+P_{aII}(p),
\]

\[
P_{signal}(p)=1-P_T(p),
\]

\[
ARL(p)=\frac{1}{P_{signal}(p)}
=
\frac{1}{1-P_T(p)}.
\]

Thus

\[
ARL_0=ARL(p_0),
\qquad
ARL_1=ARL(p_1), \quad p_1>p_0.
\]

## 4.5 Probability of a second sample and ASS

The continuation probability is

\[
P_2(p)=P(a<D_1<b).
\]

Hence

\[
ASS(p)=n_1+n_2P_2(p).
\]

This formula assumes complete inspection of the second sample. Curtailed inspection,
where inspection stops as soon as the combined count exceeds the stage-2 limit,
would require a different expected-sample-size calculation and is not currently
implemented.

## 4.6 Independent enumeration oracle

For small sample sizes, the implementation is checked against a deliberately simple
oracle that enumerates every pair

\[
(d_1,d_2)\in\{0,\ldots,n_1\}\times\{0,\ldots,n_2\}
\]

and sums the probabilities of all accepted pairs. This oracle is slower but easier
to audit than the optimized formula. Agreement between the two calculations is a
strong regression test.

## 4.7 Published validation cases

The implementation reproduces the following published design:

\[
p_0=0.005,\quad p_1=0.0075,\quad
n_1=34,\quad n_2=162,
\]

\[
WL=1.5,\quad UCL_1=2.5,\quad UCL_2=4.5.
\]

The resulting values are

\[
ASS_0=35.94,
\qquad
ARL_0=803.41,
\qquad
ARL_1=193.22.
\]

Additional reproduced rows include:

| \(p_0\) | \(p_1\) | \(n_1\) | \(n_2\) | \(WL\) | \(UCL_1\) | \(UCL_2\) | \(ASS_0\) | \(ARL_0\) | \(ARL_1\) |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 0.005 | 0.010 | 50 | 242 | 1.5 | 2.5 | 4.5 | 55.83 | 200.04 | 21.37 |
| 0.010 | 0.020 | 25 | 118 | 1.5 | 2.5 | 4.5 | 27.81 | 212.94 | 22.24 |
| 0.020 | 0.040 | 20 | 73 | 1.5 | 3.5 | 5.5 | 24.33 | 205.27 | 13.04 |

## 4.8 Limit search

For fixed \(n_1\) and \(n_2\), candidate designs are discrete triples
\((a,b,c)\). Their half-integer representation is

\[
WL=a+0.5,
\qquad
UCL_1=b-0.5,
\qquad
UCL_2=c+0.5.
\]

Each value of \(c\) must be evaluated independently. A previous defect in the
package accidentally reused a fixed stage-2 limit while iterating over candidate
values. The regression tests now verify that increasing \(c\) changes the signal
probability monotonically in the expected direction.

The current function searches limits for fixed sample sizes. It is not yet a full
reproduction of the paper's joint optimization over sample sizes and limits.

---

# 5. Generalized variance charts

## 5.1 Sampling distribution

Let \(X_1,\ldots,X_n\) be independent observations from a
\(p\)-variate normal distribution with covariance matrix \(\Sigma\), and let
\(S\) be the usual sample covariance matrix with denominator \(n-1\). Then

\[
(n-1)S\sim W_p(n-1,\Sigma).
\]

The generalized variance statistic is

\[
G=|S|.
\]

Under Bartlett's decomposition,

\[
\frac{|(n-1)S|}{|\Sigma|}
\overset{d}{=}
\prod_{j=1}^{p}Y_j,
\qquad
Y_j\sim\chi^2_{n-j},
\]

with independent factors. Therefore

\[
G
\overset{d}{=}
\frac{|\Sigma|}{(n-1)^p}
\prod_{j=1}^{p}Y_j.
\]

This identity is the foundation of the exact moments, simulation algorithm, and the
closed dimension-two distribution used in IQCC.

## 5.2 Ordinary moments

For \(r>0\),

\[
E(Y_j^r)
=2^r\frac{\Gamma(r+(n-j)/2)}{\Gamma((n-j)/2)}.
\]

Independence gives

\[
E(G^r)
=
|\Sigma|^r
\left(\frac{2}{n-1}\right)^{pr}
\prod_{j=1}^{p}
\frac{\Gamma(r+(n-j)/2)}{\Gamma((n-j)/2)}.
\]

The implementation evaluates this expression on the log scale to reduce overflow
and underflow.

Let \(a_r=E(G^r)\). The first four central moments are reconstructed as

\[
\mu_2=a_2-a_1^2,
\]

\[
\mu_3=a_3-3a_1a_2+2a_1^3,
\]

\[
\mu_4=a_4-4a_1a_3+6a_1^2a_2-3a_1^4.
\]

Then

\[
\gamma_1=\frac{\mu_3}{\mu_2^{3/2}},
\qquad
\gamma_2=\frac{\mu_4}{\mu_2^2}-3.
\]

The constants used in the report are represented by

\[
E(G)=b_1|\Sigma|,
\qquad
\operatorname{Var}(G)=b_2|\Sigma|^2.
\]

## 5.3 Normal limits

Using the exact mean and variance but a Gaussian quantile gives

\[
LCL_N=\max\{0, E(G)+z_{\alpha_L}\sqrt{\operatorname{Var}(G)}\},
\]

\[
UCL_N=E(G)+z_{\alpha_U}\sqrt{\operatorname{Var}(G)}.
\]

For an upper chart, \(\alpha_U=1-\alpha\) and the lower limit is zero. For a
two-sided chart, \(\alpha_L=\alpha/2\) and
\(\alpha_U=1-\alpha/2\).

## 5.4 Cornish-Fisher limits

For the standardized variable

\[
Z_G=\frac{G-E(G)}{\sqrt{\operatorname{Var}(G)}},
\]

the one-term corrected quantile is

\[
q_{CF1}=z+\frac{\gamma_1}{6}(z^2-1).
\]

The two-term option is

\[
q_{CF2}
=z
+\frac{\gamma_1}{6}(z^2-1)
+\frac{\gamma_2}{24}(z^3-3z)
-\frac{\gamma_1^2}{36}(2z^3-5z).
\]

The control limit on the original scale is

\[
E(G)+q_{CF}\sqrt{\operatorname{Var}(G)}.
\]

The research report emphasizes that a one-term correction already provides a large
improvement in the studied settings. IQCC exposes both orders because all required
moments are available and the distinction is useful for research comparisons.

## 5.5 Exact distribution for dimension two

For \(p=2\),

\[
G
\overset{d}{=}
\frac{|\Sigma|}{(n-1)^2}
\chi^2_{n-1}\chi^2_{n-2}.
\]

The product has the distributional identity

\[
\chi^2_{n-1}\chi^2_{n-2}
\overset{d}{=}
\frac{1}{4}\left(\chi^2_{2n-4}\right)^2.
\]

Consequently,

\[
G
\overset{d}{=}
\frac{|\Sigma|}{4(n-1)^2}
\left(\chi^2_{2n-4}\right)^2.
\]

Thus an exact quantile is

\[
q_G(u)
=
\frac{|\Sigma|}{4(n-1)^2}
\left[q_{\chi^2_{2n-4}}(u)\right]^2.
\]

The exact CDF can also be inverted algebraically:

\[
P(G\leq g)
=
F_{\chi^2_{2n-4}}
\left(2(n-1)\sqrt{g/|\Sigma|}\right).
\]

This permits exact false-alarm evaluation for dimension two.

## 5.6 Dimension three and the Meijer-G boundary

For general \(p\), the density of a product of independent gamma variables may be
written using a Meijer-G function. The research report develops that representation
and supplies exact numerical quantiles for selected cases.

The current IQCC implementation does not claim a generic Meijer-G evaluator. For
\(p=3\), it uses only the published upper-tail quantiles for:

- \(n=4,\ldots,15\);
- \(\alpha=0.0020\) or \(0.0027\);
- upper one-sided charts.

All other higher-dimensional exact requests are rejected explicitly rather than
silently approximated.

## 5.7 Simulation through Bartlett factors

Simulation does not require generating full Wishart matrices. From the Bartlett
representation, simulate independently

\[
Y_j\sim\chi^2_{n-j},\qquad j=1,\ldots,p,
\]

and compute

\[
G^{(b)}
=
\frac{|\Sigma|}{(n-1)^p}
\prod_{j=1}^{p}Y_j^{(b)}.
\]

This is faster and simpler than repeatedly generating covariance matrices. It also
makes the distribution's scale property explicit:

\[
G/|\Sigma|
\]

depends only on \(n\) and \(p\).

The implementation accepts an optional seed and restores the caller's random-number
state after the calculation.

## 5.8 Phase I estimation and the b3 correction

Suppose \(m\) Phase I subgroup covariance matrices are averaged:

\[
\bar S=\frac{1}{m}\sum_{i=1}^{m}S_i.
\]

Then

\[
m(n-1)\bar S\sim W_p(m(n-1),\Sigma).
\]

The determinant is biased. If \(\nu=m(n-1)\), then

\[
E(|\bar S|)=b_3|\Sigma|,
\]

with

\[
b_3=\frac{\prod_{j=1}^{p}(\nu-j+1)}{\nu^p}.
\]

Therefore IQCC estimates

\[
|\Sigma|\approx\frac{|\bar S|}{b_3}.
\]

This is a determinant-level correction. The current implementation does not attempt
to reconstruct a corrected covariance matrix whose determinant equals the corrected
value.

## 5.9 Published numerical checks

### Dimension two

For

\[
n=10,\qquad |\Sigma|=0.5320,
\]

the implementation reproduces:

| Method | UCL |
|---|---:|
| Normal | 1.4286 |
| Cornish-Fisher | 2.1602 |
| Exact | 2.1536 |

### Dimension three

For

\[
p=3,\qquad n=15,\qquad m=30,
\]

with

\[
|\bar S|=69.8438
\]

and the published \(b_3\) correction, the implementation reproduces:

| Method | UCL |
|---|---:|
| Normal | 170.294 |
| Cornish-Fisher | 267.652 |
| Exact | 265.462 |

The closeness of the Cornish-Fisher and exact limits, compared with the normal
limit, illustrates the practical importance of skewness correction.

---

# 6. Validation philosophy

## 6.1 Three levels of validation

The package now uses three complementary validation levels:

1. **algebraic properties**: scaling, monotonicity, boundary behavior, and exact
   identities;
2. **independent numerical oracles**: direct enumeration or exact CDFs;
3. **published examples**: values reproduced to stated numerical tolerances.

Passing only one of these levels is insufficient for a research-oriented package.

## 6.2 Why published-value tests matter

A formula can be algebraically reasonable while using a different convention from
the paper. This happened during the p-chart audit: the general signed Cornish-Fisher
lower-tail expression did not reproduce the article's operational equation. Tests
against published tables exposed the discrepancy.

## 6.3 Why independent oracles matter

Reusing the same optimized function to test itself can preserve a shared bug. The
DS-np direct enumerator and the exact dimension-two generalized-variance CDF are
examples of deliberately independent checks.

---

# 7. Candidate structure for a future package article

A future article could be organized as follows:

1. **Motivation**: classical Shewhart simplicity versus poor probabilistic
   calibration for skewed, discrete, or bounded statistics.
2. **Package architecture**: pure numerical functions, chart wrappers, diagnostics,
   and reproducible validation.
3. **High-quality attribute processes**:
   - p chart with CF1 and CF2;
   - exact binomial risk;
   - u chart derivation;
   - DS-np design and performance measures.
4. **Multivariate monitoring**:
   - Hotelling T-squared facilities already in IQCC;
   - generalized variance theory;
   - normal, Cornish-Fisher, exact, and simulation limits.
5. **Numerical validation**: published tables and independent calculations.
6. **Software engineering**: continuous integration, test coverage, pkgdown, and
   backward compatibility.
7. **Limitations and future work**:
   - full DS-np sample-size optimization;
   - generic Meijer-G quantile implementation;
   - trace-based variability charts;
   - non-normal robustness studies;
   - broader Phase I estimation research.

---

# 8. Open methodological questions

The following questions should remain visible rather than being hidden in code:

- Should arbitrary integer DS-np limits be rejected, or should the current generic
  strict-inequality conversion remain supported?
- Should generalized-variance exact quantiles be extended with numerical Mellin
  inversion, a Meijer-G library, or a carefully validated saddlepoint method?
- Should the default generalized-variance chart be upper one-sided, reflecting
  interest in dispersion increases, or two-sided for broader monitoring?
- How should parameter-estimation uncertainty be incorporated into achieved
  false-alarm risk for Phase I estimated charts?
- Should curtailed second-stage inspection be added to DS-np ASS calculations?

These are potential research contributions, not merely programming tasks.

---

# References

- Anderson, T. W. (1958, 1984). *An Introduction to Multivariate Statistical Analysis*.
- Barbosa, E. P., Gneri, M. A., and Meneguetti, A. *Improving Shewhart-type Generalized Variance Control Charts for Multivariate Process Variability Monitoring using Cornish-Fisher Quantile Correction, Meijer-G Function and Other Tools*.
- Cornish, E. A., and Fisher, R. A. (1960). The percentage points of distributions having known cumulants.
- Joekes, S., and Barbosa, E. P. (2013). An improved attribute control chart for monitoring non-conforming proportion in high quality processes. *Control Engineering Practice*, 21, 407-412.
- Joekes, S., Smrekar, M., and Barbosa, E. P. (2015). Extending a double sampling control chart for non-conforming proportion in high quality processes to the case of small samples. *Statistical Methodology*, 23, 35-49.
- Lee, Y., and Lee, M. C. (1992). On the derivation and computation of the Cornish-Fisher expansion.
- Smith, W. B., and Hocking, R. R. (1972). Algorithm AS 53: Wishart variate generator.
