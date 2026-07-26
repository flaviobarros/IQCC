# Asymptotic Robustness of Hotelling T²

Documentation of the asymptotic robustness property of the Hotelling T²
statistic under non-normal distributions, based on Gneri and Barbosa
(2006), and its relationship to the IQCC package implementation.

## Theorem 3 (Gneri & Barbosa, 2006, Sección 4.2)

Let \\X\\ be a \\p\\-dimensional random vector with continuous
distribution, mean vector \\\mu\\, positive-definite covariance matrix
\\\Sigma\\, and finite fourth moments. Let \\X_1, X_2, \dots, X_n\\ be
an i.i.d. sample, and denote by \\\bar{X}(n)\\ and \\S(n)\\ the sample
mean vector and sample covariance matrix. The Hotelling T² statistic is
defined as:

\$\$T^2 = n (\bar{X}(n) - \mu)' S(n)^{-1} (\bar{X}(n) - \mu).\$\$

Then, as \\n \to \infty\\, \\T^2\\ converges in distribution to a
chi-squared random variable with \\p\\ degrees of freedom.

## Proof sketch

The proof proceeds in three steps:

1.  The sample covariance converges in probability: \\S(n)\_{ij} \to
    \Sigma\_{ij}\\ elementwise (Khinchin's LLN), and by continuity of
    matrix inversion, \\S(n)^{-1} \to \Sigma^{-1}\\ in probability.

2.  By the Central Limit Theorem, \\\sqrt{n}(\bar{X}(n) - \mu) \to
    N_p(0, \Sigma)\\ in distribution.

3.  By Proposition 2 (a Slutsky-type result), \\T^2 \to W' \Sigma^{-1}
    W\\ where \\W \sim N_p(0, \Sigma)\\, i.e., \\T^2 \to \chi^2_p\\.

## Discrepancy in moment conditions

The abstract and introduction of Gneri & Barbosa (2006) state the
theorem under the assumption of finite second moments only ("momentos de
orden 2 finitos"). However, the formal statement of Theorem 3 (Sección
4.2) requires finite fourth moments. The proof uses:

- Khinchin's Law of Large Numbers for the sample covariance \\S(n)\\,
  which requires \\E\[\|X_i X_j\|\] \< \infty\\. By Cauchy-Schwarz, this
  holds when second moments are finite, so order 2 suffices for this
  step.

- The Central Limit Theorem for \\\sqrt{n}(\bar{X} - \mu)\\, which
  requires finite second moments.

The fourth-moment condition in the theorem statement is therefore
conservative. The theorem holds under finite second moments, provided
the covariance matrix is finite and positive-definite.

## Scope and limitations

The theorem is asymptotic and does **not** imply:

- The exact finite-sample distribution of \\T^2\\ under non-normality.

- The joint distribution of a sequence of charted points in a control
  chart.

- Valid Average Run Length (ARL), false-alarm risk, or nominal coverage
  for small samples.

- Resolution of parameter estimation uncertainty in Phase I.

- Justification of the finite-sample F or beta control limits derived
  under normality for arbitrary continuous distributions.

- Coverage of discrete distributions, singular covariance, increasing
  dimension with \\n\\, temporal dependence, or infinite moments.

## Relationship to IQCC functions

The following table maps the T² statistic in Theorem 3 to the
corresponding IQCC functions:

|  |  |  |
|----|----|----|
| **Concept** | **IQCC function** | **Notes** |
| T² statistic | [`T2.1`](https://flaviobarros.github.io/IQCC/reference/T2.1.md) | Phase I; uses \\\bar{\bar{x}}\\ and pooled \\\bar{S}\\ from [`stats`](https://flaviobarros.github.io/IQCC/reference/stats.md) |
| T² statistic | [`T2.2`](https://flaviobarros.github.io/IQCC/reference/T2.2.md) | Phase II; tests new observations against Phase I estimates |
| Phase I chart | [`cchart.T2.1`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.1.md) | Uses beta (n=1) or F (n\>1) limits (finite-sample normal theory) |
| Phase II chart | [`cchart.T2.2`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.2.md) | Uses F limits (finite-sample normal theory) |
| Auxiliary stats | [`stats`](https://flaviobarros.github.io/IQCC/reference/stats.md) | Grand mean, pooled covariance, subgroup means |
| Phase I data | [`data.1`](https://flaviobarros.github.io/IQCC/reference/data.1.md) | Generates multivariate normal samples |
| Phase II data | [`data.2`](https://flaviobarros.github.io/IQCC/reference/data.2.md) | Generates Phase II observation(s) |

The T² statistic in
[`T2.1()`](https://flaviobarros.github.io/IQCC/reference/T2.1.md) with
\\n \> 1\\ matches the form in Theorem 3 exactly: \\T^2_i = n
(\bar{x}\_i - \bar{\bar{x}})' \bar{S}^{-1} (\bar{x}\_i -
\bar{\bar{x}})\\. For individual observations (\\n = 1\\), the centering
is at zero rather than the grand mean: \\T^2_i = x_i' \bar{S}^{-1}
x_i\\.

The control limits in
[`cchart.T2.1()`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.1.md)
and
[`cchart.T2.2()`](https://flaviobarros.github.io/IQCC/reference/cchart.T2.2.md)
are based on exact finite-sample distributions under multivariate
normality (F and beta distributions). These limits are **not** justified
by Theorem 3 for non-normal data in finite samples. The theorem only
guarantees that as \\n \to \infty\\, the T² statistic approaches a
\\\chi^2_p\\ distribution regardless of the underlying continuous
distribution (provided the moment conditions hold).

## References

Gneri, M. A. and Barbosa, E. P. (2006). "Robustez Asintótica de la
Estadística de Hotelling". Sección 4.2, Teorema 3, pp. 34-36.
IMECC-UNICAMP.

Montgomery, D. C. (2009). "Introduction to Statistical Quality Control".
Chapter 11. Wiley.
