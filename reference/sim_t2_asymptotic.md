# Simulate Asymptotic Distribution of Hotelling T²

Monte Carlo simulation to study the convergence of the Hotelling T²
statistic to its asymptotic \\\chi^2_p\\ distribution under various
multivariate distributions. Used to validate the asymptotic robustness
result of Gneri and Barbosa (2006).

## Usage

``` r
sim_t2_asymptotic(
  n = c(10, 30, 100, 500),
  p = c(2, 5),
  distributions = c("normal", "t5", "gamma2"),
  nsim = 10000,
  sig_levels = c(0.9, 0.95, 0.99),
  seed = 42,
  rho = 0.3
)
```

## Arguments

- n:

  Integer vector of sample sizes to evaluate. Each element must satisfy
  \\n \> p\\ for the corresponding dimension. Default is
  `c(10, 30, 100, 500)`.

- p:

  Integer vector of dimensions to evaluate. Default is `c(2, 5)`.

- distributions:

  Character vector of distribution names. Available options:

  `"normal"`

  :   Multivariate normal \\N(0, \Sigma)\\. All moments finite. Baseline
      case.

  `"t5"`

  :   Elliptical multivariate t with 5 degrees of freedom, scaled to
      covariance \\\Sigma\\. Symmetric, heavy-tailed, finite fourth
      moment (\\4 \< 5\\).

  `"gamma2"`

  :   Independent Gamma(2, 1) margins, centered and scaled to covariance
      \\\Sigma\\. Asymmetric, all moments finite.

  `"t4"`

  :   Elliptical multivariate t with 4 degrees of freedom (stress test).
      Has infinite fourth moment, violating the formal condition of
      Theorem 3.

- nsim:

  Number of Monte Carlo replications. A single positive integer. Default
  is 10000.

- sig_levels:

  Numeric vector of quantile levels in \\(0, 1)\\ to compare. Default is
  `c(0.90, 0.95, 0.99)`.

- seed:

  Random seed for reproducibility. A single integer. Default is `42`.

- rho:

  Correlation parameter for the equicorrelation matrix \\\Sigma\_{ij} =
  1\\ if \\i = j\\, \\\rho\\ otherwise. Must satisfy \\\rho \>
  -1/(p-1)\\ for each \\p\\ to ensure positive definiteness. Default is
  `0.3`.

## Value

A data frame with columns:

- `n`:

  Sample size.

- `p`:

  Dimension.

- `distribution`:

  Distribution name.

- `level`:

  Quantile level (e.g., 0.95).

- `empirical`:

  Empirical quantile of simulated T².

- `chisq`:

  Theoretical \\\chi^2_p\\ quantile.

- `mcse`:

  Monte Carlo standard error of the empirical quantile.

- `nsim`:

  Number of valid replications (after discarding singular covariance
  draws).

## Details

For each combination of sample size \\n\\, dimension \\p\\, and
distribution, the function generates `nsim` samples, computes the
Hotelling T² statistic, and compares empirical quantiles against the
limiting \\\chi^2_p\\ distribution.

The T² statistic is computed from a single sample \\X_1, \dots, X_n\\ as
\\T^2 = n (\bar{X} - \mu)' S^{-1} (\bar{X} - \mu)\\ where \\\mu = 0\\ is
the known null mean and \\S\\ is the sample covariance. This matches the
form in Theorem 3 of Gneri and Barbosa (2006). It does **not** match the
Phase I statistic
[`T2.1`](https://flaviobarros.github.io/IQCC/reference/T2.1.md), which
uses estimated grand means and pooled covariance from multiple
subgroups.

## RNG preservation

The function saves and restores `.Random.seed` on exit, so it does not
alter the global RNG state.

## Monte Carlo standard error

The MCSE of a sample quantile at level \\q\\ is approximated as
\\\sqrt{q(1-q) / (N f(q)^2)}\\ where \\f\\ is the \\\chi^2_p\\ density.
This approximation assumes the density of the T² statistic is close to
\\\chi^2_p\\, which is reasonable when the asymptotic approximation
holds. It is not a substitute for the true standard error.

## References

Gneri, M. A. and Barbosa, E. P. (2006). "Robustez Asintótica de la
Estadística de Hotelling". Sección 4.2, Teorema 3, pp. 34-36.

## Examples

``` r
# Quick test with few replications
res <- sim_t2_asymptotic(
  n = c(30, 100),
  p = 2,
  distributions = c("normal", "t5"),
  nsim = 500,
  seed = 42
)
res
#>      n p distribution level empirical    chisq      mcse nsim
#> 1   30 2       normal  0.90  5.644723 4.605170 0.2683282  500
#> 2   30 2       normal  0.95  6.889938 5.991465 0.3898718  500
#> 3   30 2       normal  0.99 11.751425 9.210340 0.8899438  500
#> 4  100 2       normal  0.90  4.695593 4.605170 0.2683282  500
#> 5  100 2       normal  0.95  5.968509 5.991465 0.3898718  500
#> 6  100 2       normal  0.99  9.551809 9.210340 0.8899438  500
#> 7   30 2           t5  0.90  5.390198 4.605170 0.2683282  500
#> 8   30 2           t5  0.95  6.625412 5.991465 0.3898718  500
#> 9   30 2           t5  0.99  9.992108 9.210340 0.8899438  500
#> 10 100 2           t5  0.90  4.677420 4.605170 0.2683282  500
#> 11 100 2           t5  0.95  5.903337 5.991465 0.3898718  500
#> 12 100 2           t5  0.99  9.244146 9.210340 0.8899438  500
```
