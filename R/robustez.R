#' Asymptotic Robustness of Hotelling T²
#'
#' Documentation of the asymptotic robustness property of the Hotelling T²
#' statistic under non-normal distributions, based on Gneri and Barbosa
#' (2006), and its relationship to the IQCC package implementation.
#'
#' @section Theorem 3 (Gneri & Barbosa, 2006, Sección 4.2):
#'
#' Let \eqn{X} be a \eqn{p}-dimensional random vector with continuous
#' distribution, mean vector \eqn{\mu}, positive-definite covariance matrix
#' \eqn{\Sigma}, and finite fourth moments. Let \eqn{X_1, X_2, \dots, X_n}
#' be an i.i.d. sample, and denote by \eqn{\bar{X}(n)} and \eqn{S(n)} the
#' sample mean vector and sample covariance matrix. The Hotelling T²
#' statistic is defined as:
#'
#' \deqn{T^2 = n (\bar{X}(n) - \mu)' S(n)^{-1} (\bar{X}(n) - \mu).}
#'
#' Then, as \eqn{n \to \infty}, \eqn{T^2} converges in distribution to a
#' chi-squared random variable with \eqn{p} degrees of freedom.
#'
#' @section Proof sketch:
#'
#' The proof proceeds in three steps:
#' \enumerate{
#'   \item The sample covariance converges in probability:
#'         \eqn{S(n)_{ij} \to \Sigma_{ij}} elementwise (Khinchin's LLN),
#'         and by continuity of matrix inversion,
#'         \eqn{S(n)^{-1} \to \Sigma^{-1}} in probability.
#'   \item By the Central Limit Theorem,
#'         \eqn{\sqrt{n}(\bar{X}(n) - \mu) \to N_p(0, \Sigma)} in
#'         distribution.
#'   \item By Proposition 2 (a Slutsky-type result),
#'         \eqn{T^2 \to W' \Sigma^{-1} W} where \eqn{W \sim N_p(0, \Sigma)},
#'         i.e., \eqn{T^2 \to \chi^2_p}.
#' }
#'
#' @section Discrepancy in moment conditions:
#'
#' The abstract and introduction of Gneri & Barbosa (2006) state the theorem
#' under the assumption of finite second moments only ("momentos de orden 2
#' finitos"). However, the formal statement of Theorem 3 (Sección 4.2)
#' requires finite fourth moments. The proof uses:
#' \itemize{
#'   \item Khinchin's Law of Large Numbers for the sample covariance
#'         \eqn{S(n)}, which requires \eqn{E[|X_i X_j|] < \infty}. By
#'         Cauchy-Schwarz, this holds when second moments are finite, so
#'         order 2 suffices for this step.
#'   \item The Central Limit Theorem for \eqn{\sqrt{n}(\bar{X} - \mu)},
#'         which requires finite second moments.
#' }
#' The fourth-moment condition in the theorem statement is therefore
#' conservative. The theorem holds under finite second moments, provided
#' the covariance matrix is finite and positive-definite.
#'
#' @section Scope and limitations:
#'
#' The theorem is asymptotic and does \strong{not} imply:
#' \itemize{
#'   \item The exact finite-sample distribution of \eqn{T^2} under
#'         non-normality.
#'   \item The joint distribution of a sequence of charted points in a
#'         control chart.
#'   \item Valid Average Run Length (ARL), false-alarm risk, or nominal
#'         coverage for small samples.
#'   \item Resolution of parameter estimation uncertainty in Phase I.
#'   \item Justification of the finite-sample F or beta control limits
#'         derived under normality for arbitrary continuous distributions.
#'   \item Coverage of discrete distributions, singular covariance,
#'         increasing dimension with \eqn{n}, temporal dependence, or
#'         infinite moments.
#' }
#'
#' @section Relationship to IQCC functions:
#'
#' The following table maps the T² statistic in Theorem 3 to the
#' corresponding IQCC functions:
#'
#' \tabular{lll}{
#'   \strong{Concept} \tab \strong{IQCC function} \tab \strong{Notes}\cr
#'   T² statistic      \tab \code{\link{T2.1}}        \tab Phase I; uses \eqn{\bar{\bar{x}}} and pooled \eqn{\bar{S}} from \code{\link{stats}}\cr
#'   T² statistic      \tab \code{\link{T2.2}}        \tab Phase II; tests new observations against Phase I estimates\cr
#'   Phase I chart     \tab \code{\link{cchart.T2.1}} \tab Uses beta (n=1) or F (n>1) limits (finite-sample normal theory)\cr
#'   Phase II chart    \tab \code{\link{cchart.T2.2}} \tab Uses F limits (finite-sample normal theory)\cr
#'   Auxiliary stats   \tab \code{\link{stats}}        \tab Grand mean, pooled covariance, subgroup means\cr
#'   Phase I data      \tab \code{\link{data.1}}       \tab Generates multivariate normal samples\cr
#'   Phase II data     \tab \code{\link{data.2}}       \tab Generates Phase II observation(s)\cr
#' }
#'
#' The T² statistic in \code{T2.1()} with \eqn{n > 1} matches the form in
#' Theorem 3 exactly: \eqn{T^2_i = n (\bar{x}_i - \bar{\bar{x}})' \bar{S}^{-1}
#' (\bar{x}_i - \bar{\bar{x}})}. For individual observations (\eqn{n = 1}),
#' the centering is at zero rather than the grand mean: \eqn{T^2_i = x_i'
#' \bar{S}^{-1} x_i}.
#'
#' The control limits in \code{cchart.T2.1()} and \code{cchart.T2.2()} are
#' based on exact finite-sample distributions under multivariate normality
#' (F and beta distributions). These limits are \strong{not} justified by
#' Theorem 3 for non-normal data in finite samples. The theorem only
#' guarantees that as \eqn{n \to \infty}, the T² statistic approaches a
#' \eqn{\chi^2_p} distribution regardless of the underlying continuous
#' distribution (provided the moment conditions hold).
#'
#' @references
#' Gneri, M. A. and Barbosa, E. P. (2006). "Robustez Asintótica de la
#' Estadística de Hotelling". Sección 4.2, Teorema 3, pp. 34-36.
#' IMECC-UNICAMP.
#'
#' Montgomery, D. C. (2009). "Introduction to Statistical Quality Control".
#' Chapter 11. Wiley.
#'
#' @name robustez
#' @aliases robustez
#' @docType NULL
#' @keywords internal
NULL
