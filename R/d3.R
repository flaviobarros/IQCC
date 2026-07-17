#' Standard Deviation of the Relative Range
#'
#' Compute the constant \eqn{d_3}, the standard deviation of the relative range
#' \eqn{W = R / \sigma} for a sample of size \eqn{n} drawn from a normal
#' distribution, where \eqn{R} is the sample range and \eqn{\sigma} is the
#' process standard deviation.
#'
#' Under normality, \eqn{W} has the studentized range distribution with infinite
#' denominator degrees of freedom. Using the identity \eqn{E[W^2] = 2 \int_0^\infty
#' w \{1 - F_W(w)\} dw}, the standard deviation is obtained as \eqn{d_3 =
#' \sqrt{E[W^2] - d_2^2}}, where \eqn{d_2} is computed by \code{\link{d2}}. The
#' integrals are evaluated numerically using \code{\link[stats]{ptukey}} with
#' infinite degrees of freedom.
#'
#' @param n Integer sample size, at least 2. Can be a vector.
#' @return Numeric vector of \eqn{d_3} values, the same length as \code{n}.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @references
#' Montgomery, D. C. (2009). \emph{Introduction to Statistical Quality Control},
#' 6th ed. Wiley.
#'
#' Barbosa, E. P., Gneri, M. A. and Meneguetti, A. (2013). Range control charts
#' revisited: Simpler Tippett-like formulae, its practical implementation, and
#' the study of false alarm. \emph{Communications in Statistics - Simulation
#' and Computation}, 42(2), 247--262. \doi{10.1080/03610918.2011.639967}.
#' @seealso \code{\link{d2}}, \code{\link{c4}}
#' @importFrom stats ptukey integrate
#' @examples
#'
#' d3(7)
#' d3(2:10)
#'
d3 <- function(n)
{
    d2 <- d2(n)
    e <- numeric(length(n))
    for(i in seq_along(n))
    {
        int <- integrate(function(w) { w * (1 - ptukey(w, n[i], Inf)) }, 0, Inf)
        e[i] <- sqrt(2 * int[[1]] - (d2[i]) ^ 2)
    }
    return(e)
}
