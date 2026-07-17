#' Mean of the Relative Range
#'
#' Compute the constant \eqn{d_2}, the expected value of the relative range
#' \eqn{W = R / \sigma} for a sample of size \eqn{n} drawn from a normal
#' distribution, where \eqn{R} is the sample range and \eqn{\sigma} is the
#' process standard deviation.
#'
#' Under normality, \eqn{W} has the studentized range distribution with infinite
#' denominator degrees of freedom. Its expectation is the integral of the
#' survival function, \eqn{E[W] = \int_0^\infty \{1 - F_W(w)\} dw}, which is
#' evaluated numerically using \code{\link[stats]{ptukey}} with infinite
#' degrees of freedom.
#'
#' @param n Integer sample size, at least 2. Can be a vector.
#' @return Numeric vector of \eqn{d_2} values, the same length as \code{n}.
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
#' @seealso \code{\link{d3}}, \code{\link{c4}}, \code{\link{table.const}}
#' @importFrom stats ptukey integrate
#' @examples
#'
#' d2(5)
#' d2(2:10)
#'
d2 <- function(n)
{
    if(any(n < 2))
        stop("n must be >= 2")
    d <- numeric(length(n))
    for(i in seq_along(n))
    {
        int <- integrate(function(w) {1 - ptukey(w, n[i], Inf)}, 0, Inf)
        d[i] <- int[[1]]
    }
    return(d)
}
