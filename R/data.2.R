#' Simulate Phase II Multivariate Normal Data
#'
#' Generate a single new observation from \eqn{N(\mu_0 + \delta, \Sigma)} using
#' Phase I reference parameters for Phase II monitoring.
#'
#' To use this function it is necessary to have the information about Phase I
#' given by the functions \code{\link{data.1}} and \code{\link{stats}}.
#'
#' @param estat List from \code{\link{stats}} (Phase I estimates).
#' @param n Subgroup size. Use \eqn{n = 1} for individual observations,
#'   \eqn{n > 1} for subgroups.
#' @param delta Optional shift added to mean vector for out-of-control
#'   simulation.
#' @param p Dimension.
#' @return For \eqn{n = 1}: a numeric vector. For \eqn{n > 1}: an
#'   \eqn{n \times p} matrix.
#' @seealso \code{\link{data.1}}, \code{\link{T2.2}}
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
#' datum <- data.1(20, 10, mu, Sigma)
#' # estat is the list with the values of the auxiliary statistics.
#' estat <- stats(datum, 20, 10, 2)
#' datum2 <- data.2(estat, 10, p = 2)
#' 
data.2 <- function(estat, n, delta = 0, p)
{
    N <- mvrnorm(n, c(estat[[1]]) + delta, matrix(c(estat[[2]]), p, p))
}
