#' Simulate Phase I Multivariate Normal Data
#'
#' Generate \eqn{m} samples from a \eqn{p}-variate normal distribution for
#' Phase I Hotelling T² chart development. For \eqn{n = 1}: returns an
#' \eqn{m \times p} matrix. For \eqn{n > 1}: returns an
#' \eqn{n \times p \times m} array.
#'
#' @param m Number of samples.
#' @param n Sample size per subgroup. Use \eqn{n = 1} for individual
#'   observations, \eqn{n > 1} for subgroups.
#' @param mu Mean vector (length \eqn{p}).
#' @param Sigma \eqn{p \times p} covariance matrix.
#' @return For \eqn{n = 1}: an \eqn{m \times p} matrix. For \eqn{n > 1}: an
#'   \eqn{n \times p \times m} array.
#' @section Simulation:
#'   Uses \code{MASS::mvrnorm()}. Set the seed externally for reproducibility.
#' @seealso \code{\link{data.2}}, \code{\link{T2.1}}
#' @importFrom MASS mvrnorm
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
#' #Simulated data with individual observations
#' datum <- data.1(50, 1, mu, Sigma)
#' #Simulated data with sub-group observations
#' datum <- data.1(20, 10, mu, Sigma)
#' 
data.1 <- function(m, n, mu, Sigma)
{
    p <- dim(Sigma)[1]
    if(n == 1)
    {
        u <- matrix(nrow = m, ncol = p)
        for(i in 1:m)
        {
            N <- mvrnorm(n, mu, Sigma)
            u[i, ] <- N
        }
    }
    if(n > 1)
    {
        u <- array(dim = c(n, p, m))
        for(i in 1:m)
        {
            N <- mvrnorm(n, mu, Sigma)
            u[, , i] <- N 
        }
    }
    return(u)
}
