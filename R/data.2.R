#' Hotelling Control Chart Phase II simulated data.
#' 
#' This function simulate a normal data set to be used in the phase II
#' Hotelling control charts.
#' 
#' To use this function it is necessary to have the information about the phase
#' I given by the functions data.1 and stats.
#' 
#' @param estat The values of the auxiliary statistics. Should be a list with a
#' matrix with the means, mean of the means and mean of the standard deviation.
#' @param n The size of each sample. If they are individual observations, use n
#' = 1.
#' @param delta A value to be added on the vector of means.
#' @param p The dimension.
#' @return Return an array with the simulated data.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{data.1}
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- symMatrix(c(3.770, -5.495, 13.53), 2)
#' datum <- data.1(20, 10, mu, Sigma)
#' # estat is the list with the values of the auxiliary statistics.
#' estat <- stats(datum, 20, 10, 2)
#' datum2 <- data.2(estat, 10, p = 2)
#' 
data.2 <- function(estat, n, delta = 0, p)
{
    N <- mvrnorm(n, c(estat[[1]]) + delta, matrix(c(estat[[2]]), p, p))
}
