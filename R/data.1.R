#' Hotelling Control Chart Phase I simulated data.
#' 
#' This function simulate a normal data set to be used in the phase I Hoteliing
#' control charts.
#' 
#' 
#' @param m The number of samples to be generated.
#' @param n The size of each sample. If they are individual observations, then
#' use n = 1.
#' @param mu The vector with the means of the data to be generated.
#' @param Sigma The vector with the variance-covariance matrix of the data to
#' be generated.
#' @return Return an array with the simulated data.
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{data.2}
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- symMatrix(c(3.770, -5.495, 13.53), 2)
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
