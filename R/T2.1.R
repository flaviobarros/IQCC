#' Hotelling T2 Statistic for Phase I.
#' 
#' Calculate the Hotelling T2 statistic for multivariate observations at phase
#' I , to be used to build the corresponding control chart.
#' 
#' Before using this function it is necessary to execute the function
#' "stats"(that calculate the auxiliary statistics involved in the T2 formula)
#' and the function "data.1" (or other way to supply the data).
#' 
#' @param estat The values of the auxiliary statistics. Should be a list with a
#' matrix with the means, mean of the means and mean of the standard deviation.
#' @param m The number of samples generated previously in data.1.
#' @param n The size of each samples used previously in data.1.
#' @return Return a vector with the Hotelling T2 statistics.
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{stats}, \link{data.1}, \link{cchart.T2.1}
#' @references Montgomery, D.C.,(2008)."Introduction to Statistical Quality
#' Control". Chapter 11. Wiley.
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- symMatrix(c(3.770, -5.495, 13.53), 2)
#' #Example with individual observations
#' datum <- data.1(50, 1, mu, Sigma)
#' estat <- stats(datum, 50, 1, 2) 
#' T2.1(estat, 50, 1)
#' #Example with sub group observations
#' datum <- data.1(20, 10, mu, Sigma)
#' estat <- stats(datum, 20, 10, 2) 
#' T2.1(estat, 20, 10)
#' 
T2.1 <- function(estat, m, n)
{
    t2 <- vector()
    if(n == 1)
    {
        for (i in 1:m)
        {
            T2 <- (t(estat[[3]][i, ]) %*% solve(estat[[2]]) %*% (estat[[3]][i, ]))
            t2 <- c(t2, T2)
        }
    }
    if(n > 1)
    {
        for (i in 1:m)
        {
            T2 <- n * (t(estat[[3]][i, ] - estat[[1]]) %*% solve(estat[[2]]) %*% (estat[[3]][i, ] - estat[[1]]))
            t2 <- c(t2, T2)
        }
    }
    return(t2)
}
