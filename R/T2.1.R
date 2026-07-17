#' Hotelling T² Statistic for Phase I
#'
#' Calculate the Hotelling T² statistic for Phase I using estimated mean vector
#' and covariance matrix. For individual observations (\eqn{n = 1}):
#' \eqn{T^2 = x_i' S^{-1} x_i}. For subgroups (\eqn{n > 1}):
#' \eqn{T^2_i = n (\bar{x}_i - \bar{\bar{x}})' S^{-1} (\bar{x}_i - \bar{\bar{x}})}.
#'
#' Before using this function it is necessary to execute \code{\link{stats}}
#' (that calculates the auxiliary statistics involved in the T² formula) and
#' the function \code{\link{data.1}} (or other way to supply the data).
#'
#' @param estat A list returned by \code{\link{stats}} with three components:
#'   \code{[[1]]} grand mean vector, \code{[[2]]} pooled covariance matrix,
#'   \code{[[3]]} matrix of subgroup means (\eqn{m \times p}).
#' @param m Number of subgroups (Phase I sample size).
#' @param n Subgroup size. Use \eqn{n = 1} for individual observations,
#'   \eqn{n > 1} for subgroups.
#' @return A numeric vector of length \eqn{m} with the T² statistics.
#' @section Phase convention:
#'   Phase I \code{---} uses parameters estimated from the same data being
#'   plotted. The T² statistics are not independent.
#' @section Errors:
#'   Stop if \eqn{n < 1}.
#' @section Decision rule:
#'   A Phase I point signals when T² exceeds the UCL from
#'   \code{\link{cchart.T2.1}}.
#' @seealso \code{\link{T2.2}}, \code{\link{cchart.T2.1}},
#'   \code{\link{stats}}, \code{\link{data.1}}
#' @references Montgomery, D.C., (2009). "Introduction to Statistical Quality
#' Control". Chapter 11. Wiley.
#' @importFrom miscTools symMatrix
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
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
