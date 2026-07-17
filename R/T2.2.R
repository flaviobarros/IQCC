#' Hotelling T² Statistic for Phase II
#'
#' Calculate T² for new (Phase II) observations using Phase I reference
#' parameters. For individual observations:
#' \eqn{T^2 = (x_{new} - \bar{\bar{x}})' S^{-1} (x_{new} - \bar{\bar{x}})}.
#' For subgroups:
#' \eqn{T^2 = n (\bar{x}_{new} - \bar{\bar{x}})' S^{-1} (\bar{x}_{new} - \bar{\bar{x}})}.
#'
#' Before using this function it is necessary to execute \code{\link{stats}}
#' (that calculates the auxiliary statistics involved in the T² formula) and
#' the function \code{\link{data.2}} (or other way to supply the data).
#'
#' @param datum2 Phase II data. For \eqn{n = 1}: a numeric vector of length
#'   \eqn{p}. For \eqn{n > 1}: an \eqn{n \times p} matrix.
#' @param estat List from \code{\link{stats}} (Phase I estimates).
#' @param n Subgroup size. Use \eqn{n = 1} for individual observations,
#'   \eqn{n > 1} for subgroups.
#' @return A scalar T² statistic.
#' @section Phase convention:
#'   Phase II \code{---} tests new observations against Phase I reference
#'   distribution.
#' @seealso \code{\link{T2.1}}, \code{\link{cchart.T2.2}}
#' @references Montgomery, D.C., (2009). "Introduction to Statistical Quality
#' Control". Chapter 11. Wiley.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
#' #Example with individual observations
#' datum <- data.1(50, 1, mu, Sigma)
#' estat <- stats(datum, 50, 1, 2)
#' datum2 <- data.2(estat, 1, p = 2)
#' T2II <- T2.2(datum2, estat, 1)
#' #Example with subgroup observations
#' datum <- data.1(20, 10, mu, Sigma)
#' estat <- stats(datum, 20, 10, 2)
#' datum2 <- data.2(estat, 10, p = 2)
#' T2II <- T2.2(datum2, estat, 10)
#' 
T2.2 <- function(datum2, estat, n)
{
    if(n == 1)
    {
        T2 <- (t(datum2 - estat[[1]]) %*% solve(estat[[2]]) %*% (datum2 - estat[[1]]))
    }
    if(n > 1)
    {
        media <- colMeans(datum2)
        T2 <- n * (t(media - estat[[1]]) %*% solve(estat[[2]]) %*% (media - estat[[1]]))
    }
    return(T2)
}
