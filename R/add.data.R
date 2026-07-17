#' Add a New Observation to a Phase II T² Chart
#'
#' Add a new Phase II point to an existing Hotelling T² chart. Computes the T²
#' statistic via \code{\link{T2.2}} and plots it. Use after
#' \code{\link{cchart.T2.2}} to build the chart incrementally.
#'
#' To use this function it is necessary to have the output given by the
#' function \code{\link{T2.2}}. At every step you should enter the new data set.
#'
#' @param datum2 New Phase II data (vector for \eqn{n = 1}, matrix for
#'   \eqn{n > 1}).
#' @param estat Phase I statistics (list from \code{\link{stats}}).
#' @param T2II Previous T² vector (updated in calling code).
#' @param n Subgroup size. Use \eqn{n = 1} for individual observations,
#'   \eqn{n > 1} for subgroups.
#' @param j Index of this new sample.
#' @param m Optional. Number of Phase I samples (needed if Phase I data is
#'   shown).
#' @return Invisibly, the new T² statistic.
#' @seealso \code{\link{T2.2}}, \code{\link{cchart.T2.2}}
#' @importFrom graphics points lines
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
#' datum <- data.1(20, 10, mu, Sigma)
#' estat <- stats(datum, 20, 10, 2)
#' datum2 <- data.2(estat, 10, p = 2)
#' T2II <- T2.2(datum2, estat, 10)
#' #Not showing the phase I data set.
#' cchart.T2.2(T2II, 20, 10, 1, 25, 2)
#' datum3 <- data.2(estat, 10, p = 2)
#' add.data(datum3, estat, T2II, 10, 2)
#' #Showing the phase I data set.
#' cchart.T2.2(T2II, 20, 10, 1, 25, 2, datum = datum)
#' datum3 <- data.2(estat, 10, p = 2)
#' add.data(datum3, estat, T2II, 10, 2, 20)
#' 
#' #Example with individual observations
#' datum <- data.1(50, 1, mu, Sigma)
#' estat <- stats(datum, 50, 1, 2)
#' datum2 <- data.2(estat, 1, p = 2)
#' T2II <- T2.2(datum2, estat, 1)
#' #Not showing the phase I data set.
#' cchart.T2.2(T2II, 50, 1, 1, 25, 2)
#' datum3 <- data.2(estat, 1, p = 2)
#' add.data(datum3, estat, T2II, 1, 2)
#' #Showing the phase I data set.
#' cchart.T2.2(T2II, 50, 1, 1, 25, 2, datum = datum)
#' datum3 <- data.2(estat, 1, p = 2)
#' add.data(datum3, estat, T2II, 1, 2, 50)
#' 
add.data <- function(datum2, estat, T2II, n, j, m = NULL)
{
    b <- T2.2(datum2, estat, n)
    if(!is.null(m))
        j <- j + m + 1
    points(j, b[1], pch = 16)
    c <- c(j, j - 1)
    d <- c(b[1], T2II[1])
    lines(c, d)
invisible(b)
}
