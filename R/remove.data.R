#' Remove an Observation from Phase I Data
#'
#' Remove the \eqn{i}-th subgroup from a Phase I data set (array or matrix).
#' Used during Phase I retrospective analysis to eliminate out-of-control
#' signals before recomputing reference limits.
#'
#' @param datum Phase I data (matrix for \eqn{n = 1}, 3D array for
#'   \eqn{n > 1}).
#' @param i Index of the subgroup to remove (1-based).
#' @return The data set without the \eqn{i}-th subgroup.
#' @seealso \code{\link{data.1}}, \code{\link{T2.1}}
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
#' datum <- data.1(20, 10, mu, Sigma)
#' # Removing the observatiob 13 from the data set "datum" and updating it:
#' datum <- remove.data(datum, 13)
#' 
remove.data <- function(datum, i)
{
    if(is.matrix(datum))
        datum <- datum[-i, ]
    else
        datum <- datum[, , -i]
    return(datum)
}
