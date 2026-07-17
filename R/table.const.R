#' Table of Control Chart Constants d2, d3, and c4
#'
#' Build a matrix with the constants \eqn{d_2}, \eqn{d_3}, and \eqn{c_4} for
#' sample sizes \eqn{n = 2, 3, \ldots,} the specified maximum.
#'
#' The three constants are those returned by \code{\link{d2}},
#' \code{\link{d3}}, and \code{\link{c4}} respectively. They are the standard
#' Shewhart chart constants for the relative range and the standard deviation
#' under normality.
#'
#' @param n Maximum sample size (must be \eqn{\ge 2}). The table includes rows
#'   for \eqn{n = 2, 3, \ldots, n}.
#' @return A matrix with \eqn{n - 1} rows and 3 columns named \code{"d2"},
#'   \code{"d3"}, \code{"c4"}. Row names are the sample sizes.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @section Errors:
#' Stops if \code{n < 2}, raised by the internal \code{\link{d2}} call.
#' @references
#' Montgomery, D. C. (2009). \emph{Introduction to Statistical Quality Control},
#' 6th ed. Wiley.
#' @seealso \code{\link{d2}}, \code{\link{d3}}, \code{\link{c4}}
#' @examples
#'
#' table.const(10)
#' table.const(5)
#'
table.const <- function(n)
{
    n <- 2:n
    u <- matrix(c(d2(n), d3(n), c4(n)), max(n) - 1, 3, byrow = FALSE)
    colnames(u) <- c("d2", "d3", "c4")
    rownames(u) <- n
    return(u)    
}
