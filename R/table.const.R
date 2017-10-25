#' Table of values for the constants d2, d3 and c4.
#' 
#' This function is used to build a table of values for the constants d2, d3
#' and c4 for sucessive values of sample size n.
#' 
#' It builds a table in matrix form with 3 columns (one for each constant) and
#' one row for each value of n from 2 to a specified value.
#' 
#' @param n The maximum size.
#' @return Return the values of these three constants.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{d2},\link{d3},\link{c4}
#' @examples
#' 
#' table.const(17)
#' 
table.const <- function(n)
{
    n <- 2:n
    u <- matrix(c(d2(n), d3(n), c4(n)), max(n) - 1, 3, byrow = FALSE)
    colnames(u) <- c("d2", "d3", "c4")
    rownames(u) <- n
    return(u)    
}
