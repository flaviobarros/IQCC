#' Remove an undesirable observation.
#' 
#' This function removes an undesirable data that might be out of control in
#' you data set. It is used at Hotelling T2 control charts for phase I.
#' 
#' 
#' @param datum The data set. Should be an array.
#' @param i The index in the matrix of the data to be removed.
#' @return Return the new data set without the observation that was removed.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- symMatrix(c(3.770, -5.495, 13.53), 2)
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
