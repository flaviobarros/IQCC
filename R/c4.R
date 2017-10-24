#' C4 Constant.
#' 
#' This function is used to calculate the bias correction constant c4 for the
#' sample standard deviation statistic.
#' 
#' It is used to correct the bias for small sample sizes in the sample standard
#' deviation statistic.
#' 
#' @param n The sample size.
#' @return Return the value of c4 for a given sample size n.
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{d2},\link{d3}
#' @examples
#' 
#' c4(5)
#' 
c4 <- function(n)
{
    c <- (sqrt(2 / (n-1))) * (gamma(n/2) / gamma((n-1) / 2))
    return(c)
}
