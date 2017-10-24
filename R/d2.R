#' D2 Constant.
#' 
#' This function is used to calculate the mean of the sample relative range (W
#' statistic).
#' 
#' 
#' @param n The sample size.
#' @return Return the value of d2 for a given sample size n.
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{d3},\link{c4}
#' @examples
#' 
#' d2(8)
#' 
d2 <- function(n)
{
    d <- vector()
    for(i in 1:length(n))
    {
        int <- integrate(function(w) {1 - ptukey(w, n[i], Inf)}, 0, Inf)
        d <- append(d, int[[1]])
    }
    return(d)
}
