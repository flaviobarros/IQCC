#' D3 Constant.
#' 
#' This function is used to calculate the standard deviation of the sample
#' relative range (W statistic).
#' 
#' 
#' @param n The sample size.
#' @return Return the value of d3 for a given sample size n.
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{d2},\link{c4}
#' @examples
#' 
#' d3(7)
#' 
d3 <- function(n)
{
    d2 <- d2(n)
    e <- vector()
    for(i in 1:length(n))
    {
        int <- integrate(function(w) { w * (1 - ptukey(w, n[i], Inf)) }, 0, Inf)
        e <- append(e, sqrt(2 * int[[1]] - (d2[1]) ^ 2))
    }
    return(e)
}
