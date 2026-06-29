#' D3 Constant.
#' 
#' This function is used to calculate the standard deviation of the sample
#' relative range (W statistic).
#' 
#' 
#' @param n The sample size.
#' @return Return the value of d3 for a given sample size n.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{d2},\link{c4}
#' @examples
#' 
#' d3(7)
#' 
d3 <- function(n)
{
    d2 <- d2(n)
    e <- numeric(length(n))
    for(i in seq_along(n))
    {
        int <- integrate(function(w) { w * (1 - ptukey(w, n[i], Inf)) }, 0, Inf)
        e[i] <- sqrt(2 * int[[1]] - (d2[i]) ^ 2)
    }
    return(e)
}
