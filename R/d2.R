#' D2 Constant.
#' 
#' This function is used to calculate the mean of the sample relative range (W
#' statistic).
#' 
#' 
#' @param n The sample size.
#' @return Return the value of d2 for a given sample size n.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{d3},\link{c4}
#' @importFrom stats ptukey integrate
#' @examples
#' 
#' d2(8)
#' 
d2 <- function(n)
{
    if(any(n < 2))
        stop("n must be >= 2")
    d <- numeric(length(n))
    for(i in seq_along(n))
    {
        int <- integrate(function(w) {1 - ptukey(w, n[i], Inf)}, 0, Inf)
        d[i] <- int[[1]]
    }
    return(d)
}
