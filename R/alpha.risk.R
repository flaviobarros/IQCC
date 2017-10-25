#' False Alarm probability for the 3-sigma R chart.
#' 
#' Used to calculate the real probability of false alarm in the 3-sigma R
#' chart.
#' 
#' This alpha risk is calculated under the exact R statistics distribution and
#' its values for small sample sizes will be much larger than the reference
#' value 0,0027.
#' 
#' @param n The sample size.
#' @return Return the value of the alpha risk for a given sample size n.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{d2},\link{d3},\link{c4}
#' @examples
#' 
#' alpha.risk(15)
#' 
alpha.risk <- function(n)
{
    D1 <- function(n)
    {
        d1 <- max(0, d2(n) - 3 * d3(n))
        return(d1)
    }
    D2 <- function(n)
    {
        D2 <- d2(n) + 3 * d3(n)
        return(D2)
    }
    risco <- function(n)
    {
        risco <- 1 - (ptukey(D2(n), n, Inf) - ptukey(D1(n), n, Inf))
        return(risco)
    }
    risk <- rep(0, length(n))
    for(i in 1:length(n))
        risk[i] <- risco(n[i])
    return(risk)
}
