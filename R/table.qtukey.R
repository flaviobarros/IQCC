#' Tukey Quantile Table
#' 
#' Builds a table with quantiles of the sample relative range distribution.
#' 
#' 
#' @param alpha The probability of type-I error of false alarm , that is equal
#' to 1 minus the confidence level.
#' @param n The maximum sample size.
#' @return It is used the fact that the sample relative range distribution is
#' the same as the sample studentized range distribution (tukey distribution)
#' with infinity d.f. in the denominator . It is considered 4 quantiles:
#' alpha/2 , alpha , 1-alpha and 1-alpha/2, for different sample size values .
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{table.const},\link{alpha.risk},\link{qtukey}
#' @examples
#' 
#' table.qtukey(0.0027, 15)
#' 
table.qtukey <- function(alpha, n)
{
    u <- matrix(nrow = n, ncol = 4)
    colnames(u) <- c("alpha/2", "alpha", "1-alpha", "1-alpha/2")
    for(i in 2:n)
    {
        a <- function(i) {
            qtukey(alpha / 2, i, Inf)
        }
        b <- function(i) {
            qtukey(alpha, i, Inf)
        }
        g <- function(i) {
            qtukey(1 - alpha, i, Inf)
        }
        d <- function(i) {
            qtukey(1 - (alpha / 2), i, Inf)
        }
        u[i, ] <- c(a(i), b(i), g(i), d(i))
    }
    y <- u[2:n, ]
    rownames(y) <- c(2:n)
    print(y)
}
