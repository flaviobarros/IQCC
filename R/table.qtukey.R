#' Tukey Quantile Table
#' 
#' Builds a table with quantiles of the sample relative range distribution.
#' 
#' 
#' @param alpha The probability of type-I error of false alarm , that is equal
#' to 1 minus the confidence level.
#' @param n The maximum sample size.
#' @return Returns a matrix with 4 columns containing the quantiles, printed to
#' the console and returned invisibly.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{table.const},\link{alpha.risk},\link{qtukey}
#' @importFrom stats qtukey
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
        u[i, ] <- c(qtukey(alpha / 2, i, Inf),
                     qtukey(alpha, i, Inf),
                     qtukey(1 - alpha, i, Inf),
                     qtukey(1 - (alpha / 2), i, Inf))
    }
    y <- u[2:n, ]
    rownames(y) <- as.character(2:n)
    print(y)
    invisible(y)
}
