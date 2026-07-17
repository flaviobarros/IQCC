#' Tukey Quantile Table for the Relative Range
#'
#' Print a table of quantiles of the studentized range distribution
#' \eqn{W = R / \sigma} for sample sizes \eqn{n = 2} through the specified
#' maximum, using \code{\link[stats]{qtukey}} with infinite denominator degrees
#' of freedom.
#'
#' Four tail probabilities are tabulated for each \eqn{n}: \eqn{\alpha/2},
#' \eqn{\alpha}, \eqn{1 - \alpha}, and \eqn{1 - \alpha/2}. The default examples
#' use \eqn{\alpha = 0.0027} (US convention) and \eqn{\alpha = 0.0020}
#' (European convention).
#'
#' @param alpha Type-I error probability (\eqn{0 < \alpha < 1}).
#' @param n Maximum sample size (\eqn{\ge 2}).
#' @return Invisibly, a matrix with \eqn{n - 1} rows and 4 columns. The matrix
#'   is also printed to the console.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @section Errors:
#' Stops if \code{n < 2} or if \code{alpha} is not strictly between 0 and 1.
#' @references
#' Barbosa, E. P., Gneri, M. A. and Meneguetti, A. (2013). On the the
#' evaluation of the relative range distribution. \emph{Communications in
#' Statistics - Simulation and Computation}, 42, 1311--1339.
#' \doi{10.1080/03610918.2011.639967}.
#' @seealso \code{\link{alpha.risk}}, \code{\link{cchart.R}}
#' @importFrom stats qtukey
#' @examples
#'
#' table.qtukey(0.0027, 15)
#' table.qtukey(0.0020, 10)
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
