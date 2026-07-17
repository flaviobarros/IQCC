#' Phase I Hotelling T² Control Chart
#'
#' Build a Phase I T² control chart. For \eqn{n = 1}:
#' \eqn{UCL = ((m-1)^2/m) \times qbeta(1-\alpha, p/2, (m-p-1)/2)}.
#' For \eqn{n > 1}:
#' \eqn{UCL = (p(m-1)(n-1))/(mn-m-p+1) \times qf(1-\alpha, p, mn-m-p+1)}.
#' The control limits are based on the beta and F distributions.
#'
#' @param T2 Numeric vector of T² statistics from \code{\link{T2.1}}.
#' @param m Number of subgroups in Phase I.
#' @param n Subgroup size. Use \eqn{n = 1} for individual observations,
#'   \eqn{n > 1} for subgroups.
#' @param p Number of variables (dimension).
#' @return Draws the chart. Returns nothing.
#' @section Phase convention:
#'   Phase I retrospective analysis. All points are plotted simultaneously.
#' @section Errors:
#'   Stop if \eqn{n < 1}.
#' @seealso \code{\link{T2.1}}, \code{\link{cchart.T2.2}}
#' @references Montgomery, D.C., (2009). "Introduction to Statistical Quality
#' Control". Chapter 11. Wiley.
#' @importFrom graphics plot lines mtext abline axis title
#' @importFrom stats qbeta qf
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
#' datum <- data.1(20, 10, mu, Sigma)
#' estat <- stats(datum, 20, 10, 2)
#' T2 <- T2.1(estat, 20, 10)
#' # estat is a list with the auxiliary statistics. T2 is a matrix with the values of the T2 statistic.
#' cchart.T2.1(T2, 20, 10, 2)
#' 
cchart.T2.1 <- function(T2, m, n, p)
{
    if(n < 1)
    {
        stop("n must be equal to or higher than 1.")
    }
    if(n == 1)
    {
        UCL <- (((m - 1) ^ 2) / m) * qbeta(1 - ALPHA, p / 2, (m - p - 1) / 2)
        plot(1:m, T2, main = "Hotelling T2: Individual Observations - Phase I", ylim = c(0, UCL + 1), ylab = "T2")
    }
    if(n > 1)
    {
        UCL <- ((p * (m - 1) * (n - 1)) / (m * n - m - p + 1)) * qf(1 - ALPHA, p, m * n - m - p + 1) 
        plot(1:m, T2, main = "Hotelling T2: Subgroup Observations - Phase I", ylim = c(0, UCL + 1), ylab = "T2")
    }
    lines(1:m, T2)
    mtext("UCL", side = 4, outer = FALSE, at = UCL, padj = 0, col = 'red', font = 2)
    abline(h = UCL, lty = 2, col = 'red')
}
