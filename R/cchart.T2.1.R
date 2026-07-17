#' Phase I Hotelling Control Chart.
#' 
#' Builds the phase I Hotelling control chart.
#' 
#' It builds the Hotelling T2 control chart for multivariate normal data (m
#' samples / samples of size n > 1), used retrospective / validation analysis
#' (phase I); the control limits are based on the F distribution.
#' 
#' @param T2 The values of the T2 statistic. Should be a numeric vector.
#' @param m The number of samples generated previously in data.1.
#' @param n The size of each sample used previously in data.1. If they are
#' individual observations, then use n = 1.
#' @param p The dimension used previously in function data.1.
#' @return Return a control chart.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{cchart.T2.2}, \link{robustez}
#' @references Montgomery, D.C.,(2008)."Introduction to Statistical Quality
#' Control". Chapter 11. Wiley
#' @importFrom graphics plot lines mtext abline axis title
#' @importFrom stats qbeta qf
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
