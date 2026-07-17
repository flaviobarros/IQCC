#' Phase II Hotelling T² Control Chart
#'
#' Build or update a Phase II T² chart. Can optionally display Phase I
#' reference data. Limits: For \eqn{n = 1}:
#' \eqn{UCL = (p(m+1)(m-1))/(m^2-mp) \times qf(1-\alpha, p, m-p)}.
#' For \eqn{n > 1}:
#' \eqn{UCL = (p(m+1)(n-1))/(mn-m-p+1) \times qf(1-\alpha, p, mn-m-p+1)}.
#'
#' @param T2II Vector with Phase II T² values (from \code{\link{T2.2}}).
#' @param m Number of Phase I subgroups.
#' @param n Subgroup size.
#' @param j Index of the current Phase II sample (1-based).
#' @param t Maximum x-axis limit (number of Phase II samples to show).
#' @param p Number of variables.
#' @param datum Optional. Phase I data array to display reference points.
#' @param stats Optional. Phase I statistics to compute T² if not provided.
#' @param T2 Optional. Pre-computed Phase I T² values.
#' @return Draws the chart. Returns nothing.
#' @section Phase convention:
#'   Phase II monitoring. The vertical dashed line separates Phase I (left) from
#'   Phase II (right).
#' @seealso \code{\link{T2.2}}, \code{\link{cchart.T2.1}}
#' @references Montgomery, D.C., (2009). "Introduction to Statistical Quality
#' Control". Chapter 11. Wiley.
#' @importFrom graphics plot title mtext abline axis lines
#' @importFrom stats qf
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
#' datum <- data.1(20, 10, mu, Sigma)
#' estat <- stats(datum, 20, 10, 2)
#' datum2 <- data.2(estat, 10, p = 2)
#' T2II <- T2.2(datum2, estat, 10)
#' # For the first sample j = 1. T2II is a vector with the value of the first T2 statistic.
#' cchart.T2.2(T2II, 20, 10, 1, 25, 2)
#' # Same of the above, but now showing the phase I data set.
#' cchart.T2.2(T2II, 20, 10, 1, 25, 2, datum = datum)
#' 
#' #Example with individual observations
#' datum <- data.1(50, 1, mu, Sigma)
#' estat <- stats(datum, 50, 1, 2)
#' datum2 <- data.2(estat, 1, p = 2)
#' T2II <- T2.2(datum2, estat, 1)
#' # For the first sample j = 1. T2II is a vector with the value of the first T2 statistic.
#' cchart.T2.2(T2II, 50, 1, 1, 25, 2)
#' # Same of the above, but now showing the phase I data set.
#' cchart.T2.2(T2II, 50, 1, 1, 25, 2, datum = datum)
#' 
#' 
cchart.T2.2 <- function(T2II, m, n, j, t, p, datum = NULL, stats = NULL, T2 = NULL)
{
    if(n == 1)
        UCL <- ((p * (m + 1) * (m - 1)) / ((m ^ 2) - m * p)) * qf(1 - ALPHA, p, m - p)
    if(n > 1)
        UCL <- ((p * (m + 1) * (n - 1))/(m * n - m - p + 1)) * qf(1 - ALPHA, p, m * n - m - p + 1)

    old <- FALSE
    if(!is.null(T2))
    {
        plot(c(1:m, j + m + 1), c(T2, T2II[1]), ylim = c(0, UCL + 1), xlim = c(1, t + m + 1), ylab = "T2", xlab = "Sample", pch = 16, xaxt = 'n')
        old <- TRUE
    }
    else
    {
        if(is.null(T2) && !is.null(stats))
        {
            T2 <- T2.1(stats, m, n)
            plot(c(1:m, j + m + 1), c(T2, T2II[1]), ylim = c(0, UCL + 1), xlim = c(1, t + m + 1), ylab = "T2", xlab = "Sample", pch = 16, xaxt = 'n')
            old <- TRUE
        }
        else
        {
            if(is.null(T2) && is.null(stats) && !is.null(datum))
            {
                stats <- stats(datum, m, n, p)
                T2 <- T2.1(stats, m, n)
                plot(c(1:m, j + m + 1), c(T2, T2II[1]), ylim = c(0, UCL + 1), xlim = c(1, t + m + 1), ylab = "T2", xlab = "Sample", pch = 16, xaxt = 'n')
                old <- TRUE
            }
            else
                if(is.null(T2) && is.null(stats) && is.null(datum))
                    plot(j, T2II[1], ylim = c(0, UCL + 1), xlim = c(1, t), ylab = "T2", xlab = "Sample", pch = 16)
        }
    }
    if(n == 1)
        title("Hotelling T2: Individual Observations - Phase II")
    if(n > 1)
        title("Hotelling T2: Subgroup Observations - Phase II")

    mtext("UCL", side = 4, outer = FALSE, at = UCL, padj = 0, col = 'red', font = 2)
    abline(h = UCL, lty = 2, col = 'red')
    if(old)
    {
        axis(1, at = 1:m, labels = 1:m)
        axis(1, at = (m+2):(m+t+1), labels = 1:t)
        lines(T2)
        abline(v = m + 1)
    }
}
