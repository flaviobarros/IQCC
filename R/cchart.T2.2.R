#' Phase II Hotelling Control Chart.
#' 
#' Builds the sub group phase II Hotelling control chart.
#' 
#' It builds the Hotelling T2 control chart for multivariate normal data to be
#' used in the operational phase (known as phase II); the control limits are
#' based on the F distribution.
#' 
#' @param T2II A vector with the value of T2 statistic for one sample.
#' @param m The number of samples generated previously in data.1.
#' @param n The size of each sample used previously in data.1. If they are
#' individual observations, use n = 1.
#' @param j The index of the current sample.
#' @param t The maximum value of the x axis.
#' @param p The dimension used previously in function data.1.
#' @param datum The data set used in phase I.
#' @param stats The auxiliary statistics created by the function stats.
#' @param T2 The Hotelling T2 statistic for multivariate observations at phase
#' I created by the function T2.1.
#' @return Return a control chart.
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{cchart.T2.1}
#' @references Montgomery, D.C.,(2008). "Introduction to Statistical Quality
#' Control". Chapter 11. Wiley
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- symMatrix(c(3.770, -5.495, 13.53), 2)
#' datum <- data.1(20, 10, mu, Sigma)
#' estat <- stats(datum, 20, 10, 2)
#' datum2 <- data.2(estat, 10, p = 2)
#' T2II <- T2.2(datum2, estat, 10)
#' # For the first sample j = 1. T2II is a vector with the value of the firts T2 statistic.
#' cchart.T2.2(T2II, 20, 10, 1, 25, 2)
#' # Same of the above, but now showing the phase I data set.
#' cchart.T2.2(T2II, 20, 10, 1, 25, 2, datum = datum)
#' 
#' #Example with individual observations
#' datum <- data.1(50, 1, mu, Sigma)
#' estat <- stats(datum, 50, 1, 2)
#' datum2 <- data.2(estat, 1, p = 2)
#' T2II <- T2.2(datum2, estat, 1)
#' # For the first sample j = 1. T2II is a vector with the value of the firts T2 statistic.
#' cchart.T2.2(T2II, 50, 1, 1, 25, 2)
#' # Same of the above, but now showing the phase I data set.
#' cchart.T2.2(T2II, 50, 1, 1, 25, 2, datum = datum)
#' 
#' 
cchart.T2.2 <- function(T2II, m, n, j, t, p, datum = NULL, stats = NULL, T2 = NULL)
{
    if(n == 1)
        UCL <- ((p * (m + 1) * (m - 1)) / ((m ^ 2) - m * p)) * qf(1 - 0.0027, p, m - p) 
    if(n > 1)
        UCL <- ((p * (m + 1) * (n - 1))/(m * n - m - p + 1)) * qf(1 - 0.0027, p, m * n - m - p + 1)

    old = FALSE
    if(is.null(T2) == FALSE)
    {
        plot(c(1:m, j + m + 1), c(T2, T2II[1]), ylim = c(0, UCL + 1), xlim = c(1, t + m + 1), ylab = "T2", xlab = "Sample", pch = 16, xaxt = 'n')
        old = TRUE
    }
    else
    {
        if(is.null(T2) && is.null(stats) == FALSE)
        {
            T2 <- T2.1(stats, m, n)
            plot(c(1:m, j + m + 1), c(T2, T2II[1]), ylim = c(0, UCL + 1), xlim = c(1, t + m + 1), ylab = "T2", xlab = "Sample", pch = 16, xaxt = 'n')
            old = TRUE
	  }
        else
        {
            if(is.null(T2) && is.null(stats) && is.null(datum) == FALSE)
            {
                stats <- stats(datum, m, n, p)
                T2 <- T2.1(stats, m, n)
                plot(c(1:m, j + m + 1), c(T2, T2II[1]), ylim = c(0, UCL + 1), xlim = c(1, t + m + 1), ylab = "T2", xlab = "Sample", pch = 16, xaxt = 'n')
                old = TRUE
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
    
    mtext("UCL", side = 4, outer = F, at = UCL , padj = 0, col = 'red', font = 2)
    abline(h = UCL, lty = 2, col = 'red')
    if(old == TRUE)
    {
        axis(1, at = 1:m, labels = 1:m)
        axis(1, at = (m+2):(m+t+1), labels = 1:t)
        lines(T2)
        abline(v = m + 1)
    }
}
