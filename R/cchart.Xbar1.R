#' X-bar Shewhart Control Chart for phase I.
#' 
#' Builds the x-bar control chart for phase I.
#' 
#' Even if the data is not normal the x-bar statistic will be close to the
#' normal by the central limit theorem.
#' 
#' @param x The data to be plotted.
#' @param sizes A value or a vector of values specifying the sample sizes
#' associated with each group.
#' @return Return a x-bar control chart for phase I.
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{cchart.Xbar2}
#' @examples
#' 
#' data(pistonrings)
#' cchart.Xbar1(pistonrings[1:25, ])
#' 
cchart.Xbar1 <- function(x, sizes)
{
    a <- rowMeans(x)
    x2bar <- mean(a)
    sigma <- sd.xbar(x)
    qcc(x, type = "xbar", sizes)
	stat <- list(c(x2bar, sigma))
    return(stat)
}
