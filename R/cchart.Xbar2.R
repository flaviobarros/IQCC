#' X-bar Shewhart Control Chart for phase II.
#' 
#' Builds the x-bar control chart for phase II.
#' 
#' To use this function it is necessary to have the output given by the
#' function XbarI.
#' 
#' @param x The data to be plotted.
#' @param x2bar The mean of means.
#' @param sigma The standar deviation of the data.
#' @param sizes A value or a vector of values specifying the sample sizes
#' associated with each group.
#' @return Return a x-bar control chart for phase II.
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{cchart.Xbar1}
#' @examples
#' 
#' data(pistonrings)
#' stat <- cchart.Xbar1(pistonrings[1:25, ])
#' cchart.Xbar2(pistonrings[26:40, ], stat[[1]][1], stat[[1]][2])
#' 
cchart.Xbar2 <- function(x, x2bar, sigma, sizes)
{
    qcc(x, type = "xbar", center = x2bar, std.dev = sigma)
}
