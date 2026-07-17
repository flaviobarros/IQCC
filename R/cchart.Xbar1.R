#' Phase I X-bar Shewhart Control Chart
#' 
#' Builds the X-bar control chart for Phase I retrospective analysis. The
#' control limits are estimated from the supplied data. Even if the data is
#' not normal, the X-bar statistic is approximately normal by the central
#' limit theorem when subgroup sizes are sufficiently large.
#' 
#' @param x Phase I data. Matrix or data frame with subgroups in rows.
#' @param sizes Subgroup size(s). A single integer (equal sizes) or a vector.
#' @return A list with components \code{x2bar} (mean of subgroup means) and \code{sigma} (standard deviation), returned invisibly. The control chart is drawn as a side effect.
#' @section Phase convention:
#' Phase I \code{---} control limits estimated from the data.
#' @seealso \code{\link{cchart.Xbar2}}, \code{\link{cchart.Xbar}}, \code{\link{cchart.Xbar_R}}
#' @references Montgomery, D.C., (2009). "Introduction to Statistical Quality Control". Chapter 6. Wiley.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @importFrom qcc qcc sd.xbar
#' @examples
#' 
#' data(pistonrings)
#' cchart.Xbar1(pistonrings[1:25, ], 5)
#' 
cchart.Xbar1 <- function(x, sizes)
{
    a <- rowMeans(x)
    x2bar <- mean(a)
    sigma <- sd.xbar(x)
    qcc(x, type = "xbar", sizes)
    stat <- list(c(x2bar, sigma))
    invisible(stat)
}
