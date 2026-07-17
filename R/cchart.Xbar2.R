#' Phase II X-bar Shewhart Control Chart
#' 
#' Builds the X-bar control chart for Phase II monitoring using reference
#' limits obtained from a Phase I analysis. The Phase I estimates of
#' \code{x2bar} (mean of means) and \code{sigma} (standard deviation) are
#' required, typically obtained from \code{\link{cchart.Xbar1}}.
#' 
#' @param x Phase II data. Matrix or data frame with subgroups in rows.
#' @param x2bar Mean of subgroup means from Phase I (center line).
#' @param sigma Standard deviation from Phase I (for control limits).
#' @param sizes Phase II subgroup size(s).
#' @return Draws the X-bar control chart. Returns nothing.
#' @section Phase convention:
#' Phase II \code{---} tests new data against Phase I reference distribution.
#' @seealso \code{\link{cchart.Xbar1}}, \code{\link{cchart.Xbar}}
#' @references Montgomery, D.C., (2009). "Introduction to Statistical Quality Control". Chapter 6. Wiley.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' data(pistonrings)
#' stat <- cchart.Xbar1(pistonrings[1:25, ], 5)
#' cchart.Xbar2(pistonrings[26:40, ], stat[[1]][1], stat[[1]][2], 5)
#' 
cchart.Xbar2 <- function(x, x2bar, sigma, sizes)
{
    qcc(x, type = "xbar", center = x2bar, std.dev = sigma)
}
