#' X-bar and R Control Charts
#' 
#' Draws the X-bar control chart and the R (range) control chart side by
#' side in the same graphics window, using estimated Phase I control limits.
#' Both charts share the same subgroup data.
#' 
#' @param x Phase I data. Matrix or data frame with subgroups in rows.
#' @param sizes Subgroup size(s). A single integer (equal sizes) or a vector.
#' @return Draws two control charts side by side. Returns nothing.
#' @section Phase convention:
#' Phase I \code{---} control limits estimated from the data for both charts.
#' @seealso \code{\link{cchart.Xbar1}}, \code{\link{cchart.Xbar2}}, \code{\link{cchart.Xbar}}
#' @references Montgomery, D.C., (2009). "Introduction to Statistical Quality Control". Chapter 6. Wiley.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @importFrom graphics par
#' @importFrom qcc qcc
#' @examples
#' 
#' data(pistonrings)
#' attach(pistonrings)
#' cchart.Xbar_R(pistonrings[1:25, ], 5)
#' 
cchart.Xbar_R <- function(x, sizes)
{
    par(mfrow = c(1, 2))                 # setup 1 row and 2 columns for plotting
    qcc(x, type = "xbar", add.stats = FALSE)
    qcc(x, type = "R", add.stats = FALSE)
}
