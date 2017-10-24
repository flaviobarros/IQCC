#' X-bar and R control charts
#' 
#' This function builds the X-bar and R control charts in the same window.
#' 
#' 
#' @param x The data to be plotted.
#' @param sizes A value or a vector of values specifying the sample sizes
#' associated with each group.
#' @return Return the two control charts.
#' @author Daniela R. Recchia, Emanuel P. Barbosa.
#' @examples
#' 
#' data(pistonrings)
#' attach(pistonrings)
#' cchart.Xbar_R(pistonrings[1:25, ])
#' 
cchart.Xbar_R <- function(x, sizes)
{
    par(mfrow = c(1, 2))                 # setup 1 row and 2 columns for plotting
    qcc(x, type = "xbar", add.stats = F)
    qcc(x, type = "R", add.stat = F)
}
