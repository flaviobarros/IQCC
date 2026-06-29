#' X-bar Control Chart for phase I and II.
#'
#' Builds the x-bar control chart for phase I or phase II.
#'
#' @param x1 The phase I data to be plotted.
#' @param n1 A value or a vector of values specifying the sample sizes
#' associated with each group for the phase I data.
#' @param x2 The phase II data to be plotted.
#' @param n2 A value or a vector of values specifying the sample sizes
#' associated with each group for the phase II data.
#' @param x2bars The mean of means from phase I.
#' @param sigma The standard deviation from phase I.
#' @return Return an x-bar control chart.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{cchart.Xbar1}, \link{cchart.Xbar2}
#' @examples
#'
#' data(pistonrings)
#' cchart.Xbar(x1 = pistonrings[1:25, ], n1 = 5)
#' cchart.Xbar(x1 = pistonrings[1:25, ], n1 = 5, x2 = pistonrings[26:40, ], n2 = 5)
#'
cchart.Xbar <- function(x1 = NULL, n1 = NULL, x2 = NULL, n2 = NULL, x2bars = NULL, sigma = NULL)
{
    if(!is.null(x1) && !is.null(n1))
        OK1 = TRUE
    else
        OK1 = FALSE
    if(!is.null(x2) && !is.null(n2) && (OK1 || (!is.null(x2bars) && !is.null(sigma))))
        OK2 = TRUE
    else
        OK2 = FALSE
#-- Error messages
    if(!OK1 && !OK2)
    {
        if(is.null(x1) && is.null(n1))
            stop("Phase I data and samples sizes are missing")
        else
        {
            if(!is.null(n1))
                stop("Phase I data is missing")
            else
                stop("Phase I samples sizes not specified")
        }
    }
    if(!OK2)
    {
        if(is.null(x2) && !is.null(n2))
            stop("Phase II data is missing")
        if(!is.null(x2) && is.null(n2))
            stop("Phase II samples sizes not specified")
    }

#-- Phase I
    if(OK1 && !OK2)
    {
        a <- rowMeans(x1)
        x2bars <- mean(a)
        sigma <- sd.xbar(x1)
        qcc(x1, type = "xbar", n1)
    }
#-- Phase II
    if(OK2)
    {
        if(is.null(x2bars))
        {
            a <- rowMeans(x1)
            x2bars <- mean(a)
        }
        if(is.null(sigma))
            sigma <- sd.xbar(x1)
        qcc(x2, type = "xbar", center = x2bars, std.dev = sigma)
    }
}