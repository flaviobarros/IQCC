#' S Control Chart.
#' 
#' This function builds a S control chart.
#' 
#' The exact limits are the alpha/2 and 1-alpha/2 quantiles of the S
#' distribution which is proportional to the square root of a chi-square
#' distribution.
#' 
#' @param x The data to be plotted.
#' @param type A character string specifying the type of S control chart to be
#' plotted where "n" plots a S chart with normalized probability limits and "e"
#' plots a S chart with exact limits.
#' @param m The sample sizes. Only necessary in the control chart with exact
#' (probability) limits.
#' @return Return a S control chart.
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' data(softdrink)
#' #S chart with normalized probability limits
#' cchart.S(softdrink, type = "n")
#' #S chart with exact probability limits
#' cchart.S(softdrink, type = "e", 10)
#' 
cchart.S <- function(x, type = "n", m = NULL)
{
    if(type == "n")
        qcc(x, type = "S")
    else
    {
        if(type == "c" && !is.null(m))
            qcc(x, type = "S", limits = c((sqrt(qchisq(0.00135, m - 1) / (m - 1))) * sd.S(x),(sqrt(qchisq(0.99865, m - 1) / (m - 1))) * sd.S(x)))
        else
            if(type == "c" && is.null(m))
            {
                sprintf("WARNING: The sample size m wasn't specified, so a S ??? control chart was plotted instead.")
                qcc(x, type = "S")
            }
    }
}
