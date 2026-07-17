#' X-bar Control Chart for Phase I and Phase II
#'
#' Builds and displays an X-bar control chart for Phase I (retrospective
#' analysis), Phase II (monitoring of future production), or both phases
#' in a single call. When Phase I data is supplied without Phase II data,
#' the chart uses estimated control limits. If Phase II data is supplied
#' without Phase I data, the chart requires \code{x2bars} and \code{sigma}
#' as reference values.
#'
#' @param x1 Phase I data. A matrix or data frame where each row is a subgroup.
#' @param n1 Phase I subgroup size(s). A single integer (equal sizes) or a vector.
#' @param x2 Phase II data. Same structure as x1.
#' @param n2 Phase II subgroup size(s).
#' @param x2bars Mean of subgroup means from Phase I (center line). Can be NULL and computed from x1.
#' @param sigma Standard deviation from Phase I (for control limits). Can be NULL and computed from x1.
#' @return Draws the X-bar control chart using \code{\link[qcc]{qcc}}. For Phase I only, invisibly returns a list with components \code{x2bar} (mean of means) and \code{sigma} (standard deviation).
#' @section Phase convention:
#' Phase I when only Phase I data is supplied; limits are estimated from
#' the data. Phase II when Phase II data is supplied (with or without
#' Phase I reference data).
#' @seealso \code{\link{cchart.Xbar1}}, \code{\link{cchart.Xbar2}}, \code{\link{cchart.Xbar_R}}
#' @references Montgomery, D.C., (2009). "Introduction to Statistical Quality Control". Chapter 6. Wiley.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @importFrom qcc qcc sd.xbar
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