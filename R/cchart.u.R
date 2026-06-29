#' u-chart
#' 
#' This function builds a u-chart for the Poisson-based count data statistic.
#' 
#' For a phase I u-chart, n1 must be specified and either x1 or u1.  For a
#' phase II u-chart, n2 must be specified, plus x2 or u2 and either phat, x1
#' and n1, or u1 and n1.  It is important to note that the normal approximation
#' used in the Shewhart u-chart is valid only for n*u large. For small n*p , it
#' should be used an "improved u chart" (with non-normal correction) given by
#' using the argument "CF".
#' 
#' @param x1 The phase I data that will be plotted (if it is a phase I chart).
#' @param n1 A value or a vector of values specifying the sample sizes
#' associated with each group for the phase I data.
#' @param type The type of u-chart to be plotted. The options are "norm"
#' (traditional Shewhart u-chart), "CF" (improved u-chart) and "std"
#' (standardized u-chart). If not specified, a Shewhart u-chart will be
#' plotted.
#' @param u1 The sample ratios used to estimate the Poisson parameter (lambda).
#' (x1 / n1).
#' @param x2 The phase II data that will be plotted in a phase II chart.
#' @param n2 A value or a vector of values specifying the sample sizes
#' associated with each group for the phase II data.
#' @param lambda The estimate of lambda.
#' @param u2 The sample ratios of the phase II data (x2 / n2).
#' @return Returns a u-chart.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' data(moonroof)
#' attach(moonroof)
#' cchart.u(x1 = yi[1:17], n1 = ni[1:17])
#' cchart.u(x1 = yi[1:17], n1 = ni[1:17], type = "CF", x2 = yi[18:34], n2 = ni[18:34])
#' cchart.u(type = "std", u2 = ui[18:34], n2 = ni[18:34], lambda = 1.4)
#' 
cchart.u <- function(x1 = NULL, n1 = NULL, type = "norm", u1 = NULL, x2 = NULL, n2 = NULL, lambda = NULL, u2 = NULL)
{
    if((!is.null(n1)) && (!is.null(x1) || !is.null(u1)))
        OK1 <- TRUE
    else
        OK1 <- FALSE
    if(!is.null(n2) && (!is.null(x2) || !is.null(u2)) && (OK1 || !is.null(lambda)))
        OK2 <- TRUE
    else
        OK2 <- FALSE
    
#-- Error messages
    if(!OK1 && !OK2)
    {
        if(is.null(x1) && is.null(n1) && is.null(u1))
            stop("Phase I data and samples sizes are missing")
        else
        {
            if(is.null(n1))
                stop("Phase I samples sizes not specified")
            else
                stop("Phase I data is missing")
        }
    }
    if(!OK2)
    {
        if(is.null(n2) && (!is.null(x2) || !is.null(u2)))
            stop("Phase II samples sizes not specified")
        if(!is.null(n2) && (is.null(x2) && is.null(u2)))
            stop("Phase II data is missing")
        if(!is.null(x2) && !is.null(n2) && !is.null(u2))
            stop("Information about phase I is missing")
    }

#-- Phase I
    if(OK1 && !OK2)
    {
        if(!is.null(x1))
        {
            m1 <- length(x1)
            if(length(n1) != length(x1))
                stop("The arguments x1 and n1 must have the same length")
        }
        if(!is.null(u1))
        {
            m1 <- length(u1)
            if(length(n1) != length(u1))
                stop("The arguments u1 and n1 must have the same length")
        }
        if(is.null(u1))
            u1 <- x1 / n1
        if(is.null(x1))
            x1 <- u1 * n1
        lambda <- mean(u1)
        l <- numeric(m1)
#------ Shewhart
        if(type == "norm")
        {
            u <- numeric(m1)
            for(i in 1:m1)
            {
                UCL <- lambda + (SIGMA_MULT * sqrt(lambda / n1[i]))
                u[i] <- UCL
                LCL <- lambda - (SIGMA_MULT * sqrt(lambda / n1[i]))
                l[i] <- LCL
            }
            qcc(x1, type = "u", n1, limits = c(l, u), center = lambda, title = "Shewhart u-chart (phase I)")
        }
#------ Cornish-Fisher
        if(type == "CF")
        {
            u <- numeric(m1)
            for(i in 1:m1)
            {
                UCL <- lambda + (SIGMA_MULT * sqrt(lambda / n1[i])) + (4 / (3 * n1[i])) - (1 / ((3 * n1[i]) * sqrt(lambda * n1[i])))
                u[i] <- UCL
                LCL <- lambda - (SIGMA_MULT * sqrt(lambda / n1[i])) + (4 / (3 * n1[i])) - (1 / ((3 * n1[i]) * sqrt(lambda * n1[i])))
                l[i] <- LCL
            }
            qcc(x1, type = "u", n1, limits = c(l, u), center = lambda, title = "Cornish-Fisher u-exact (phase I)")
        }
#------ Standardized
        if(type == "std")
        {
            for(i in 1:m1)
            {
                z <- (u1[i] - lambda) / sqrt(lambda / n1[i])
                l[i] <- z
            }
            qcc(l, type = "u", n1, center = 0, limits = c(-SIGMA_MULT, SIGMA_MULT), title = "Standardized u-chart (phase I)")
        }
    }
#-- Phase II
    if(OK2)
    {
        if(!is.null(x2))
        {
            m2 <- length(x2)
            if(length(n2) != length(x2))
                stop("The arguments x2 and n2 must have the same length")
        }
        if(!is.null(u2))
        {
            m2 <- length(u2)
            if(length(n2) != length(u2))
                stop("The arguments u2 and n2 must have the same length")
        }
        if(is.null(u2))
            u2 <- x2 / n2
        if(is.null(x2))
            x2 <- u2 * n2
        if(is.null(lambda))
        {
            if(is.null(u1))
                u1 <- x1 / n1
            lambda <- mean(u1)
        }
        l <- numeric(m2)
#------ Shewhart
        if(type == "norm")
        {
            u <- numeric(m2)
            for(i in 1:m2)
            {
                UCL <- lambda + (SIGMA_MULT * sqrt(lambda / n2[i]))
                u[i] <- UCL
                LCL <- lambda - (SIGMA_MULT * sqrt(lambda / n2[i]))
                l[i] <- LCL
            }
            qcc(x2, type = "u", n2, limits = c(l, u), center = lambda, title = "Shewhart u-chart (phase II)")
        }
#------ Cornish-Fisher
        if(type == "CF")
        {
            u <- numeric(m2)
            for(i in 1:m2)
            {
                UCL <- lambda + (SIGMA_MULT * sqrt(lambda / n2[i])) + (4 / (3 * n2[i])) - (1 / ((3 * n2[i]) * sqrt(lambda * n2[i])))
                u[i] <- UCL
                LCL <- lambda - (SIGMA_MULT * sqrt(lambda / n2[i])) + (4 / (3 * n2[i])) - (1 / ((3 * n2[i]) * sqrt(lambda * n2[i])))
                l[i] <- LCL
            }
            qcc(x2, type = "u", n2, limits = c(l, u), center = lambda, title = "Cornish-Fisher u-exact (phase II)")
        }
#------ Standardized
        if(type == "std")
        {
            for(i in 1:m2)
            {
                z <- (u2[i] - lambda) / sqrt(lambda / n2[i])
                l[i] <- z
            }
            qcc(l, type = "u", n2, center = 0, limits = c(-SIGMA_MULT, SIGMA_MULT), title = "Standardized u-chart (phase II)")
        }
    }
}
