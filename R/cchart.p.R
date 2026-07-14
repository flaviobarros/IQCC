.pchart_type <- function(type)
{
    if(!is.character(type) || length(type) != 1 || is.na(type))
        stop("type must be a single character value")

    key <- tolower(type)
    aliases <- c(
        norm = "normal",
        normal = "normal",
        cf = "cf1",
        cf1 = "cf1",
        cf2 = "cf2",
        std = "standardized",
        standardized = "standardized"
    )

    if(!key %in% names(aliases))
        stop("type must be one of 'normal', 'cf1', 'cf2', or 'standardized'")

    unname(aliases[key])
}

.pchart_prepare_data <- function(x, p, n, x_name, p_name, n_name)
{
    if(!is.null(x) && !is.null(p))
        stop(paste0("supply exactly one of ", x_name, " or ", p_name))

    m <- if(!is.null(x)) length(x) else length(p)
    if(m < 1)
        stop(paste0("the ", if(!is.null(x)) x_name else p_name,
                    " data must not be empty"))

    if(!is.numeric(n) || length(n) < 1 || any(!is.finite(n)) ||
       any(n < 1) || any(n != floor(n)))
        stop(paste0(n_name, " must contain positive integers"))

    if(length(n) == 1)
        n <- rep(n, m)

    data_name <- if(!is.null(x)) x_name else p_name
    if(length(n) != m)
        stop(paste0("The arguments ", data_name, " and ", n_name,
                    " must have the same length"))

    if(!is.null(x))
    {
        if(!is.numeric(x) || any(!is.finite(x)) || any(x < 0) ||
           any(x != floor(x)))
            stop(paste0(x_name, " must contain finite non-negative integers"))
        if(any(x > n))
            stop(paste0(x_name, " values must not exceed ", n_name))
        p_obs <- x / n
    }
    else
    {
        if(!is.numeric(p) || any(!is.finite(p)) || any(p < 0 | p > 1))
            stop(paste0(p_name, " must contain finite values between 0 and 1"))
        p_obs <- p
        x <- p_obs * n
    }

    list(x = x, p = p_obs, n = n)
}

.pchart_draw <- function(prepared, phat, type, alpha, phase)
{
    if(type == "standardized")
    {
        z <- (prepared$p - phat) /
            sqrt(phat * (1 - phat) / prepared$n)
        z_limit <- stats::qnorm(1 - alpha / 2)

        return(qcc(
            z,
            type = "xbar.one",
            center = 0,
            limits = c(-z_limit, z_limit),
            title = paste0("Standardized p-chart (phase ", phase, ")")
        ))
    }

    limits <- pchart_limits(phat, prepared$n, alpha = alpha, type = type)
    title_method <- switch(
        type,
        normal = "Shewhart",
        cf1 = "Cornish-Fisher CF1",
        cf2 = "Cornish-Fisher CF2"
    )

    qcc(
        prepared$x,
        type = "p",
        sizes = prepared$n,
        limits = cbind(limits$lcl, limits$ucl),
        center = phat,
        title = paste0(title_method, " p-chart (phase ", phase, ")")
    )
}

#' p-chart
#'
#' Build normal, Cornish-Fisher corrected, or standardized p charts.
#'
#' For a Phase I chart, \code{n1} and exactly one of \code{x1} or \code{p1}
#' must be supplied. For a Phase II chart, \code{n2} and exactly one of
#' \code{x2} or \code{p2} must be supplied, together with Phase I information
#' or a known \code{phat}.
#'
#' When sample sizes vary, the process proportion is estimated by the pooled
#' binomial estimator, \eqn{sum(x_i) / sum(n_i)}, rather than by the unweighted
#' mean of subgroup proportions. The plotting wrapper uses two-sided limits;
#' use \code{pchart_limits()} directly for one-sided upper limits.
#'
#' @param x1 Phase I nonconforming counts.
#' @param n1 Phase I sample size or vector of sample sizes.
#' @param type Chart type. Accepted values are \code{"normal"}, \code{"cf1"},
#'   \code{"cf2"}, and \code{"standardized"}. The legacy aliases
#'   \code{"norm"}, \code{"CF"}, and \code{"std"} remain supported;
#'   \code{"CF"} maps to \code{"cf1"}.
#' @param p1 Phase I subgroup proportions. Used instead of \code{x1}.
#' @param x2 Phase II nonconforming counts.
#' @param n2 Phase II sample size or vector of sample sizes.
#' @param phat Known or previously estimated in-control proportion.
#' @param p2 Phase II subgroup proportions. Used instead of \code{x2}.
#' @param alpha Nominal two-sided false alarm probability. Defaults to 0.0027.
#'
#' @return Invisibly returns the \code{qcc} object used to draw the chart.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @references
#' Montgomery, D. C. (2008). \emph{Introduction to Statistical Quality
#' Control}. Wiley.
#'
#' Joekes, S. and Barbosa, E. P. (2013). An improved attribute control chart
#' for monitoring non-conforming proportion in high quality processes.
#' \emph{Control Engineering Practice}, 21, 407--412.
#' \doi{10.1016/j.conengprac.2012.12.005}.
#' @importFrom qcc qcc
#' @examples
#'
#' data(binomdata)
#' cchart.p(x1 = binomdata$Di[1:12], n1 = binomdata$ni[1:12])
#' cchart.p(x1 = binomdata$Di[1:12], n1 = binomdata$ni[1:12],
#'          type = "cf2", x2 = binomdata$Di[13:25],
#'          n2 = binomdata$ni[13:25])
#' cchart.p(type = "standardized", p2 = binomdata$Di[13:25] /
#'          binomdata$ni[13:25], n2 = binomdata$ni[13:25],
#'          phat = 0.1115833)
#'
cchart.p <- function(x1 = NULL, n1 = NULL, type = "norm", p1 = NULL,
                     x2 = NULL, n2 = NULL, phat = NULL, p2 = NULL,
                     alpha = ALPHA)
{
    type <- .pchart_type(type)

    if(!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) ||
       alpha <= 0 || alpha >= 1)
        stop("alpha must be a finite scalar between 0 and 1")

    if(!is.null(x1) && !is.null(p1))
        stop("supply exactly one of x1 or p1")
    if(!is.null(x2) && !is.null(p2))
        stop("supply exactly one of x2 or p2")

    ok1 <- !is.null(n1) && (!is.null(x1) || !is.null(p1))
    ok2 <- !is.null(n2) && (!is.null(x2) || !is.null(p2)) &&
        (ok1 || !is.null(phat))

    if(!ok1 && !ok2)
    {
        if(is.null(x1) && is.null(n1) && is.null(p1))
            stop("Phase I data and samples sizes are missing")
        if(is.null(n1))
            stop("Phase I samples sizes not specified")
        stop("Phase I data is missing")
    }

    if(!ok2)
    {
        if(is.null(n2) && (!is.null(x2) || !is.null(p2)))
            stop("Phase II samples sizes not specified")
        if(!is.null(n2) && is.null(x2) && is.null(p2))
            stop("Phase II data is missing")
        if((!is.null(x2) || !is.null(p2)) && !ok1 && is.null(phat))
            stop("Information about phase I is missing")
    }

    phase1 <- NULL
    if(ok1)
    {
        phase1 <- .pchart_prepare_data(x1, p1, n1, "x1", "p1", "n1")
        phat_phase1 <- sum(phase1$x) / sum(phase1$n)
    }

    if(is.null(phat) && ok1)
        phat <- phat_phase1

    if(!is.numeric(phat) || length(phat) != 1 || !is.finite(phat) ||
       phat <= 0 || phat >= 1)
        stop("phat must be a finite scalar strictly between 0 and 1")

    if(!ok2)
        return(invisible(.pchart_draw(phase1, phat, type, alpha, "I")))

    phase2 <- .pchart_prepare_data(x2, p2, n2, "x2", "p2", "n2")
    invisible(.pchart_draw(phase2, phat, type, alpha, "II"))
}
