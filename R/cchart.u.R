.uchart_type <- function(type)
{
    if(!is.character(type) || length(type) != 1 || is.na(type))
        stop("type must be a single character value")
    aliases <- c(norm = "normal", normal = "normal", cf1 = "cf1",
                 cf = "cf2", cf2 = "cf2", std = "standardized",
                 standardized = "standardized")
    key <- tolower(type)
    if(!key %in% names(aliases))
        stop("type must be one of 'normal', 'cf1', 'cf2', or 'standardized'")
    unname(aliases[key])
}

.uchart_prepare_data <- function(x, u, n, x_name, u_name, n_name)
{
    if(!is.null(x) && !is.null(u))
        stop(paste0("supply exactly one of ", x_name, " or ", u_name))
    m <- if(!is.null(x)) length(x) else length(u)
    if(m < 1)
        stop("data must not be empty")
    if(!is.numeric(n) || length(n) < 1 || any(!is.finite(n)) || any(n <= 0))
        stop(paste0(n_name, " must contain finite positive values"))
    if(length(n) == 1)
        n <- rep(n, m)
    if(length(n) != m)
        stop(paste0("The arguments ", if(!is.null(x)) x_name else u_name,
                    " and ", n_name, " must have the same length"))
    if(!is.null(x))
    {
        if(!is.numeric(x) || any(!is.finite(x)) || any(x < 0) ||
           any(x != floor(x)))
            stop(paste0(x_name, " must contain finite non-negative integers"))
        rate <- x / n
    }
    else
    {
        if(!is.numeric(u) || any(!is.finite(u)) || any(u < 0))
            stop(paste0(u_name, " must contain finite non-negative values"))
        rate <- u
        x <- rate * n
    }
    list(x = x, u = rate, n = n)
}

.uchart_draw <- function(prepared, lambda, type, alpha, phase)
{
    if(type == "standardized")
    {
        z <- (prepared$u - lambda) / sqrt(lambda / prepared$n)
        z_limit <- stats::qnorm(1 - alpha / 2)
        return(qcc(z, type = "xbar.one", center = 0,
                   limits = c(-z_limit, z_limit),
                   title = paste0("Standardized u-chart (phase ", phase, ")")))
    }
    limits <- uchart_limits(lambda, prepared$n, alpha = alpha, type = type)
    qcc(prepared$x, type = "u", sizes = prepared$n,
        limits = cbind(limits$lcl, limits$ucl), center = lambda,
        title = paste0(toupper(type), " u-chart (phase ", phase, ")"))
}

#' u-chart
#'
#' Build normal, Cornish-Fisher corrected, or standardized u charts.
#'
#' For Phase I, supply \code{n1} and exactly one of \code{x1} or \code{u1}.
#' For Phase II, supply \code{n2} and exactly one of \code{x2} or \code{u2},
#' together with Phase I information or a known \code{lambda}.
#'
#' When inspection sizes vary, the in-control rate is estimated by the pooled
#' Poisson estimator \eqn{sum(x_i)/sum(n_i)}.
#'
#' @param x1 Phase I defect counts.
#' @param n1 Phase I inspection size or vector of sizes.
#' @param type Chart type: \code{"normal"}, \code{"cf1"}, \code{"cf2"}, or
#'   \code{"standardized"}. Legacy aliases \code{"norm"}, \code{"CF"}, and
#'   \code{"std"} remain supported; \code{"CF"} maps to \code{"cf2"} because
#'   the historical IQCC formula contains both Cornish-Fisher adjustments.
#' @param u1 Phase I rates, used instead of \code{x1}.
#' @param x2 Phase II defect counts.
#' @param n2 Phase II inspection size or vector of sizes.
#' @param lambda Known or estimated in-control defect rate.
#' @param u2 Phase II rates, used instead of \code{x2}.
#' @param alpha Nominal two-sided false-alarm probability.
#'
#' @return Invisibly returns the \code{qcc} object used to draw the chart.
#' @export
#' @importFrom qcc qcc
#' @examples
#' data(moonroof)
#' cchart.u(x1 = moonroof$yi[1:17], n1 = moonroof$ni[1:17])
#' cchart.u(x1 = moonroof$yi[1:17], n1 = moonroof$ni[1:17],
#'          type = "CF", x2 = moonroof$yi[18:34], n2 = moonroof$ni[18:34])
#' cchart.u(type = "std", u2 = moonroof$ui[18:34],
#'          n2 = moonroof$ni[18:34], lambda = 1.4)
#'
cchart.u <- function(x1 = NULL, n1 = NULL, type = "norm", u1 = NULL,
                     x2 = NULL, n2 = NULL, lambda = NULL, u2 = NULL,
                     alpha = ALPHA)
{
    type <- .uchart_type(type)
    if(!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) ||
       alpha <= 0 || alpha >= 1)
        stop("alpha must be a finite scalar between 0 and 1")
    if(!is.null(x1) && !is.null(u1))
        stop("supply exactly one of x1 or u1")
    if(!is.null(x2) && !is.null(u2))
        stop("supply exactly one of x2 or u2")

    ok1 <- !is.null(n1) && (!is.null(x1) || !is.null(u1))
    ok2 <- !is.null(n2) && (!is.null(x2) || !is.null(u2)) &&
        (ok1 || !is.null(lambda))

    if(!ok1 && !ok2)
    {
        if(is.null(x1) && is.null(n1) && is.null(u1))
            stop("Phase I data and samples sizes are missing")
        if(is.null(n1)) stop("Phase I samples sizes not specified")
        stop("Phase I data is missing")
    }
    if(!ok2)
    {
        if(is.null(n2) && (!is.null(x2) || !is.null(u2)))
            stop("Phase II samples sizes not specified")
        if(!is.null(n2) && is.null(x2) && is.null(u2))
            stop("Phase II data is missing")
        if((!is.null(x2) || !is.null(u2)) && !ok1 && is.null(lambda))
            stop("Information about phase I is missing")
    }

    phase1 <- NULL
    if(ok1)
    {
        phase1 <- .uchart_prepare_data(x1, u1, n1, "x1", "u1", "n1")
        lambda_phase1 <- sum(phase1$x) / sum(phase1$n)
    }
    if(is.null(lambda) && ok1)
        lambda <- lambda_phase1
    if(!is.numeric(lambda) || length(lambda) != 1 || !is.finite(lambda) ||
       lambda <= 0)
        stop("lambda must be a finite positive scalar")

    if(!ok2)
        return(invisible(.uchart_draw(phase1, lambda, type, alpha, "I")))

    phase2 <- .uchart_prepare_data(x2, u2, n2, "x2", "u2", "n2")
    invisible(.uchart_draw(phase2, lambda, type, alpha, "II"))
}
