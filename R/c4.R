#' Bias Correction Constant for Sample Standard Deviation
#'
#' Compute the constant \eqn{c_4}, which corrects the bias of the sample
#' standard deviation \eqn{S} as an estimator of the process standard deviation
#' \eqn{\sigma} under normality.
#'
#' The constant is given by
#' \deqn{c_4 = \sqrt{\frac{2}{n-1}} \,\frac{\Gamma(n/2)}{\Gamma\{(n-1)/2\}},}
#' so that \eqn{E[S] = c_4 \, \sigma}. Multiplying \eqn{S} by \eqn{1 / c_4}
#' produces an unbiased estimator of \eqn{\sigma}.
#'
#' @param n Integer sample size, at least 2. Can be a vector.
#' @return Numeric vector of \eqn{c_4} values, the same length as \code{n}.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @references
#' Montgomery, D. C. (2009). \emph{Introduction to Statistical Quality Control},
#' 6th ed. Wiley.
#' @seealso \code{\link{d2}}, \code{\link{d3}}, \code{\link{table.const}}
#' @examples
#'
#' c4(5)
#' c4(2:15)
#'
c4 <- function(n)
{
    if(any(n < 2))
        stop("n must be >= 2")
    c <- (sqrt(2 / (n-1))) * (gamma(n/2) / gamma((n-1) / 2))
    return(c)
}
