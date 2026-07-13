#' Generalized Variance by Subgroup
#'
#' Compute the generalized variance, defined as the determinant of the unbiased
#' sample covariance matrix, for each multivariate subgroup.
#'
#' For subgroup \eqn{i} with \eqn{n} observations on \eqn{p} variables,
#' \code{gv_stat()} calculates
#' \deqn{|S_i|, \qquad S_i = \frac{1}{n-1}
#'       \sum_{j=1}^{n}(x_{ij}-\bar{x}_i)(x_{ij}-\bar{x}_i)^T.}
#' All subgroups must have the same \eqn{n} and \eqn{p}, with \eqn{p >= 2}
#' and \eqn{n > p}. The latter condition permits a nonsingular sample
#' covariance matrix under nondegenerate data, although an observed subgroup
#' may still have determinant zero because of exact linear dependence.
#'
#' @param x Multivariate subgroup data in one of three forms:
#' \itemize{
#'   \item a non-empty list whose elements are \eqn{n \times p} numeric
#'     matrices;
#'   \item a numeric three-dimensional array with dimensions
#'     \code{subgroup} by \code{observation} by \code{variable}; or
#'   \item a numeric matrix formed by stacking equal-sized subgroups in
#'     consecutive blocks of rows.
#' }
#' Every observation must be finite.
#' @param size Positive integer subgroup size. It is required when \code{x} is
#' a stacked matrix and ignored for list or array input. The number of matrix
#' rows must be an exact multiple of \code{size}.
#'
#' @return A numeric vector of length equal to the number of subgroups. Element
#' \eqn{i} is \eqn{|S_i|}. A determinant that is slightly negative only because
#' of floating-point roundoff is returned as zero.
#'
#' @section Errors:
#' An error is raised for unsupported input types, empty or unequal-sized
#' subgroups, non-finite observations, fewer than two variables, \eqn{n <= p},
#' an invalid matrix \code{size}, or a covariance determinant that is genuinely
#' negative.
#'
#' @references
#' Barbosa, E. P., Gneri, M. A., and Meneguetti, A. \emph{Improving
#' Shewhart-type Generalized Variance Control Charts for Multivariate Process
#' Variability Monitoring using Cornish-Fisher Quantile Correction, Meijer-G
#' Function and Other Tools}. Research report.
#'
#' Anderson, T. W. (1984). \emph{An Introduction to Multivariate Statistical
#' Analysis}, 2nd ed. Wiley.
#'
#' @seealso \code{\link{gv_limits}}, \code{\link{gv_alpha_risk}},
#' \code{\link{cchart.GV}}
#' @export
#' @examples
#' g1 <- cbind(c(0, 1, 2, 3), c(0, 2, 1, 3))
#' g2 <- cbind(c(1, 2, 4, 7), c(3, 1, 5, 2))
#'
#' gv_stat(list(g1, g2))
#' gv_stat(rbind(g1, g2), size = 4)
#'
#' x <- array(c(g1, g2), dim = c(2, 4, 2))
#' gv_stat(x)
#'
gv_stat <- function(x, size = NULL)
{
    groups <- .gv_groups(x, size)

    vapply(
        groups,
        function(group)
        {
            if(!is.numeric(group) || any(!is.finite(group)))
                stop("all subgroup observations must be finite numeric values")
            if(nrow(group) <= ncol(group))
                stop("each subgroup must contain more observations than variables")

            value <- determinant(stats::cov(group), logarithm = FALSE)
            det_value <- as.numeric(value$modulus) * value$sign
            if(det_value < -sqrt(.Machine$double.eps))
                stop("sample covariance matrix must be positive semidefinite")
            max(0, det_value)
        },
        numeric(1)
    )
}

.gv_groups <- function(x, size = NULL)
{
    if(is.list(x))
    {
        if(length(x) < 1)
            stop("x must contain at least one subgroup")
        groups <- lapply(x, as.matrix)
    }
    else if(is.array(x) && length(dim(x)) == 3)
    {
        d <- dim(x)
        if(any(d < 1))
            stop("x must contain non-empty subgroups")
        groups <- lapply(seq_len(d[1]), function(i) x[i, , , drop = FALSE][1, , ])
        groups <- lapply(groups, function(z) matrix(z, nrow = d[2], ncol = d[3]))
    }
    else if(is.matrix(x))
    {
        if(is.null(size) || !is.numeric(size) || length(size) != 1 ||
           !is.finite(size) || size < 2 || size != floor(size))
            stop("size must be a positive integer when x is a matrix")
        if(nrow(x) %% size != 0)
            stop("the number of matrix rows must be a multiple of size")
        starts <- seq.int(1L, nrow(x), by = as.integer(size))
        groups <- lapply(starts, function(i) x[i:(i + size - 1L), , drop = FALSE])
    }
    else
    {
        stop("x must be a list, a three-dimensional array, or a numeric matrix")
    }

    p <- unique(vapply(groups, ncol, integer(1)))
    n <- unique(vapply(groups, nrow, integer(1)))
    if(length(p) != 1 || length(n) != 1)
        stop("all subgroups must have the same dimensions")
    if(p < 2)
        stop("generalized variance requires at least two variables")

    groups
}
