#' Generalized Variance by Subgroup
#'
#' Calculate the determinant of the sample covariance matrix for each subgroup.
#'
#' @param x A list of numeric matrices, a three-dimensional array with dimensions
#' subgroup by observation by variable, or a numeric matrix containing consecutive
#' subgroups.
#' @param size Required only when \code{x} is a matrix. Number of observations in
#' each consecutive subgroup.
#' @return A numeric vector containing the generalized variance \eqn{|S|} for each
#' subgroup.
#' @export
#' @examples
#' x <- array(rnorm(4 * 6 * 2), dim = c(4, 6, 2))
#' gv_stat(x)
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
