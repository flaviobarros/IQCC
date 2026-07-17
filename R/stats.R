#' Auxiliary Statistics for Hotelling T² Charts
#'
#' Compute the reference statistics needed by \code{\link{T2.1}} and
#' \code{\link{T2.2}} from Phase I data. Returns a list with: \code{[[1]]}
#' grand mean vector (\eqn{\bar{\bar{x}}}), \code{[[2]]} pooled covariance
#' matrix (\eqn{\bar{S}}), \code{[[3]]} matrix of subgroup means.
#'
#' To use this function it is necessary to have the information from
#' \code{\link{data.1}}.
#'
#' @param datum For \eqn{n = 1}: \eqn{m \times p} matrix. For \eqn{n > 1}:
#'   \eqn{n \times p \times m} array.
#' @param m Number of subgroups.
#' @param n Subgroup size.
#' @param p Dimension.
#' @return A list with three components: grand mean (vector), pooled covariance
#'   (matrix), subgroup means (\eqn{m \times p} matrix).
#' @section Phase convention:
#'   Designed for Phase I reference data.
#' @seealso \code{\link{T2.1}}, \code{\link{data.1}}
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)
#' #Example with individual observations
#' datum <- data.1(50, 1, mu, Sigma)
#' estat <- stats(datum, 50, 1, 2)
#' #Example with sub-group observations
#' datum <- data.1(20, 10, mu, Sigma)
#' estat <- stats(datum, 20, 10, 2)
#' 
stats <- function(datum, m, n, p)
{
    if(n == 1)
    {
        g <- array(dim = c(1, p, m - 1))
        media <- colMeans(datum)             # mean of each column of the data
        m1 <- matrix(media, m, p, byrow = TRUE) # matrix with the 20 means
        w <- datum - m1                      # array with the difference X - Xbar
        for(i in 1:(m-1))
        {
            v <- matrix(datum[i + 1, ] - datum[i, ], nrow = 1, ncol = p)
            g[, , i] <- v
        }
        p1 <- matrix(unlist(split(as.matrix(g), rep(1:p, each = 1))), nrow = m - 1, ncol = p)
        S <- (t(p1) %*% (p1)) / (2 * (m - 1))
        return(list(media, S, w))
    }
    if(n > 1)
    {
        q <- array(dim = c(p, p, m))
        w <- array(dim = c(n, p, m))
        M2 <- array(dim = c(n, p, m))
        media <- colMeans(datum)                    # mean of each face of the array, must be a matrix with 20 rows and 2 columns
        m1 <- matrix(media, m, p, byrow = TRUE)        # matrix with the 20 means
        mm <- colMeans(m1)                          # mean of means
        for(i in 1:m)
        {
            M1 <- matrix(m1[i, ], n, p, byrow = TRUE)  # repeating each mean of matrix m1 to build an array of means and subtract from the data
            M2[, , i] <- M1                         # keep the repetitions in an array
        }
        w <- datum - M2                             # array with X - Xbarra
        for(i in 1:m)
        {
            S <- (t(w[, , i]) %*% w[, , i]) / (n-1) # matrix S
            q[, , i] <- S
        }
        mS <- rowMeans(q, dims = 2)                 # mean of the matrix of the var-cov
        return(list(mm, mS, m1))
    }
}
