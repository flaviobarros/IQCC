#' Auxiliary statistics for the multivariate control chart.
#' 
#' This function calculate the auxiliary statistics necessary to build the
#' control chart reference lines.
#' 
#' To use this function it is necessary to have the information about the
#' data.1.
#' 
#' @param datum The data set. Should be an array.
#' @param m The number of sub groups generated previously in data.1.
#' @param n The size of each sub group used previously in data.1.
#' @param p The dimension used previously in function data.1.
#' @return Return the values of the three statistics: a vector with the mean of
#' the means, the mean of the estimated variance-covariance matrixes and a
#' matrix with the means of each sample.
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @examples
#' 
#' mu <- c(5.682, 88.22)
#' Sigma <- symMatrix(c(3.770, -5.495, 13.53), 2)
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
        media <- colMeans(datum)             # média das colunas dos dados
        m1 <- matrix(media, m, p, byrow = T) # matriz com as 20 médias
        w <- datum - m1                      # array com a diferença de X - Xbarra
        for(i in 1:m-1)
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
        m1 <- matrix(media, m, p, byrow = T)        # matrix with the 20 means
        mm <- colMeans(m1)                          # mean of means
        for(i in 1:m)
        {
            M1 <- matrix(m1[i, ], n, p, byrow = T)  # repeating each mean of matrix m1 to build an array of means and subtract from the data
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
