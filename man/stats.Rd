\name{stats}
\alias{stats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Auxiliary statistics for the multivariate control chart. }
\description{
  This function calculate the auxiliary statistics necessary to build the control chart reference lines.
}
\usage{
stats(datum, m, n, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datum}{ The data set. Should be an array. }
  \item{m}{ The number of sub groups generated previously in data.1. }
  \item{n}{ The size of each sub group used previously in data.1. }
  \item{p}{ The dimension used previously in function data.1. }
}
\details{
To use this function it is necessary to have the information about the data.1. 
}
\value{
  Return the values of the three statistics: a vector with the mean of the means, the mean of the estimated variance-covariance matrixes and a matrix with the means of each sample.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\examples{
mu <- c(5.682, 88.22)
Sigma <- symMatrix(c(3.770, -5.495, 13.53), 2)
#Example with individual observations
datum <- data.1(50, 1, mu, Sigma)
estat <- stats(datum, 50, 1, 2)
#Example with sub-group observations
datum <- data.1(20, 10, mu, Sigma)
estat <- stats(datum, 20, 10, 2)
}
