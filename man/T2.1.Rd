\name{T2.1}
\alias{T2.1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hotelling T2 Statistic for Phase I. }
\description{
  Calculate the Hotelling T2 statistic for multivariate observations at phase I , to be used to build the corresponding control chart.
}
\usage{
T2.1(estat, m, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{estat}{ The values of the auxiliary statistics. Should be a list with a matrix with the means, mean of the means and mean of the standard deviation. }
  \item{m}{ The number of samples generated previously in data.1. }
  \item{n}{ The size of each samples used previously in data.1. }
}
\details{
  Before using this function it is necessary to execute the function "stats"(that calculate the auxiliary statistics involved in the T2 formula) and the function "data.1" (or other way to supply the data).
}
\value{
  Return a vector with the Hotelling T2 statistics.
}
\references{ Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control". Chapter 11. Wiley. }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{stats}, \link{data.1}, \link{cchart.T2.1} }
\examples{
mu <- c(5.682, 88.22)
Sigma <- symMatrix(c(3.770, -5.495, 13.53), 2)
#Example with individual observations
datum <- data.1(50, 1, mu, Sigma)
estat <- stats(datum, 50, 1, 2) 
T2.1(estat, 50, 1)
#Example with sub group observations
datum <- data.1(20, 10, mu, Sigma)
estat <- stats(datum, 20, 10, 2) 
T2.1(estat, 20, 10)
}
