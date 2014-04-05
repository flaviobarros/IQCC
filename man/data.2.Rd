\name{data.2}
\alias{data.2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hotelling Control Chart Phase II simulated data. }
\description{
  This function simulate a normal data set to be used in the phase II Hotelling control charts.
}
\usage{
data.2(estat, n, delta = 0, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{estat}{ The values of the auxiliary statistics. Should be a list with a matrix with the means, mean of the means and mean of the standard deviation. }
  \item{n}{ The size of each sample. If they are individual observations, use n = 1. }
  \item{delta}{ A value to be added on the vector of means. }
  \item{p}{ The dimension. }
}
\details{
To use this function it is necessary to have the information about the phase I given by the functions data.1 and stats.
}
\value{
  Return an array with the simulated data.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{data.1} }
\examples{
mu <- c(5.682, 88.22)
Sigma <- symMatrix(c(3.770, -5.495, 13.53), 2)
datum <- data.1(20, 10, mu, Sigma)
# estat is the list with the values of the auxiliary statistics.
estat <- stats(datum, 20, 10, 2)
datum2 <- data.2(estat, 10, p = 2)
}