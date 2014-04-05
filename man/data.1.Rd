\name{data.1}
\alias{data.1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hotelling Control Chart Phase I simulated data. }
\description{
  This function simulate a normal data set to be used in the phase I Hoteliing control charts.
}
\usage{
data.1(m, n, mu, Sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{ The number of samples to be generated. }
  \item{n}{ The size of each sample. If they are individual observations, then use n = 1. }
  \item{mu}{ The vector with the means of the data to be generated. }
  \item{Sigma}{ The vector with the variance-covariance matrix of the data to be generated. }
}
\value{
  Return an array with the simulated data.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{data.2} }
\examples{
mu <- c(5.682, 88.22)
Sigma <- symMatrix(c(3.770, -5.495, 13.53), 2)
#Simulated data with individual observations
datum <- data.1(50, 1, mu, Sigma)
#Simulated data with sub-group observations
datum <- data.1(20, 10, mu, Sigma)
}
