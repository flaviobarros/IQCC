\name{cchart.T2.1}
\alias{cchart.T2.1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Phase I Hotelling Control Chart. }
\description{
  Builds the phase I Hotelling control chart.
}
\usage{
cchart.T2.1(T2, m, n, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T2}{ The values of the T2 statistic. Shoul be a matrix. }
  \item{m}{ The number of samples generated previously in data.1. }
  \item{n}{ The size of each sample used previously in data.1. If they are individual obsersations, then use n = 1. }
  \item{p}{ The dimension used previously in function data.1. }
}
\details{
  It builds the Hotelling T2 control chart for multivariate normal data (m samples / samples of size n > 1), used retrospective / validation analysis (phase I); the control limits are based on the F distribution.
}
\value{
  Return a control chart.
}
\references{ Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control". Chapter 11. Wiley }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{cchart.T2.2} }
\examples{
mu <- c(5.682, 88.22)
Sigma <- symMatrix(c(3.770, -5.495, 13.53), 2)
datum <- data.1(20, 10, mu, Sigma)
estat <- stats(datum, 20, 10, 2)
T2 <- T2.1(estat, 20, 10)
# estat is a list with the auxiliary statistics. T2 is a matrix with the values of the T2 statistic.
cchart.T2.1(T2, 20, 10, 2)
}
