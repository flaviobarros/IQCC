\name{cchart.T2.2}
\alias{cchart.T2.2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Phase II Hotelling Control Chart. }
\description{
  Builds the sub group phase II Hotelling control chart.
}
\usage{
cchart.T2.2(T2II, m, n, j, t, p, datum = NULL, stats = NULL, T2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T2II}{ A vector with the value of T2 statistic for one sample. }
  \item{m}{ The number of samples generated previously in data.1. }
  \item{n}{ The size of each sample used previously in data.1. If they are individual observations, use n = 1. }
  \item{j}{ The index of the current sample. }
  \item{t}{ The maximum value of the x axis. }
  \item{p}{ The dimension used previously in function data.1. }
  For showing the phase I data set in the plot, one of the arguments below must be used (only one of them is necessary).
  \item{datum}{ The data set used in phase I. }
  \item{stats}{ The auxiliary statistics created by the function stats. }
  \item{T2}{ The Hotelling T2 statistic for multivariate observations at phase I created by the function T2.1. }
}
\details{
  It builds the Hotelling T2 control chart for multivariate normal data to be used in the operational phase (known as phase II); the control limits are based on the F distribution.
}
\value{
 Return a control chart.
}
\references{ Montgomery, D.C.,(2008). "Introduction to Statistical Quality Control". Chapter 11. Wiley }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{cchart.T2.1} }
\examples{
mu <- c(5.682, 88.22)
Sigma <- symMatrix(c(3.770, -5.495, 13.53), 2)
datum <- data.1(20, 10, mu, Sigma)
estat <- stats(datum, 20, 10, 2)
datum2 <- data.2(estat, 10, p = 2)
T2II <- T2.2(datum2, estat, 10)
# For the first sample j = 1. T2II is a vector with the value of the firts T2 statistic.
cchart.T2.2(T2II, 20, 10, 1, 25, 2)
# Same of the above, but now showing the phase I data set.
cchart.T2.2(T2II, 20, 10, 1, 25, 2, datum = datum)

#Example with individual observations
datum <- data.1(50, 1, mu, Sigma)
estat <- stats(datum, 50, 1, 2)
datum2 <- data.2(estat, 1, p = 2)
T2II <- T2.2(datum2, estat, 1)
# For the first sample j = 1. T2II is a vector with the value of the firts T2 statistic.
cchart.T2.2(T2II, 50, 1, 1, 25, 2)
# Same of the above, but now showing the phase I data set.
cchart.T2.2(T2II, 50, 1, 1, 25, 2, datum = datum)

}
