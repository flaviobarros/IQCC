\name{cchart.Xbar2}
\alias{cchart.Xbar2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ X-bar Shewhart Control Chart for phase II. }
\description{
  Builds the x-bar control chart for phase II.
}
\usage{
cchart.Xbar2(x, x2bar, sigma, sizes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{x2bar}{ The mean of means. }
  \item{sigma}{ The standar deviation of the data. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
}
\details{
  To use this function it is necessary to have the output given by the function XbarI. 
}
\value{
  Return a x-bar control chart for phase II.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{cchart.Xbar1} }
\examples{
data(pistonrings)
stat <- cchart.Xbar1(pistonrings[1:25, ])
cchart.Xbar2(pistonrings[26:40, ], stat[[1]][1], stat[[1]][2])
}
