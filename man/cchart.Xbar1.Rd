\name{cchart.Xbar1}
\alias{cchart.Xbar1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ X-bar Shewhart Control Chart for phase I. }
\description{
  Builds the x-bar control chart for phase I.
}
\usage{
cchart.Xbar1(x, sizes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
}
\details{
  Even if the data is not normal the x-bar statistic will be close to the normal by the central limit theorem.
}
\value{
  Return a x-bar control chart for phase I.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{cchart.Xbar2} }
\examples{
data(pistonrings)
cchart.Xbar1(pistonrings[1:25, ])
}
