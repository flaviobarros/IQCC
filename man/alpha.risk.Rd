\name{alpha.risk}
\alias{alpha.risk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ False Alarm probability for the 3-sigma R chart. }
\description{
  Used to calculate the real probability of false alarm in the 3-sigma R chart.
}
\usage{
alpha.risk(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ The sample size. }
}
\details{
  This alpha risk is calculated under the exact R statistics distribution and its values for small sample sizes will be much larger than the reference value 0,0027.
}
\value{
  Return the value of the alpha risk for a given sample size n.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{d2},\link{d3},\link{c4} }
\examples{
alpha.risk(15)
}
