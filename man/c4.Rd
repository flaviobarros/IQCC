\name{c4}
\alias{c4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ C4 Constant. }
\description{
  This function is used to calculate the bias correction constant c4 for the sample standard deviation statistic.
}
\usage{
c4(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ The sample size. }
}
\details{
  It is used to correct the bias for small sample sizes in the sample standard deviation statistic.
}
\value{
  Return the value of c4 for a given sample size n.
}
\author{Daniela R. Recchia, Emanuel P. Barbosa }

\seealso{ \link{d2},\link{d3} }
\examples{
c4(5)
}
