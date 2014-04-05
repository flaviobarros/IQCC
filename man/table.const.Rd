\name{table.const}
\alias{table.const}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Table of values for the constants d2, d3 and c4. }
\description{
  This function is used to build a table of values for the constants d2, d3 and c4 for sucessive values of sample size n.
}
\usage{
table.const(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ The maximum size. }
}
\details{
  It builds a table in matrix form with 3 columns (one for each constant) and one row for each value of n from 2 to a specified value.
}
\value{
  Return the values of these three constants.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{d2},\link{d3},\link{c4} }
\examples{
table.const(17)
}

