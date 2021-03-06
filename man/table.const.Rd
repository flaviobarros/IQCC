% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table.const.R
\name{table.const}
\alias{table.const}
\title{Table of values for the constants d2, d3 and c4.}
\usage{
table.const(n)
}
\arguments{
\item{n}{The maximum size.}
}
\value{
Return the values of these three constants.
}
\description{
This function is used to build a table of values for the constants d2, d3
and c4 for sucessive values of sample size n.
}
\details{
It builds a table in matrix form with 3 columns (one for each constant) and
one row for each value of n from 2 to a specified value.
}
\examples{

table.const(17)

}
\seealso{
\link{d2},\link{d3},\link{c4}
}
\author{
Daniela R. Recchia, Emanuel P. Barbosa
}
