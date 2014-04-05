\name{cchart.S}
\alias{cchart.S}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ S Control Chart. }
\description{
  This function builds a S control chart.
}
\usage{
cchart.S(x, type = "n", m = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{type}{ A character string specifying the type of S control chart to be plotted where "n" plots a S chart with normalized probability limits and "e" plots a S chart with exact limits. }
  \item{m}{ The sample sizes. Only necessary in the control chart with exact (probability) limits. }
}
\details{
  The exact limits are the alpha/2 and 1-alpha/2 quantiles of the S distribution which is proportional to the square root of a chi-square distribution.
}
\value{
  Return a S control chart.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\examples{
data(softdrink)
#S chart with normalized probability limits
cchart.S(softdrink, type = "n")
#S chart with exact probability limits
cchart.S(softdrink, type = "e", 10)
}
