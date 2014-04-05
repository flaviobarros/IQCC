\name{cchart.R.norm}
\alias{cchart.R}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ R control chart }
\description{
  This function builds a R control chart.
}
\usage{
cchart.R(x, n, type = "norm", y = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{n}{ The sample size. }
  \item{type}{ The type of R chart to be plotted. The options are "norm" (traditional Shewhart R chart) and "tukey" (exact R chart). If not specified, a Shewhart R chart will be plotted. }
  \item{y}{ The data used in phase I to estimate the standard deviation. }
}
\details{
  The Shewhart R chart was designed for phase I (at this moment).
  The limits of the exact R chart are the alpha/2 and 1-alpha/2 quantiles of the R distribution that are calculated as estimated process sd times the quantiles of the relative range (W=R/sigma) distribution.
}
\value{
  Return a R control chart.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\examples{
data(pistonrings)
attach(pistonrings)
cchart.R(pistonrings[1:25,], 5)
cchart.R(pistonrings[26:40, ], 5, type = "tukey", pistonrings[1:25, ])
}
