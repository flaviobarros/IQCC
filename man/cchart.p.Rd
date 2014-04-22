\name{cchart.p}
\alias{cchart.p}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ p-chart }
\description{
  This function builds p-charts.
}
\usage{
cchart.p(x1 = NULL, n1 = NULL, type = "norm", p1 = NULL, x2 = NULL, 
n2 = NULL, phat = NULL, p2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{ The phase I data that will be plotted (if it is a phase I chart). }
  \item{n1}{ A value or a vector of values specifying the sample sizes associated with each group for the phase I data. }
  \item{type}{ The type of p-chart to be plotted. The options are "norm" (traditional Shewhart p-chart), "CF" (Cornish Fisher p-chart) and "std" (standardized p-chart). If not specified, a Shewhart p-chart will be plotted. }
  \item{p1}{ The data used to estimate the phat (x1 / n1). }
  \item{x2}{ The phase II data that will be plotted in a phase II chart. }
  \item{n2}{ A value or a vector of values specifying the sample sizes associated with each group for the phase II data. }
  \item{phat}{ The estimate of p. }
  \item{p2}{ The values corresponding to x2 / n2. }
}
\details{
  For a phase I p-chart, n1 must be specified and either x1 or p1.
  For a phase II p-chart, n2 must be specified, plus x2 or p2 and either phat, x1 and n1, or p1 and n1.
  The Shewhart is based on normal-aprroximation and should be used only for large values of np or n*p (n*p > 6).
}
\value{
  Return a p-chart.
}
\references{ Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control". Chapter 11. Wiley }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\examples{
data(binomdata)
attach(binomdata)
cchart.p(x1 = Di[1:12], n1 = ni[1:12])
cchart.p(x1 = Di[1:12], n1 = ni[1:12], type = "CF", x2 = Di[13:25], n2 = ni[13:25])
cchart.p(type = "std", p2 = Di[13:25], n2 = ni[13:25], phat = 0.1115833)
}
