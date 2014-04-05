\name{table.qtukey}
\alias{table.qtukey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Tukey Quantile Table }
\description{
  Builds a table with quantiles of the sample relative range distribution.
}
\usage{
table.qtukey(alpha, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha}{ The probability of type-I error of false alarm , that is equal to 1 minus the confidence level. }
  \item{n}{ The maximum sample size. }
}
\value{
  It is used the fact that the sample relative range distribution is the same as the sample studentized range distribution (tukey distribution) with infinity d.f. in the denominator . It is considered 4 quantiles: alpha/2 , alpha , 1-alpha and 1-alpha/2, for different sample size values .
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{table.const},\link{alpha.risk},\link{qtukey} }
\examples{
table.qtukey(0.0027, 15)
}
