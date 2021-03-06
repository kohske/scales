\name{censor}
\alias{censor}
\title{Censor any values outside of range.}
\usage{
  censor(x, range = c(0, 1))
}
\arguments{
  \item{x}{numeric vector of values to manipulate.}

  \item{range}{numeric vector of length two giving desired
  output range.}
}
\description{
  Censor any values outside of range.
}
\examples{
censor(c(-1, 0.5, 1, 2, NA))
}

