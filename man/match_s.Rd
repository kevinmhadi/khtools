% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.R
\name{match_s}
\alias{match_s}
\title{ordered match}
\usage{
match_s(x, y)
}
\arguments{
\item{x}{A vector}

\item{y}{A vector}
}
\value{
a vector of indices of \code{x} ordered by \code{y}
}
\description{
match x indices in terms of y
}
\examples{
match_s(c(1,3,5,7,9), c(9, 5, 3))
match_s(c(1,3,5,7,9), c(3, 5, 9))
}
