% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.R
\name{dt_na2empty}
\alias{dt_na2empty}
\title{convert columns with NA to empty character}
\usage{
dt_na2empty(dt)
}
\arguments{
\item{dt}{data.table}

\item{these_cols}{NULL by default, will select columns of character class, otherwise will be specified}
}
\value{
A data.table
}
\description{
coerce NA in columns of class "character" to ""
}
