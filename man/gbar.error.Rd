% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.R
\name{gbar.error}
\alias{gbar.error}
\title{barplot with errorbars

A barplot of fractions with confidence intervals. To be used with
binom.conf.}
\usage{
gbar.error(
  frac,
  conf.low,
  conf.high,
  group,
  wes = "Royal1",
  other.palette = NULL,
  print = TRUE,
  fill = NULL,
  stat = "identity",
  position = position_dodge(width = 0.9)
)
}
\arguments{
\item{frac}{any numeric vector, does not have to be a fraction}
}
\value{
A ggplot object
}
\description{
barplot with errorbars

A barplot of fractions with confidence intervals. To be used with
binom.conf.
}