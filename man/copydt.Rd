% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.R
\name{copydt}
\alias{copydt}
\title{copy data frame/table columns to a new data table with forced column structure}
\usage{
copydt(dt, columns, as.data.table = TRUE)
}
\description{
Ensure that all columns in out data table possess the specified columns
in which default values for missing columns will be NA valuess
}
