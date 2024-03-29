% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_tbl.R
\name{read_tbl}
\alias{read_tbl}
\title{Read tabular data}
\usage{
read_tbl(file, ..., verbose = getOption("verbose"))
}
\arguments{
\item{file}{name of a .csv, .xls, or .xlsx file}

\item{...}{further arguments to \link{read_csv} or \link{read_xls}}

\item{verbose}{whether to print status messages}
}
\value{
a \link[dplyr]{tbl_df}
}
\description{
Efficiently read
tabular data from CSV files or Excel worksheets.
}
\note{
In contrast to \link{read.csv}, \code{stringsAsFactors} and
\code{check.names} are effectively \code{FALSE}.
}
