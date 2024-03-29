% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exact_join.R
\name{exact_join}
\alias{exact_join}
\title{Join two tbls together}
\usage{
exact_join(
  x,
  y,
  by = NULL,
  ...,
  fail_with = stop,
  convert = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{x, }{y tbls to join}

\item{by}{a character vector of variables to join by}

\item{...}{see \code{help(join, package = "dplyr")}}

\item{convert}{passed to \code{dplyr::all_equal()}}

\item{verbose}{display messages}
}
\description{
This function will complain if x and y cannot be "exactly" joined.
This happens when there are either:
\itemize{
\item rows in \code{x} with no match in \code{y}; or
\item rows in \code{y} with no match in \code{x}
}
}
