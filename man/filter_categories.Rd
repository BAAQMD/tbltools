% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_categories.R
\name{filter_categories}
\alias{filter_categories}
\title{filter_categories}
\usage{
filter_categories(
  input_data,
  ...,
  .name = "category",
  verbose = getOption("verbose")
)
}
\arguments{
\item{input_data}{(tabular data)}

\item{...}{either an (optionally named) vector or list of category IDs, \emph{or} a tabular dataset having a column \code{cat_id}.}

\item{.name}{if the dot-args (above) are named, then the names will be used to populate a column in the result. \code{.name} will be that column's name.}
}
\description{
Filter by category, and (optionally) create custom labels at the same time.
}
\seealso{
\code{\link[=filter_facilities]{filter_facilities()}}

\code{\link[=filter_pollutants]{filter_pollutants()}}
}
