% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_facilities.R
\name{filter_facilities}
\alias{filter_facilities}
\title{Filter and (re)name facilities}
\usage{
filter_facilities(
  input_data,
  ...,
  .name = "facility",
  verbose = getOption("verbose")
)
}
\arguments{
\item{input_data}{(tabular data)}

\item{...}{either an (optionally named) vector or list of category IDs, \emph{or} a tabular dataset having a column \code{cat_id}.}

\item{.name}{if the dot-args (above) are named, then the names will be used to populate a column in the result. \code{.name} will be that column's name.}
}
\description{
\strong{This really helps when the same facility changes IDs.} You
provide a named list, where the elements of the list are vectors of \emph{one
or more} (DataBank) facility IDs that all "should be counted as" the
corresponding name.
}
\details{
Specifics may depend on your intent, but some commonly referenced
"sets" are provided for you by the \code{inventory} package:
\itemize{
\item \code{DST_REFINING_FACILTIES}
\item \code{DST_LANDFILL_FACILITIES}
\item \code{DST_POWER_FACILITIES}
}
}
\note{
\code{input_data} \strong{must} contain column \code{fac_id}
}
\seealso{
\code{\link[=filter_categories]{filter_categories()}}

\code{\link[=filter_pollutants]{filter_pollutants()}}

\code{\link[=find_facility_ids]{find_facility_ids()}}

\code{\link[=with_facility_name]{with_facility_name()}}

\code{\link[=with_IRIS_site]{with_IRIS_site()}}
}
