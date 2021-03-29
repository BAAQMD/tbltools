#' filter_set
#'
#' Filter tabular data using a set specification
#'
#' @param data tabular data, like a `tibble` or `data.frame`
#' @param x set specification (see Details)
#' @param ... reserved for future use
#'
#' @details
#' - `x` is presently expected to be tabular (e.g., a `data.frame` or `tibble`).
#' - `x` and `data` must have at least column in common.
#' - The result is essentially a `semi_join()` on the common columns.
#' - If `x` is empty (zero rows) or missing, the result will just be `data`.
#'   This is an attempt to behave analogously to [dplyr::filter()].
#'
#' @importFrom strtools str_csv
#' @importFrom dplyr semi_join
#' @importFrom generics intersect setequal
#'
#' @return a subset of `data` (possibly empty, or unchanged)
#'
#' @seealso The BY2015 Conventions Paper doc.
#'
#' @export
filter_set <- function (data, x, ...) {
  UseMethod("filter_set")
}

#' @noRd
filter_set.data.frame <- function (data, x, ...) {

  if (missing(x)) {
    return(data)
  }

  if (isFALSE(inherits(x, "data.frame"))) {
    err_msg <- "x must be tabular data, like a tibble or data.frame"
    stop(err_msg)
  }

  if (isFALSE(ncol(x) > 0)) {
    err_msg <- "x must have at least one column"
    stop(err_msg)
  }

  if (isFALSE(nrow(x) > 0)) {
    warn_msg <- "x probably should have at least one row; returning data unchanged"
    warning(warn_msg)
    return(data)
  }

  by_vars <- intersect(names(x), names(data))
  if (isFALSE(setequal(by_vars, names(x)))) {
    warn_msg <- strtools::str_csv("[filter_set] filtering only on: ", by_vars)
    warning(warn_msg)
  }

  filtered <- dplyr::semi_join(data, x, by = by_vars)
  return(filtered)

}
