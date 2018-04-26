#' Emulate a rolling join
#'
#' @note Uses \pkg{data.table} under the hood. See \code{\link[data.table]{[.data.table}} for details.
#'
#' @param x,y tbls to join
#' @param by a character vector of columns to join by. The last column will be rolled.
#' @param roll \code{Inf} to roll forward; \code{-Inf} to roll backwards.
#' @param \dots other parameters passed on to methods
#'
#' @importFrom data.table data.table
#'
#' @export
rolling_join <- function (x, y, by, roll = Inf, allow.cartesian = TRUE, ...) {
  require(data.table)
  dt_x <- data.table(x, key = by)
  dt_y <- data.table(y, key = by)
  joined <- data.table:::`[.data.table`(dt_y, dt_x, roll = roll, allow.cartesian = allow.cartesian, ...)
  return(as.tbl(as.data.frame(joined)))
}

