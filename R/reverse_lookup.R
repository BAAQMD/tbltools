#' Revalue a vector based on another (named) vector
#'
#' @param x vector
#' @param table named vector
#'
#' @export
reverse_lookup <- function (x, table, ...) {
  revalued <- names(table)[match(x, table)]
  factor(revalued, levels = unique(names(table)), ...)
}
