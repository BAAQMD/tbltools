#' pull_total
#'
#' `pull_total(x, var)` is equivalent to `total(pull(x, var)).``
#'
#' @param x tabular data
#' @param ... arguments passed to `pull()`
#'
#' @export
pull_total <- function (x, ...) {
  qtytools::total(pull(x, ...))
}
