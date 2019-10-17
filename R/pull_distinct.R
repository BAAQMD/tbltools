#' pull_distinct
#'
#' `pull_distinct(x, var)` is equivalent to `pull(distinct(x, var), var).``
#'
#' @param x tabular data
#' @param ... arguments passed to `distinct()` and `pull()`
#'
#' @export
pull_distinct <- function (x, ...) {
  pull(distinct(x, ...), ...)
}
