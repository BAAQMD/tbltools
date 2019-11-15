#' pull_summary
#'
#' `pull_summary(x, var)` is equivalent to `summary(pull(x, var)).``
#'
#' @param x tabular data
#' @param ... arguments passed to `pull()`
#'
#' @export
pull_summary <- function (x, ...) {
  summary(pull(x, ...))
}
