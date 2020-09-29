#' pull_first
#'
#' @param x tibble
#' @param ... passed to [dplyr::pull()]
#'
#' @return object
#' @export
#'
pull_first <- function (x, ...) {
  dplyr::pull(x, ...)[[1]]
}
