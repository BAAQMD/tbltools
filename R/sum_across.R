#' sum_across
#'
#' @param x
#' @param ...
#' @param na.rm
#' @param .groups
#'
#' @return
#' @export
#'
#' @examples
sum_across <- function (x, ..., na.rm = TRUE, .groups = "drop") {
  dplyr::summarize(
    x,
    across(
      c(...),
      ~ sum(., na.rm = na.rm)),
    .groups = .groups)
}
