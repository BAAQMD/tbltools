#' sum_across
#'
#' @note May not work for list-columns. See TODO inside function.
#'
#' @param x
#' @param ...
#' @param na.rm
#' @param .groups
#'
#' @importFrom dplyr summarize across
#'
#' @return
#' @export
#'
#' @examples
sum_across <- function (x, ..., na.rm = TRUE, .groups = "drop") {

  #
  # TODO: support aggregation of list-columns via something like:
  #
  #   ~ list(reduce(., `+`))
  #
  # ... instead of
  #
  #   ~ sum(., na.rm = na.rm))
  #

  dplyr::summarize(
    x,
    dplyr::across(
      c(...),
      ~ sum(., na.rm = na.rm)),
    .groups = .groups)

}
