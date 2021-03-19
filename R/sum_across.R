#' sum_across
#'
#' @param input_data tabular data
#' @param ... column name(s)
#' @param na.rm logical
#' @param .groups passed to [dplyr::summarise()]
#'
#' @importFrom dplyr summarize across
#'
#' @return
#' @export
#'
sum_across <- function (
  input_data,
  ...,
  na.rm = TRUE,
  .groups = "drop"
) {

  dplyr::summarise(
    input_data,
    dplyr::across(
      c(...),
      ~ sum(., na.rm = na.rm)),
    .groups = .groups)

}
