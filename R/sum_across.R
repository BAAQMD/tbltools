#' sum_across
#'
#' @param input_data tabular data
#' @param ... column name(s)
#' @param na.rm logical
#' @param .groups passed to [dplyr::summarise()]
#'
#' @importFrom dplyr summarise across
#' @importFrom units drop_units
#' @importFrom unittools restore_units
#'
#' @export
#'
sum_across <- function (
  input_data,
  ...,
  na.rm = TRUE,
  .groups = "drop"
) {

  unitless_data <-
    units::drop_units(input_data)

  summarised_data <-
    dplyr::summarise(
      unitless_data,
      dplyr::across(
        c(...),
        ~ sum(., na.rm = na.rm)),
      .groups = .groups)

  unit_aware_data <-
    unittools::restore_units(
      to = summarised_data,
      from = input_data)

  return(unit_aware_data)

}
