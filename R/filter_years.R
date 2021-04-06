#' filter_years
#'
#' Placeholder for more sophisticated filtering (once `NPS` package is underway).
#'
#' @param input_data tabular, with column `year`
#' @param years `RY`, `PY`, or `CY`
#' @param verbose display messages
#'
#' @export
filter_years <- function (
  input_data,
  years,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[filter_years] ", ...)

  filtered_data <-
    filter(
      input_data,
      year %in% years)

  if (nrow(filtered_data) == 0) {

    warn_msg <- glue::glue(
      "those years aren't in your data. ",
      "Maybe you want to use RY(), PY(), or CY() instead?",
      .sep = "\n")

    msg(warn_msg)

  }

  return(filtered_data)

}
