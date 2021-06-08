#' Move selected column(s) to first position(s)
#'
#' @param input_data tabular data
#' @param ... column name(s)
#'
#' @importFrom tidyselect vars_select all_of
#' @importFrom dplyr select
#'
#' @seealso [select_last()]
#'
#' @return
#' @export
#'
select_first <- function (input_data, ...)  {
  selected_vars <- tidyselect::vars_select(names(input_data), ...)
  other_vars <- setdiff(names(input_data), selected_vars)
  selected_data <- dplyr::select(input_data, tidyselect::all_of(selected_vars), tidyselect::all_of(other_vars))
  return(selected_data)
}
