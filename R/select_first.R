#' Move selected column(s) to first position
#'
#' @param input_data tabular data
#' @param ... column name(s)
#'
#' @seealso [select_last()]
#'
#' @return
#' @export
#' @importFrom tidyselect everything
#'
select_first <- function (input_data, ...) {

  selected_vars <-
    names(select(input_data, ...))

  select(
    input_data,
    all_of(selected_vars),
    tidyselect::everything())

}
