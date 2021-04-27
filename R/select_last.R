#' Move selected column(s) to last position
#'
#' @param input_data tabular data
#' @param ... column name(s)
#'
#' @seealso [select_first()]
#'
#' @return
#' @export
#' @importFrom tidyselect everything
#'
select_last <- function (input_data, ...) {

  selected_vars <-
    names(select(input_data, ...))

  select(
    input_data,
    -all_of(selected_vars),
    tidyselect::everything(),
    all_of(selected_vars))

}
