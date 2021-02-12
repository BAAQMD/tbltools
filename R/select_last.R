#' Move selected column(s) to last position
#'
#' @param input_data tabular data
#' @param ... column name(s)
#'
#' @seealso [select_first()]
#'
#' @return
#' @export
#'
select_last <- function (input_data, ...) {

  selected_vars <-
    names(select(input_data, ...))

  select(
    input_data,
    -all_of(selected_vars),
    everything(),
    all_of(selected_vars))

}
