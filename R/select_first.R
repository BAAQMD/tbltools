#' Move selected column(s) to first position
#'
#' @param input_data
#' @param ...
#'
#' @seealso [select_last()]
#'
#' @return
#' @export
#'
#' @examples
select_first <- function (input_data, ...) {

  selected_vars <-
    names(select(input_data, ...))

  select(
    input_data,
    all_of(selected_vars),
    everything())

}
