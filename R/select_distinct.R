#' Select (and arrange) distinct items
#'
#' @importFrom dplyr select distinct arrange
#'
#' @export
select_distinct <- function (input_data, ...) {
  selected <- distinct(select(input_data, ...))
  ensured <- ensure_distinct(selected, ...)
  arrange(ensured, ...)
}
