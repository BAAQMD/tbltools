#' @export
count_distinct <- function (...) {
  nrow(distinct(dplyr::select(...)))
}
