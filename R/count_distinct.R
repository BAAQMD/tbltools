#' @export
count_distinct <- function (...) {
  nrow(distinct(select(...)))
}
