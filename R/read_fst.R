#' read_fst
#'
#' Thin wrapper around fst::read_fst()
#'
#' Returns a tibble instead of a data.frame.
#'
#' @export
read_fst <- function (...) {

  fst_data <- fst::read_fst(...)
  return(tibble::as_tibble(fst_data))

}
