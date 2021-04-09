#' read_fst
#'
#' Thin wrapper around fst::read_fst()
#'
#' Returns a tibble instead of a data.frame.
#'
#' @export
read_fst <- function (
  ...,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[read_fst] ", ...)
  fst_data <- fst::read_fst(...)
  return(tibble::as_tibble(fst_data))

}
