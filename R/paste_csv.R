#' Paste values together, separated by commas
#'
#' @param \dots character
#' @importFrom stringr str_c
#' @export
paste_csv <- function (...) {
  stringr::str_c(..., collapse = ", ")
}
