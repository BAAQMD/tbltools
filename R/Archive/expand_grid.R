#' expand_grid
#'
#' Like `expand.grid` but treats `stringsAsFactors` as FALSE.
#'
#' @export
expand_grid <- function (..., stringsAsFactors = FALSE) {

  expanded <- base::expand.grid(
    stringsAsFactors = stringsAsFactors,
    ...)

  return(tibble::as.tibble(expanded))

}
