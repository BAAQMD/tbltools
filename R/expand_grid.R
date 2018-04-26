#' expand_grid
#'
#' Like `expand.grid` but treats `stringsAsFactors` as FALSE.
#'
#' @examples
#' expand_grid(`Foo Bar` = 1:3, baz = "bap")
#'
#' @export
expand_grid <- function (..., stringsAsFactors = FALSE) {

  expanded <- base::expand.grid(
    stringsAsFactors = stringsAsFactors,
    ...)

  return(tibble::as.tibble(expanded))

}
