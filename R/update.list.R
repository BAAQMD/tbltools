#' Update the value of items in a list (possibly adding new items).
#'
#' @param	object	a list
#' @param	...		arguments of the form key=value
#' @return	a copy of the list with updated items
#'
#' @export
update.list <- function (object, ...) {
  args <- list(...)
  for (key in names(args)) {
    object[[key]] <- args[[key]]
  }
  return(object)
}
