#' Save a pivot table to disk
#'
#' @param object created with `pivot_table()`
#' @param file (character) path on disk
#' @param ... further arguments to [htmlwidgets::saveWidget()]
#' @param verbose (logical)
#'
#' @export
write_pivot_table <- function (
  object,
  file,
  overwrite = TRUE,
  selfcontained = TRUE,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[write_pivot_table] ", ...)

  if (!isTRUE(overwrite)) {
    stop("overwrite = FALSE is not supported; stopping")
  }

  htmlwidgets::saveWidget(
    object,
    file,
    selfcontained = selfcontained,
    ...)

  return(invisible(object))

}
