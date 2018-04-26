#' Save a pivot table to disk
#'
#' @param object created with `pivot_table()`
#' @param file (character) path on disk
#' @param ... further arguments to [htmlwidgets::saveWidget()]
#' @param verbose (logical)
#'
#'
#' @export
write_pivot_table <- function (object, file, overwrite = TRUE, ..., verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[write_pivot_table] ", ...)

  tmpfn <- tempfile()

  htmlwidgets::saveWidget(object, tmpfn, ...)
  success <- file.copy(tmpfn, file, overwrite = overwrite)
  stopifnot(success)
  msg("wrote to ", file)

  file.remove(tmpfn)

  return(invisible(object))

}
