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

  tmpfn <- tempfile()

  htmlwidgets::saveWidget(
    object,
    tmpfn,
    selfcontained = selfcontained,
    ...)

  success <- file.copy(
    from = tmpfn,
    to = file,
    overwrite = overwrite,
    recursive = TRUE)

  stopifnot(success)
  msg("wrote to ", file)

  file.remove(tmpfn)

  return(invisible(object))

}
