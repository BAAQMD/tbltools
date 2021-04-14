#' Save a pivot table to disk
#'
#' @param object created with `pivot_table()`
#' @param path (character) path on disk
#' @param ... further arguments to [htmlwidgets::saveWidget()]
#' @param verbose (logical)
#'
#' @export
#' @importFrom htmlwidgets saveWidget
write_pivot_table <- function (
  object,
  path,
  overwrite = TRUE,
  selfcontained = TRUE,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[write_pivot_table] ", ...)

  if (!isTRUE(overwrite)) {
    stop("overwrite = FALSE is not supported; stopping")
  }

  tmpdir <- tempdir()
  tmpfn <- file.path(tmpdir, path)
  tmpdn <- dirname(tmpfn)

  if (!dir.exists(tmpdn)) {
    msg("creating dir: ", tmpdn)
    dir.create(tmpdn, recursive = TRUE)
  } else {
    # purge contents
    tmpdn_contents <- dir(tmpdn, recursive = TRUE, full.names = TRUE)
    file.remove(tmpdn_contents)
  }

  msg("writing to: ", tmpfn)
  htmlwidgets::saveWidget(
    object,
    file = tmpfn,
    selfcontained = TRUE,
    ...)

  if (!dir.exists(dirname(path))) {
    msg("creating dir: ", dirname(path))
    dir.create(dirname(path), recursive = TRUE)
  }

  if (isTRUE(selfcontained)) {
    success <-
      file.copy(
        dirname(tmpfn),
        file.path(dirname(path), ".."),
        recursive = TRUE,
        overwrite = overwrite)
  } else {
    success <-
      file.copy(
        tmpfn,
        dirname(path),
        overwrite = overwrite)
  }

  stopifnot(success)

  return(invisible(object))

}
