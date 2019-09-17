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

  tmpdir <- tempdir()
  tmpfn <- file.path(tmpdir, file)
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
    selfcontained = selfcontained,
    ...)

  if (!dir.exists(dirname(file))) {
    msg("creating dir: ", dirname(file))
    dir.create(dirname(file), recursive = TRUE)
  }

  success <-
    file.copy(
      dirname(tmpfn),
      file.path(dirname(file), ".."),
      recursive = TRUE)

  return(invisible(object))

}
