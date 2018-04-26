#' Write to ZIP file
#'
#' @param x  name(s) of input file(s)
#' @param file  name of ZIP file (see note)
#' @param flags  flags passed to \link{zip} ("q" means "quiet")
#' @param clean  whether to remove input file(s) afterwards
#' @param ...  further arguments to \link{zip}
#'
#' @return           \code{file}, invisibly (for chaining)
#'
#' @seealso \link{write_tbl}
#'
#' @export
zip_file <- function (x, file, flags = "-q", clean = TRUE, ...) {

  msg <- "NOTE: write_csv has new behavior (returns .data instead of file)"
  .Defunct(msg = msg)
  stopifnot(is.character(x))
  stop(msg)

  # if (missing(file) & length(x) == 1) {
  #   file <- stringr::str_replace(x, regexp("\\.csv$", ignore_case = TRUE), ".zip")
  # }
  #
  # zip(zipfile = file, files = x, extras = flags, ...)
  #
  # if (file.exists(file)) {
  #   if (clean) file.remove(x)
  # } else {
  #   stop("Couldn't create", file)
  # }
  #
  # return(invisible(file))

}
