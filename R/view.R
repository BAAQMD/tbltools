#' Carefully View
#'
#' @param x an \R object to view (maybe tabular data)
#' @param title title for viewer window
#' @param max_rows (integer) prompt if viewing data with more than this many rows
#' @param verbose (logical)
#'
#' @seealso View
#'
#' @noRd
view <- function (..., careful = TRUE, max_rows = 5000, verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[view] ", ...)

  if (isTRUE(careful)) {

    #
    # FIXME: make this more robust
    #
    x <- list(...)[[1]]

    if (isTRUE(inherits(x, "data.frame"))) {

      if (isTRUE(nrow(x) > max_rows)) {

        warn_msg <- stringr::str_c(
          "[view] your data has more than ", max_rows, " rows, and might prove unwieldy to view interactively.", "\n",
          "[view] maybe you want to first (a) trim it first; (b) use `sample_n()` first; (c) use `show()` instead?")

        message(warn_msg)

        prompt_msg <- "[view] do you want to continue anyway? [y/N] "
        response <- readline(prompt_msg)

        if (isTRUE(tolower(response) == "y")) {
          View(...)
        }

      } else {

        View(...)

      } # end (nrows)

    } else {

      View(...)

    } # end (inherits)

  } else {

    View(...)

  } # end (careful)

  # Always return x, invisibly (good for chaining maybe?)
  return(invisible(x))

}
