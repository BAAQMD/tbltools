#' Join two tbls together
#'
#' @param x, y tbls to join
#' @param by a character vector of variables to join by
#' @param ... see `help(join, package = "dplyr")`
#' @param convert passed to `dplyr::all_equal()`
#' @param verbose display messages
#'
#' @description
#' This function will complain if x and y cannot be "exactly" joined.
#' This happens when there are either:
#'
#' - rows in `x` with no match in `y`; or
#' - rows in `y` with no match in `x`
#'
#' @export
#' @importFrom strtools str_csv
#' @importFrom dplyr anti_join select inner_join all_equal
exact_join <- function (
  x,
  y,
  by = NULL,
  ...,
  fail_with = stop,
  convert = FALSE,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[exact_join] ", ...)

  if (is.null(by)) {
    by <- intersect(names(x), names(y))
    msg("by is: ", str_csv(by))
  }

  left_keys <- distinct(dplyr::select(x, !!by))
  right_keys <- distinct(dplyr::select(y, !!by))

  is_exact <-
    dplyr::all_equal(
      left_keys,
      right_keys,
      convert = convert,
      ignore_row_order = TRUE,
      ignore_col_order = TRUE)

  if (!isTRUE(is_exact)) {

    left_orphans <- anti_join(left_keys, right_keys, by = by)
    right_orphans <- anti_join(right_keys, left_keys, by = by)

    if (isTRUE(nrow(left_orphans) > 0)) {
      msg("keys on left with no match on right:")
      print(left_orphans)
    }

    if (isTRUE(nrow(right_orphans) > 0)) {
      msg("keys on right with no match on left:")
      print(right_orphans)
    }

    err_msg <- "not an exact join (see above for problems)"
    fail_with(err_msg)

  }

  joined <-
    dplyr::inner_join(
      x,
      y,
      by = by,
      ...)

  return(joined)

}
