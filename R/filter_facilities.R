#' Filter and (re)name facilities
#'
#' @param input_data (tabular data)
#' @param ... either an (optionally named) vector or list of category IDs, *or* a tabular dataset having a column `cat_id`.
#' @param .name if the dot-args (above) are named, then the names will be used to populate a column in the result. `.name` will be that column's name.
#'
#' @note `input_data` **must** contain column `fac_id`
#'
#' @description **This really helps when the same facility changes IDs.** You
#'   provide a named list, where the elements of the list are vectors of *one
#'   or more* (DataBank) facility IDs that all "should be counted as" the
#'   corresponding name.
#'
#' @details Specifics may depend on your intent, but some commonly referenced
#'   "sets" are provided for you by the `inventory` package:
#'
#'   - `DST_REFINING_FACILTIES`
#'   - `DST_LANDFILL_FACILITIES`
#'   - `DST_POWER_FACILITIES`
#'
#' @seealso [filter_categories()]
#' @seealso [filter_pollutants()]
#' @seealso [find_facility_ids()]
#' @seealso [with_facility_name()]
#' @seealso [with_IRIS_site()]
#'
#' @export
#' @importFrom dplyr first
filter_facilities <- function (
  input_data,
  ...,
  .name = "facility",
  verbose = getOption("verbose")
) {

  if ("fac_id" %not_in% names(input_data)) {
    stop("[filter_facilities] input data must have a column named fac_id")
  }

  if (inherits(dplyr::first(list(...)), "data.frame")) {
    facilities <-
      pull_distinct(dplyr::first(list(...)), fac_id)
  } else {
    facilities <-
      packtools::unpack_args(
        ...)
  }

  stopifnot(
    is.list(facilities)
    | is.numeric(facilities)
    | is.character(facilities))

  if (!is.null(names(facilities))) {

    unpacked_ids <-
      unlist(facilities)

    unpacked_codec <-
      packtools::unpack_list(facilities)

    filtered <-
      filter(
        input_data,
        fac_id %in% unpacked_ids)

    mutated <-
      mutate(
        filtered,
        !!.name := factor(
          decode(fac_id, unpacked_codec),
          levels = unique(names(unpacked_codec)))) # force subsequent ordering

    return(mutated)

  }

  if (inherits(facilities, c("integer", "numeric", "character"))) {

    # if `facilities` is numeric or character, then filter on `fac_id`
    filtered <-
      filter(
        input_data,
        fac_id %in% facilities)

    mutated <-
      mutate(
        filtered,
        !!.name := factor(
          str_c("#", fac_id),
          levels = str_c("#", unique(facilities)))) # force subsequent ordering

    return(mutated)

  } else {

    err_msg <-
      str_c(
        "[filter_facilities] ",
        "don't know how to handle a facilities of class ",
        class(facilities))

    stop(err_msg)

  }


}
