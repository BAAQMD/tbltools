#' filter_categories
#'
#' Filter by category, and (optionally) create custom labels at the same time.
#'
#' @param input_data (tabular data)
#' @param ... either an (optionally named) vector or list of category IDs, *or* a tabular dataset having a column `cat_id`.
#' @param .name if the dot-args (above) are named, then the names will be used to populate a column in the result. `.name` will be that column's name.
#'
#' @seealso [filter_facilities()]
#' @seealso [filter_pollutants()]
#'
#' @export
filter_categories <- function (
  input_data,
  ...,
  .name = "category",
  verbose = getOption("verbose")
) {

  if ("cat_id" %not_in% names(input_data)) {
    stop("[filter_categories] input data must have a column named cat_id")
  }

  if (inherits(first(list(...)), "data.frame")) {
    categories <-
      pull_distinct(first(list(...)), cat_id)
  } else {
    categories <-
      packtools::unpack_args(
        ...)
  }

  stopifnot(
    is.list(categories)
    | is.numeric(categories)
    | is.character(categories))

  if (!is.null(names(categories))) {

    unpacked_ids <-
      unlist(categories)

    unpacked_codec <-
      packtools::unpack_list(categories)

    filtered <-
      filter(
        input_data,
        cat_id %in% unpacked_ids)

    mutated <-
      mutate(
        filtered,
        !!.name := factor(
          decode(cat_id, unpacked_codec),
          levels = unique(names(unpacked_codec)))) # force subsequent ordering

    return(mutated)

  }

  if (inherits(categories, c("integer", "numeric", "character"))) {

    # if `categories` is numeric or character, then filter on `cat_id`
    filtered <-
      filter(
        input_data,
        cat_id %in% categories)

    mutated <-
      mutate(
        filtered,
        !!.name := factor(
          str_c("#", cat_id),
          levels = str_c("#", unique(categories)))) # force subsequent ordering

    return(mutated)

  } else {

    err_msg <-
      str_c(
        "[filter_categories] ",
        "don't know how to handle a categories of class ",
        class(categories))

    stop(err_msg)

  }


}
