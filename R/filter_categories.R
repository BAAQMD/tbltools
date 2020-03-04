#' filter_categories
#'
#' Filter by category, and (optionally) create custom labels at the same time.
#'
#' @param input_data (tabular data)
#' @param ... either an (optionally named) vector or list of category IDs, *or* a tabular dataset having a column `cat_id`.
#' @param .name if the dot-args (above) are named, then the names will be used to populate a column in the result. `.name` will be that column's name.
#'
#' @examples
#' # Here are some emissions we'd like to filter AND label.
#' DB_data <- RY(2011:2012) %>% point_source_emissions()
#'
#' # Here's what we want to analyze. (Hint: look at `DB_POLLUTANT_NAMES` for name-id pairs.)
#' RESIDENTIAL_NG_COMBUSTION_CATEGORIES <- c(
#'   "Space Heating" = 283,
#'   "Water Heating" = 284,
#'   "Cooking" = 285)
#'
#' # `filter_categories()` both filters the data, and assigns names to `category` (by default; use the `.name` argument to change it).
#' residential_NG_data <- BY2011::BY2011_annual_emission_data %>% filter_categories(RESIDENTIAL_NG_COMBUSTION_CATEGORIES)
#' residential_NG_data %>% tabulate_emissions_by(category, cat_id, year)
#'
#' @export
filter_categories <- function (
  input_data,
  ...,
  .name = "category",
  verbose = getOption("verbose")
) {

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

  if (inherits(categories, c("numeric", "character"))) {

    # if `categories` is numeric or character, then filter on `cat_id`
    filtered <-
      filter(
        input_data,
        cat_id %in% categories)

    return(filtered)

  } else {

    err_msg <-
      str_c(
        "[filter_categories] ",
        "don't know how to handle a categories of class ",
        class(categories))

    stop(err_msg)

  }


}
