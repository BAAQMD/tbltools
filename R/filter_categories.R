#' filter_categories
#'
#' Filter by category, and (optionally) create custom labels at the same time.
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
#' # `filter_categories()` both filters the data, and assigns names to `cat_abbr`.
#' residential_NG_data <- BY2011::BY2011_annual_emission_data %>% filter_categories(RESIDENTIAL_NG_COMBUSTION_CATEGORIES)
#' residential_NG_data %>% tabulate_emissions_by(cat_abbr, cat_id, year)
#'
#' @export
filter_categories <- function (
  input_data,
  ...,
  .name = "category",
  verbose = getOption("verbose")
) {

  categories <-
    packtools::unpack_args(...)

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

    mutated <- mutate(
      filtered,
      !!.name := factor(
        decode(cat_id, unpacked_codec),
        levels = names(unpacked_codec))) # force subsequent ordering

    return(mutated)

  }

  if (is.numeric(categories)) {

    # if `categories` is numeric, then filter on `cat_id`
    filtered <-
      filter(
        input_data,
        cat_id %in% categories)

    return(filtered)

  } else {

    err_msg <-
      str_c(
      "[filter_pollutants] ",
      "don't know how to handle a categories of class ",
      class(categories))

    stop(err_msg)

  }


}
