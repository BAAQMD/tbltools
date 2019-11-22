#' filter_pollutants
#'
#' Filter by pollutant, and (optionally) create custom labels at the same time.
#'
#' @details
#' Use with your own list (see examples below), or use with a curated list (like `DB_GHG_POLLUTANT_CODES`).
#' This can be particularly helpful when working with TACs (which may not have the `pol_abbr` that you want.)
#'
#' @examples
#' # Here are some emissions we'd like to filter AND label.
#' DB_data <- RY(2011:2015) %>% point_source_emissions()
#'
#' # Here's what we want to analyze. (Hint: look at `DB_POLLUTANT_NAMES` for name-id pairs.)
#' BTEX_POLLUTANTS <- list("Benzene" = 41, "Toluene" = 293, "Ethylbenzene" = 333, "Xylene" = 307)
#'
#' # `filter_pollutants()` both filters the data, and assigns names to `pol_abbr`.
#' BTEX_data <- DB_data %>% filter_pollutants(BTEX_POLLUTANTS)
#' BTEX_data %>% tabulate_emissions_by(pol_abbr, year)
#'
#' @export
filter_pollutants <- function (
  input_data,
  pollutants,
  ...,
  .name = "pollutant",
  verbose = TRUE
) {

  pollutants <-
    packtools::unpack_args(pollutants, ...)

  if (!is.null(names(pollutants))) {

    unpacked_ids <-
      unlist(pollutants)

    unpacked_codec <-
      unpack_list(pollutants)

    filtered <-
      filter(
        input_data,
        pol_id %in% unpacked_ids)

    mutated <- mutate(
      filtered,
      !!.name := factor(
        decode(pol_id, unpacked_codec),
        levels = names(unpacked_codec))) # force subsequent ordering

    return(mutated)

  }

  if (is.character(pollutants)) {

    filtered <-
      filter(
        input_data,
        pol_abbr %in% pollutants) # filter on `pol_abbr`, not `pol_id`

    mutated <-
      mutate(
        filtered,
        !!.name := factor(pol_abbr, levels = pollutants)) # force subsequent ordering

    return(mutated)

  }

  if (is.numeric(pollutants)) {

    filtered <- filter(input_data, pol_id %in% pollutants) # filter on `pol_id`
    return(filtered)

  } else {

    stop_msg <- str_c("[filter_pollutants] ",
                      "don't know how to handle a pollutants of class ",
                      class(pollutants))

    stop(stop_msg)

  }


}
