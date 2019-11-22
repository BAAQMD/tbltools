#' Filter and (re)name facilities
#'
#' @param input_data (tabular data) must contain `fac_id`
#' @param whitelist (named list) names = facility names; elements = facility IDs (see examples)
#' @return subset of `input_data`, with additional column `fac_name`
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
#'
#'   - `DST_LANDFILL_FACILITIES`
#'
#'   - `DST_POWER_FACILITIES`
#'
#' @seealso [filter_sources()]
#' @seealso [find_facility_ids()]
#' @seealso [with_facility_name()]
#' @seealso [with_IRIS_site()]
#'
#' @examples
#' # First retrieve some data.
#' some_CH4_data <-
#'   RY(2009:2013) %>%
#'   point_source_emissions() %>%
#'   filter(pol_abbr == "CH4")
#'
#' # You can filter for a whole "set" of related facilities ...
#' landfill_CH4_data <-
#'   some_CH4_data %>%
#'   filter_facilities(DST_LANDFILL_FACILITIES)
#'
#' # ... or just a single specific "facility" having multiple IDs.
#' P66_CH4_data <-
#'   some_CH4_data %>%
#'   filter_facilities(DST_REFINING_FACILITIES["Phillips 66"])
#'
#' # If you tabulate by `fac_id`, there's a discontinuity when P16 "becomes" P21359 ...
#' P66_CH4_data %>%
#'   tabulate_emissions_by(fac_id, pol_abbr, year)
#'
#' # ... but you can avoid this by grouping on `fac_name` instead of `fac_id`.
#' P66_CH4_data %>%
#'   tabulate_emissions_by(fac_name, pol_abbr, year)
#'
#' @export
filter_facilities <- function (input_data, whitelist) {

  stopifnot(is.list(whitelist) | is.numeric(whitelist))
  stopifnot(!is.null(names(whitelist)))

  filtered <- filter(input_data, fac_id %in% unlist(whitelist))
  mutate(filtered, fac_name = decode(fac_id, unpack_list(whitelist)))

}
