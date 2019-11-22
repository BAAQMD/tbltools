#' Filter for specific sources at specific facilities
#'
#' @param input_data data frame containing column `fac_id`
#' @param whitelist named list, with facility names as names, and data frames as elements (see note and examples)
#'
#' @return subset of `input_data`, with additional column `fac_name`
#' @return subset of `input_data`, with additional columns `fac_name` and `src_name`
#'
#' @export
#'
#'
filter_sources <- function (input_data, whitelist) {

  stopifnot(is.list(whitelist))
  stopifnot(!is.null(names(whitelist)))

  fac_list <- map(whitelist, pull, "fac_id")
  prefiltered <- filter_facilities(input_data, fac_list)

  result <- inner_join(prefiltered, bind_rows(whitelist), by = c("fac_name", "fac_id", "src_id"))
  return(result)

}
