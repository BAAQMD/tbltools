#' update.data.frame
#'
#' Analogous to SQL UPDATE command
#'
#' @examples
#' RY2015_refining_data <- RY(2015) %>% point_source_emissions() %>% filter_facilities(DST_REFINING_FACILITIES)
#' RY2015_Chevron_FCCU_PM <- where(year == RY(2015), fac_id == 10, src_id == 4285, pol_abbr == "PM")
#' this_data <- RY2015_refining_data %>% update(RY2015_Chevron_FCCU_PM, ems_qty = ems_qty * 2.0)
#' this_data %>% tabulate_emissions_by(fac_id, pol_abbr, digits = 0)
#'
#' @export
update.data.frame <- function (input_data, .where, ..., verbose = getOption("verbose")) {

  require(lazyeval)
  msg <- function (...) if(isTRUE(verbose)) message("[update.data.frame] ", ...)

  # Logical union of all conditions
  i <- which(reduce(lazy_eval(.where, input_data), `&`))

  msg(" [", expr_text(.where), "] altering ", length(i), " values")

  # Mutate rows indexed by `i`
  mutated <- mutate(input_data[i, ], ...)

  # Leave the other rows alone
  untouched <- input_data[-i, ]

  # Put the mutated and the untouched rows back together
  bind_rows(mutated, untouched)

}
