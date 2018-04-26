#' Sort by a summary (statistic) of a (grouping) variable
#'
#' @examples
#' abated_point_source_emissions(year = 2011:2014) %>%
#'  filter(pol_id == 41) %>% # Benzene
#'  annual_emissions_by(fac_id, pol_id) %>%
#'  sort_by_(wt_var = "ems_qty", group_var = "fac_id")
#'
#'  @export
sort_by_ <- function (input_data, wt_var, group_var, wt_fun = median, na.rm = TRUE) {
  grouped <- group_by_(input_data, group_var)
  ranked <- mutate(grouped, .wt = apply(get(wt_var), 1, wt_fun, na.rm = na.rm))
  arrange(ranked, desc(.wt))
}
