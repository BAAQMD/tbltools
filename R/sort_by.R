#' Sort by a summary (statistic) of a (grouping) variable
#'
#'  @export
sort_by_ <- function (input_data, wt_var, group_var, wt_fun = median, na.rm = TRUE) {
  grouped <- group_by_(input_data, group_var)
  ranked <- mutate(grouped, .wt = apply(get(wt_var), 1, wt_fun, na.rm = na.rm))
  arrange(ranked, desc(.wt))
}
