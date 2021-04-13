#' Sort by a summary (statistic) of a (grouping) variable
#'
#'  @export
#'  @importFrom dplyr group_by_ desc
#'  @importForm stats median
sort_by_ <- function (input_data, wt_var, group_var, wt_fun = median, na.rm = TRUE) {
  grouped <- dplyr::group_by_(input_data, group_var)
  ranked <- mutate(grouped, .wt = apply(get(wt_var), 1, wt_fun, na.rm = na.rm))
  arrange(ranked, dplyr::desc(.wt))
}
