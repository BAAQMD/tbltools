#' Extract a lookup table from tabular data
#'
#' @param input_data (tabular) containing 1+ "key vars" and exactly 1 "value var"
#' @param ... (vars) `key1, key2, key3, ..., value`
#' @param na.rm (logical)
#' @param verbose (logical)
#'
#' @export
extract_lookup_table <- function (input_data, ..., na.rm = TRUE, verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[extract_lookup_table] ", ...)

  extract_vars <- tidyselect::vars_select(names(input_data), ...)
  value_var <- last(extract_vars)
  key_vars <- setdiff(extract_vars, value_var)
  msg("building mapping from (", paste_csv(key_vars), ") to ", value_var)

  extracted <- distinct(select(input_data, key_vars, value_var))

  if (isTRUE(na.rm)) {
    msg("dropping rows where ", value_var, " is NA")
    extracted <- filter_at(extracted, vars(value_var), all_vars(!is.na(.)))
  }

  msg("checking for duplicates")
  distinct_keys <- distinct(select(extracted, key_vars))

  if (nrow(distinct_keys) < nrow(extracted)) {
    tallied <- count_(extracted, vars = key_vars)
    dupes <- filter(tallied, n > 1)
    stop_msg <- str_c("duplicates detected")
    stop(stop_msg)
  }

  return(extracted)

}
