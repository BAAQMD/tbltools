#' with_column_totals
#'
#' Display totals for numeric columns.
#'
#' @param input_data (tabular data)
#' @param verbose (logical) display messages
#'
#' @examples
#' BY2011::BY2011_annual_emission_data %>% sample_n(3) %>% with_row_totals()
#'
with_column_sums <- function (
  input_data,
  na.rm = TRUE,
  fill = "",
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[with_row_totals] ", ...)

  numeric_vars <-
    purrr::map_lgl(
      input_data,
      is.numeric) %>%
    purrr::keep(
      identity) %>%
    names()

  id_vars <-
    names(
      input_data) %>%
    purrr::keep(
      ~ stringr::str_ends(., "_id"))

  total_vars <-
    setdiff(
      numeric_vars,
      id_vars)

  msg("total_vars is: ", str_csv(total_vars))

  totaled_data <-
    summarise_at(
      input_data,
      vars(total_vars),
      ~ sum(., na.rm = na.rm))

  annotated_data <-
    bind_rows(
      input_data,
      summary_data)

  return(annotated_data)

}
