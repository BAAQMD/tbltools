#' validate_hierarchy
#'
#' Validates an imported or existing hierarchy.
#'
#' @note Invoked automatically by [import_hierarchy()].
#'
#' @seealso
#' - [import_hierarchy()]
#' - [with_hierarchy()]
#'
#' @export
validate_hierarchy <- function (
  input_data,
  id_var = NULL,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[validate_hierarchy] ", ...)

  if (is.null(id_var)) {
    id_var <-
      vartools::find_var(
        input_data,
        suffix = "_id")
  }

  msg("id_var is: ", id_var)
  id_values <- pull(input_data, id_var)

  msg("checking for NA and/or duplicate IDs")
  duplicate_ids <- id_values[duplicated(id_values)]
  missing_ids <- which(is.na(id_values))

  if (any(duplicate_ids)) {

    err_msg <- str_c(
      "[import_hierarchy] duplicates detected in ",
      id_var, ": ",
      pack_integers(duplicate_ids))

    stop(err_msg)

  }

  if (any(missing_ids)) {

    err_msg <- str_c(
      "[import_hierarchy] missing values detected in ",
      id_var)

    stop(err_msg)

  }

  h_vars <-
    names(input_data) %>%
    tidyselect::vars_select(
      dplyr::matches("_h[0-9]+"))

  if (length(h_vars) < 1) {
    err_msg <- "[import_hierarchy] must have at least one column ending in h1, h2, etc. "
    stop(err_msg)
  }

  msg("checking ", str_csv(h_vars))

  if (any(duplicated(h_vars))) {
    i <- which(duplicated(h_vars))
    err_msg <- str_c(
      "[import_hierarchy] found duplicate h* vars: ",
      str_csv(h_vars[i]))
  }

  sorted_h_vars <-
    sort(h_vars)

  h_indices <-
    sorted_h_vars %>%
    str_extract("[0-9]+$") %>%
    parse_integer()

  if (min(h_indices) > 1) {
    err_msg <- "[import_hierarchy] should contain _h0 or _h1"
    stop(err_msg)
  }

  if (!all_true(diff(h_indices) == 1)) {
    err_msg <- "[import_hierarchy] h* vars are not sequential"
    stop(err_msg)
  }

  n_ids <- n_distinct(id_values)
  msg("n = ", n_ids, " distinct `", id_var, "`")

  validated_hierarchy <-
    input_data %>%
    dplyr::select(
      id_var,
      !!sorted_h_vars)

  return(validated_hierarchy)

}
