#' validate_hierarchy
#'
#' Validates an imported or existing hierarchy.
#'
#' @note Invoked automatically by [import_hierarchy()].
#'
#' @importFrom vartools find_id_var
#' @importFrom dplyr pull matches n_distinct
#' @importFrom glue glue
#' @importFrom strtools str_or str_and
#' @importFrom tidyselect vars_select
#' @importFrom stringr str_extract
#' @importFrom readr parse_integer
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
      vartools::find_id_var(
        input_data,
        verbose = verbose)
  }

  msg("id_var is: ", id_var)
  id_values <- dplyr::pull(input_data, id_var)

  # Check for duplicates in the `id_var` column
  duplicate_ids <- id_values[duplicated(id_values)]
  if (length(duplicate_ids) > 0) {
    err_msg <- glue::glue(
      "[import_hierarchy] duplicate IDs detected in ",
      "{id_var}: {pack_integers(duplicate_ids)}")
    stop(err_msg)
  } else {
    msg("no duplicate IDs detected")
  }

  # Check for missing IDs in the `id_var` column
  missing_ids <- which(is.na(id_values))
  if (any(missing_ids)) {
    err_msg <- glue::glue(
      "[import_hierarchy] missing values detected in {id_var}")
    stop(err_msg)
  } else {
    msg("no missing IDs detected")
  }

  # Report the number of *distinct* IDs
  n_ids <- n_distinct(id_values)
  msg("n = ", n_ids, " distinct `", id_var, "`")

  # `h_vars` <- names of columns ending with "_h" and a digit
  h_vars <-
    sort(
      tidyselect::vars_select(
      names(input_data),
      dplyr::matches("_h[0-9]+$")))

  if (length(h_vars) < 1) {
    err_msg <- "[import_hierarchy] must have at least one column ending in h1, h2, etc. "
    stop(err_msg)
  } else {
    msg("h_vars is: ", str_csv(h_vars))
  }

  # Check for duplicates in `h_vars`
  if (any(duplicated(h_vars))) {
    i <- which(duplicated(h_vars))
    err_msg <- str_c(
      "[import_hierarchy] found duplicate h* vars: ",
      str_csv(h_vars[i]))
    stop(err_msg)
  } else {
    msg("no duplicates in ", strtools::str_or(h_vars))
  }

  h_indices <-
    readr::parse_integer(
      stringr::str_extract(
        h_vars,
        pattern = "[0-9]+$"))

  if (min(h_indices) > 1) {
    err_msg <- "[import_hierarchy] should contain _h0 or _h1"
    stop(err_msg)
  }

  msg("dropping all columns except ", strtools::str_and(id_var, h_vars))
  validated_hierarchy <-
    dplyr::select(
      input_data,
      id_var,
      !!h_vars)

  return(validated_hierarchy)

}
