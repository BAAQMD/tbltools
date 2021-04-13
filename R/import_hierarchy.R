#' Import a hierarchy from a .CSV file
#'
#' @param file, ... (character) path to .CSV file
#' @param verbose (logical)
#'
#' @seealso [with_hierarchy] is better for general usage (and relies on this function).
#'
#' @export
#' @importFrom dplyr select_if
import_hierarchy <- function (
  file,
  ...,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[import_hierarchy] ", ...)

  full_path <-
    normalizePath(
      file,
      ...,
      mustWork = TRUE)

  msg("importing ", full_path)

  # select only the columns that are not all NA
  hierarchy_object <-
    full_path %>%
    read_tbl() %>%
    select_if(
      ~ !(all_true(is.na(.)))) %>%
    validate_hierarchy(
      verbose = verbose)

  return(hierarchy_object)

}
