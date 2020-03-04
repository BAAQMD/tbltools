#' bind_inventories
#'
#' Stack two inventories on top of one another.
#'
#' @param ... one or more tabular datasets
#' @param .id you can override this
#' @param verbose display messages
#'
#' @export
bind_inventories <- function (
  ...,
  .id = "inventory",
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[bind_inventories] ", ...)

  data_list <-
    list(...)

  cat_id_classes <-
    map(
      data_list,
      ~ class(.$cat_id)) %>%
    unlist()

  if ("character" %in% cat_id_classes) {

    msg("coercing `cat_id` to character")

    data_list <-
      map(
        data_list,
        ~ mutate_at(
          .,
          vars(cat_id),
          list(as.character)))

  }

  stacked_data <-
    bind_rows(
      data_list,
      .id = .id)

  ems_unit_values <-
    pull_distinct(
      stacked_data,
      ems_unit)

  if (!all_same(ems_unit_values)) {

    err_msg <- paste0(
      "inventories must have same `ems_unit`. ",
      "Use convert_emission_units() first.")

    stop(err_msg)

  }

  return(stacked_data)

}
