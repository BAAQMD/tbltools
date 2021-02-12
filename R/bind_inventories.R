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

  #
  # Check that all arguments in `...` are named.
  #

  if (is.null(names(data_list))) {

    #
    # TODO: support extraction of patterns like "BY[0-9]{4}_", "BY_", "RY_",
    # etc. from the dot-arg symbols (e.g. `BY2011_annual_emission_data").
    #
    err_msg <- "all arguments must be named."
    stop(err_msg)

  }

  # Bit of a hack to support merging of the `.id` column, later on.
  names(data_list)[names(data_list) == ""] <- "NA"

  #
  # Check that all inventories in `...` have the same `ems_unit`.
  #

  ems_unit_values <-
    map(
      data_list,
      ~ pull(., ems_unit)) %>%
    unlist()

  msg("ems_unit is: ", strtools::str_csv(ems_unit_values))

  if (!all_same(ems_unit_values)) {

    err_msg <- paste0(
      "inventories must have same `ems_unit`. ",
      "Try using convert_emission_units() first?")

    stop(err_msg)

  }

  #
  # If `cat_id` is of type "character" for any inventory (e.g. BY2008),
  # then for each inventory, coerce `cat_id` to character as well.
  #
  # Otherwise, `bind_rows()` would throw an error. It's type-sensitive.
  #

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

  # FIXME: coercing `year` to character is a hotfix for problem stacking dfs when `year` is of class `CY`.
  # Won't be needed once the updated `yeartools` package (2021.02.07+) is installed.
  data_list <-
    map(
      data_list,
      ~ mutate(., year = as.character(year)))

  if (is.null(names(data_list))) {
    err_msg <- "Arguments to `bind_inventories()` must be named."
    stop(err_msg)
  }


  #
  # Stack the inventories together.
  #
  # Then, make the `inventory` column into a factor. Its levels should be in the
  # order that the names of `...` were provided.
  #

  stacked_data <-
    bind_rows(
      data_list,
      .id = ".name") %>%
    mutate(
      .name = na_if(.name, "NA"))

  if (.id %not_in% names(stacked_data)) {
    stacked_data[[.id]] <- NA_character_
  }

  labeled_data <-
    stacked_data %>%
    mutate(
      !!.id := coalesce(
        .[[.id]],
        .name)) %>%
    mutate_at(
      vars(!!.id),
      fct_inorder) %>%
    select(
      -.name)

  return(labeled_data)

}
