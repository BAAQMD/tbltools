#' bind_inventories
#'
#' Stack two inventories on top of one another.
#'
#' @param ... one or more tabular datasets
#' @param .id you can override this
#' @param verbose display messages
#'
bind_inventories <- function (
  ...,
  .id = "inventory",
  verbose = getOption("verbose")
) {

  stacked_data <-
    bind_rows(
      ...,
      .id = .id)

  return(stacked_data)

}
