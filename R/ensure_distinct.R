#' Ensure uniqueness of records
#'
#' @param input_data data frame
#' @param \dots key vars (like `fac_id`, `src_id`)
#'
#' @importFrom dplyr count slice
#' @importFrom strtools pack_integers
#'
#' @export
ensure_distinct <- function (input_data, ...) {

  counts <- count(input_data, ...)

  # Why doesn't a simple `with(input_data, n_distinct(...))` work here?
  # attach(input_data)
  # n <- n_distinct(...)
  # detach(input_data)

  dupes <- filter(counts, n > 1)

  if (nrow(dupes) == 0) {

    return(input_data)

  } else {

    print(dupes)
    stop("Duplicates detected")

  }

}
