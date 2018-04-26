#' Unpack a vector of "packed IDs" (like facility IDs or category IDs)
#'
#' @importFrom strtools parse_integers
#'
#' @examples
#' input_data <- data_frame(cat_ids = c("c(1, 3)", "c(1:4)"))
#' unpack_ids(input_data, id_var = "cat_ids")
#' unpack_ids(input_data)
#'
#' @export
unpack_ids <- function (input_data, id_var = NULL, verbose = FALSE)  {

  if (is.null(id_var)) {
    id_var <- vartools::find_var(input_data, suffix = "_id(s?)")
  }

  unpacked <- unpack_integers(input_data, var_name = id_var, verbose = verbose)

  singular_form <- str_replace(id_var, "_ids$", "_id")
  renamed <- rename_(unpacked, .dots = set_names(id_var, singular_form))

  return(renamed)

}
