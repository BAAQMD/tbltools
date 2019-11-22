#' @noRd
#'
#' @seealso filter_facilities filter_pollutants
filter_and_label <- function (input_data, whitelist, id_var, label_var, ..., overwrite = TRUE, verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[filter_and_label] ", ...)

  input_vars <- names(input_data)

  # Make sure `id_var` is actually in `input_data`
  tryCatch(
    id_var <- select_vars(input_vars, id_var),
    error = function (e) stop(str_c("Could not find ", id_var, " in your data")))

  # Try to warn the user about accidental overwrites
  if (label_var %in% input_vars) {
    if (isTRUE(overwrite)) {
      msg("overwriting ", label_var, " with new values")
    } else {
      stop_msg <- str_c("declining to overwrite ", label_var, " (it's already in your data).")
      stop(stop_msg)
    }
  }

  # We can only handle NAMED lists or vectors
  if (is.list(whitelist) | is.numeric(whitelist)) {
    is_named <- function (x) !(is.null(names(x)))
    if (is_named(whitelist)) {
      # OK; pass
    } else {
      stop_msg <- "[filter_and_label] must filter using a NAMED list or vector"
      stop(stop_msg)
    }
  } else {
    stop_msg <- "[filter_and_label] must filter using a list or vector"
    stop(stop_msg)
  }

  unpacked_codec <- unpack_list(whitelist)

  i <- which(input_data[[id_var]] %in% unname(unpacked_codec))
  filtered <- slice(input_data, i)
  filtered[[label_var]] <- decode(filtered[[id_var]], unpacked_codec)

  return(filtered)

}
