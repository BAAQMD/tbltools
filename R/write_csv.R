#' Write to a CSV file
#'
#' @note In contrast to \link{write.csv}, \code{row.names} is set to
#'       \code{FALSE}, so that an extra column will not be written.
#'
#' @param input_data      data frame
#' @param file       filename
#' @param prompt     `TRUE` or character; if not `FALSE`, confirmation is requested
#' @param signif     (optional) significant digits (default `Inf`)
#' @param digits     (optional) passed to `format` if `signif` is not specified
#' @param na         how to represent \code{NA} ("" means blank)
#' @param ...        further arguments to \link{write.csv}
#'
#' @return           \code{input_data}, possibly modified. See Note for details.
#'
#' @note If \code{signif} is supplied, then the values that are written will be rounded.
#'       It is these rounded values that are returned (invisibly, for chaining).
#'
#' @seealso \link{read_tbl}
#'
#' @importFrom dplyr mutate_if
#' @importFrom utils write.csv
#' @importFrom rlang is_bare_double
#' @importFrom utils timestamp
#' @export
write_csv <- function (
  input_data,
  file,
  prompt = FALSE,
  signif = Inf,
  digits = 15,
  date_format = "%Y-%m-%d",
  na = "",
  eol = "\r\n",
  row.names = FALSE,
  quote = TRUE,
  ...,
  verbose = getOption("verbose")
) {

  file <- normalizePath(file, mustWork = FALSE)

  msg <- function (...) if(isTRUE(verbose)) message("[write_csv] ", ...)

  dn <- dirname(file)
  if (!file.exists(dn)) {
    msg("creating directory: ", dn)
    dir.create(dn, recursive = TRUE)
  }

  format_date <- function (x) {
    base::format.Date(x, format = date_format)
  }

  msg("stripping non-printable characters")
  format_chr <- function (x) {
    str_printable(x)
  }

  if (is.finite(signif)) {

    msg("rounding to ", signif, " significant digits")
    format_dbl <- function (x) {
      base::signif(x, digits = signif)
    }

  } else {

    msg("keeping ", digits, " digits of precision (use `signif` or `digits` to force rounding)")
    format_dbl <- function(x, ...) {
      formatted <- format(x, digits = digits, ..., drop0trailing = TRUE)
      trimmed <- str_trim(formatted)
      if_else(str_detect(trimmed, "NA"), NA_character_, trimmed)
    }

  }

  # is_numeric_or_decimal <- function (x) {
  #   is.numeric(x) | inherits(x, "decimal")
  # }

  # do_quote <- FALSE

  tidied <-
    input_data %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character, format_chr) %>% # will also apply to factors, due to line above
    mutate_if(is_bare_double, format_dbl) %>%
    # mutate_if(is_bare_double, partial(format_decimal, signif = signif)) %>%
    # mutate_if(Negate(is_numeric_or_decimal), str_quote) %>%
    mutate_if(lubridate::is.Date, format_date)

  if (isTRUE(prompt)) {
    prompt <- glue::glue("Write {nrow(tidied)} records to {file}? [y/N]")
  }

  if (is.character(prompt)) {
    choice <- readline(prompt = prompt)
    if (choice != "y") {
      message("OK, didn't write anything to disk")
      return(invisible(input_data))
    }
  }

  write.csv(
    tidied,
    file = file,
    na = na,
    quote = quote,
    row.names = row.names,  # Not used by readr::write_csv
    eol = eol,              # Not used by readr::write_csv
    ...)

  msg("wrote ", nrow(input_data), " records to ", file)

  timestamp(quiet = TRUE)

  return(invisible(input_data))

}

#' @export
write_tbl <- function (...) {
  .Defunct("write_csv")
}
