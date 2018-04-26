#' Read from a CSV file
#'
#' @param file filename
#' @param ... further arguments to \link[read_csv]{readr}
#'
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
#' @importFrom purrr safely
#'
#' @export
read_csv <- function (file, ..., verbose = getOption("verbose")) {

  msg <- function (...) if (isTRUE(verbose)) message("[read_csv] ", ...)

  if (!file.exists(file)) {
    error_msg <- str_c("[read_csv] File not found: ", file)
    stop(error_msg)
  }

  safely_read_csv <- safely(readr::read_csv, quiet = TRUE)
  safe_attempt <- suppressMessages(safely_read_csv(file, ...))
  errors <- safe_attempt[["error"]]
  csv_data <- safe_attempt[["result"]]

  if (!is.null(errors)) {

    lapply(errors, warning)
    warning("Falling back to read.csv instead of readr::read_csv")
    csv_data <- read.csv(file, ..., stringsAsFactors = FALSE, check.names = FALSE)

  }

  try(msg("read ", nrow(csv_data), " records from ", file))

  return(as_tibble(csv_data))

}
