#' Read tabular data
#'
#' @description Efficiently read
#' tabular data from CSV files or Excel worksheets.
#'
#' @note In contrast to \link{read.csv}, \code{stringsAsFactors} and
#' \code{check.names} are effectively \code{FALSE}.
#'
#' @param file  name of a .csv, .xls, or .xlsx file
#' @param ...  further arguments to \link{read_csv} or \link{read_xls}
#' @param verbose  whether to print status messages
#'
#' @importFrom stringr str_match
#'
#' @return a \link[dplyr]{tbl_df}
#'
#' @export
read_tbl <- function (file, ..., verbose = getOption("verbose")) {

  if (verbose)
    message("[read_tbl] file: ", file)

  file <- normalizePath(file, mustWork = TRUE)

  ext_pattern <- "\\.([A-Za-z0-9]+)$"
  ext_matches <- stringr::str_match(file, ext_pattern)
  file_ext <- ext_matches[-1]

  switch (
    file_ext,
    "csv" = read_csv(file, ...),
    "xls" = read_xls(file, ...),
    "xlsx" = read_xls(file, ...),
    stop("Not sure how to import .", file_ext, " files")
  )

}
