#' Read a worksheet from an XLS or XLSX file
#'
#' @param file filename
#' @param sheet name or number (starting from 1)
#' @param ... further arguments to \link[read_excel]{readxl}
#'
#' @return \link[tbl_df]{dplyr}
#'
#' @importFrom readxl read_excel
#' @importFrom downloader download
#' @importFrom digest digest
#' @importFrom utils read.csv
#' @importFrom stringr str_detect
#'
#' @export
read_xls <- function (file, expect_md5 = NULL, sheet = 1, ..., verbose = getOption("verbose")) {

  if (file %>% str_detect("^http")) {
    url <- file
    ext <- str_extract(url, regex("\\.xlsx?$", ignore_case = TRUE))
    file <- file.path(tempdir(), str_c(md5(url), ext))
    message("Downloading as ", file)
    downloader::download(url, destfile = file)
  }

  if (!is.null(expect_md5)) {
    file_md5 <- digest::digest(readLines(file), "md5")
    if (identical(expect_md5, file_md5)) {
      if (verbose) message("md5 matches expected: ", file_md5)
    } else {
      stop("md5 does not match expected: ", file_md5, " != ", expect_md5)
    }
  }

  readxl::read_excel(file, sheet = sheet, ...)

}
