#' read_ncdf
#'
#' Import the contents of a NetCDF file as a `tibble`.
#'
#' @param path character
#' @param ... reserved for future use
#' @param verbose logical
#'
#' @return
#' @export
#'
#' @examples
read_ncdf <- function(
  path,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) {
    message("[read_ncdf_data] ", ...)
  }

  ncdf_object <-
    tidync::tidync(path)

  ncdf_data <-
    as_tibble(
      tidync::hyper_tibble(
        ncdf_object))

  attr(ncdf_data, "path") <-
    path

  return(ncdf_data)

}
