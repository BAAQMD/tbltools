#' Join tables by a single column, allowing for a wildcard character
#'
#' @param x,y tbls to join
#' @param by name of variable to join by (only one is currently supported)
#' @param wildcard a single character (default is \code{?})
#' @param \dots other parameters passed onto methods
#'
#' @importFrom fmatch fmatch
#' @importFrom tidyselect one_of
#' @importFrom stringr str_replace_all
#'
#' @export
wildcard_join <- function (x, y, by, wildcard = "X", keep_pattern = FALSE, ...) {

  stopifnot(length(by) == 1, is.character(by))

  codes <- as.character(x[[by]])
  patterns <- as.character(y[[by]]) %>% stringr::str_replace_all(wildcard, fixed("?"))

  i <- fmatch::fmatch(codes, patterns)
  match_data <- y[i, ]

  if (isTRUE(keep_pattern)) {
    match_data[[str_c(by, "_pattern")]] <- match_data[[by]]
  }

  bind_cols(x, dplyr::select(match_data, -tidyselect::one_of(by)))

}
