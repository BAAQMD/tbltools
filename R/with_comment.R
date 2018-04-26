#' Set the comment attribute on an object. Suitable for chaining.
#'
#' @param x any \R object
#' @param value a \code{character} vector, or \code{NULL}.
#'
#' @export
with_comment <- function (x, ...) {
  comment(x) <- str_c(...)
  return(x)
}
