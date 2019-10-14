#' Patch data on the fly.
#'
#' @param object  to be patched
#' @param cond    logical condition(s) to be evaluated within scope of object
#' @param \dots   name-value pairs
#' @param quiet   suppress messages
#'
#' @importFrom lazyeval lazy_dots lazy_eval
#'
#' @examples
#'   patch(mtcars, where(vs == 0, am == 1), gear = Inf, carb = carb + 10)
#'
#' @export
patch <- function (object, cond, ...) UseMethod("patch")

#' @export
patch.data.frame <- function (object, cond, ..., quiet = FALSE) {

  # Rows to be patched
  masks <- lazyeval::lazy_eval(cond, object)
  i <- which(apply(do.call(cbind, masks), 1, all))  # rows to be patched
  if (length(i) == 0) {
    warning("conditions are not all TRUE for any rows: nothing patched")
  } else {
    if (!quiet) message("Patching ", length(i), " rows")
  }

  # Columns to be patched
  dots <- lazyeval::lazy_dots(...)
  j <- match(names(dots), names(object))
  if (length(j) == 0) warning("no common names: nothing patched")

  x <- lazyeval::lazy_eval(dots, data = object[i, ])  # replacement values
  object[i, j] <- data.frame(i, x, stringsAsFactors = FALSE)[, -1]  # use `i` to force identical shape
  return(object)
}
