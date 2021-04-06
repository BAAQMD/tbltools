#' (Left) join with a custom comparator
#'
#' @param left  data.frame
#' @param right data.frame
#' @param by    names of columns to join by
#' @param fun   custom comparator (see examples)
#'
#' @export
fun_join <- function (left, right, by = intersect(names(left), names(right)), fun = `==`) {

  left_data <- as.data.frame(left)
  right_data <- as.data.frame(right)

  # Stack of (logical) matrices. FIXME: Optimize this!
  dn <- list(left = row.names(left_data), right = row.names(right_data), by = by)
  A <- array(NA, dim = lengths(dn), dimnames = as.vector(dn))
  for (j in by) {
    xl <- left_data[, j, drop = TRUE]
    xr <- right_data[, j, drop = TRUE]
    A[, , j] <- outer(xl, xr, fun)
  }

  # Matching rows
  i <- which(apply(A, c("left", "right"), all), arr.ind = TRUE)
  il <- i[, "left", drop = TRUE]
  ir <- i[, "right", drop = TRUE]

  # Don't need any `by` columns from right_data
  jr <- setdiff(names(right_data), by)

  joined <- cbind(left_data[il, ], right_data[ir, jr, drop = FALSE])
  return(joined)

}
