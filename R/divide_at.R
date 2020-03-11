#' @include multiply_at.R

#' divide_at
#'
#' @rdname multiply_at
#'
#' @examples
#' mtcars %>% divide_at(vars(everything), cyl)  # "normalizes" by `cyl`
#'
#' @export

divide_at <- function (
  .tbl,
  .vars,
  by,
  ...,
  .cols = NULL
) {

  .by <- expr(1 / {{ by }})

  divided <-
    multiply_at(
      .tbl,
      .vars,
      by = !!.by,
      ...,
      .cols = .cols)

  return(divided)

}
