#' @include pivot_table.R

#' @rdname pivot_table
#'
#' @export
pivot_chart <- function (
  ...,
  renderer = "Area Chart",
  verbose = getOption("verbose")
) {

  pivot_table(
    ...,
    renderer = renderer,
    verbose = verbose)

}
