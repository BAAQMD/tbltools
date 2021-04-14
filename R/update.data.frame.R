#' update.data.frame
#'
#' Analogous to SQL UPDATE command
#'
#' @export
#' @importFrom lazyeval lazy_eval expr_text
#' @importFrom purrr reduce
#' @importFrom dplyr mutate bind_rows
update.data.frame <- function (input_data, .where, ..., verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[update.data.frame] ", ...)

  # Logical union of all conditions
  i <- which(reduce(lazy_eval(.where, input_data), `&`))

  msg(" [", expr_text(.where), "] altering ", length(i), " values")

  # Mutate rows indexed by `i`
  mutated <- mutate(input_data[i, ], ...)

  # Leave the other rows alone
  untouched <- input_data[-i, ]

  # Put the mutated and the untouched rows back together
  bind_rows(mutated, untouched)

}
