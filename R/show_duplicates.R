#' @export
show_duplicates <- function (.data, ...) {
  grouped <- group_by(.data, ...)
  dupes <- filter(tally(grouped), n > 1)
  semi_join(.data, dupes)
}

