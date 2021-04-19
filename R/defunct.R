#' @importFrom lubridate is.POSIXt
#'
#' @export
#' @importFrom rlang int
#' @importFrom stringr str_replace_all
#' @importFrom dplyr select_ funs
#' @importFrom knitr kable
print_tbl <- function (
  input_data,
  caption = comment(input_data),
  digits = getOption("digits"),
  na = "",
  tiny = "\U2014", # en dash
  zero = "",       # empty string
  style = "simple",
  split_tables = Inf,
  trailing_zeros = TRUE,
  non_breaking = TRUE,
  big.mark = ",",
  column_totals = FALSE,
  row_totals = FALSE,
  verbose = getOption("verbose"),
  ...
) {

  .Defunct()

  # Commented out by alamstein 4/19/21.
  # Function is now .Defunct() so we could technically delete all code
  # But just commenting out these lines to satisfy R CMD check which
  # prefers we not require() packages from within functions in packages
  #require(pander)
  #require(lubridate)

  if (nrow(input_data) == 0) {
    message("No data")
    return(invisible(NULL))
  }

  if (!missing(row_totals)) {
    .Deprecated("row_totals", msg = "Please substitute `column_totals` for the (improperly named) `row_totals`. Thank you. (The Times regrets the error.)")
    column_totals <- row_totals
  }

  which_vars <- function (input_data, FUN) names(input_data)[which(sapply(input_data, FUN))]

  id_vars <- tidyselect::vars_select(names(input_data), matches("_id$"))
  int_vars <- which_vars(input_data, is.integer) %>% union(intersect(names(input_data), "year"))
  num_vars <- which_vars(input_data, is.double) %>% setdiff(union(int_vars, id_vars))

  chr_vars <- which_vars(input_data, is.character)
  fctr_vars <- which_vars(input_data, is.factor)
  POSIXt_vars <- which_vars(input_data, is.POSIXt)
  str_vars <- Reduce(union, list(chr_vars, fctr_vars, POSIXt_vars))

  # Default alignments
  align <- rep("l", ncol(input_data))
  names(align) <- names(input_data)
  align[num_vars] <- "r"
  align[id_vars] <- "c"

  # Alignments specified by user
  # align[intersect(format_left, names(input_data))] <- "l"
  # align[intersect(format_center, names(input_data))] <- "c"

  fmt_id <- . %>% int() %>% format(na.encode = TRUE) %>% str_replace_all("NA", "")
  fmt_int <- . %>% int() %>% format(na.encode = TRUE) %>% str_replace_all("NA", "")
  fmt_num <- . %>% humanize(digits = digits, tiny = tiny, zero = zero) %>% str_replace_all("NA", "")
  fmt_str <- . %>% format(na.encode = FALSE) %>% replace_which(is.na(.), "")

  if (isTRUE(column_totals)) {
    column_totals <- input_data %>% select_(.dots = num_vars) %>% total_each()
    preformatted <- bind_rows(input_data, column_totals)
  } else {
    preformatted <- input_data
  }

  if (length(id_vars) > 0) {
    if (verbose) message("Formatting as ID:\t", paste_csv(id_vars))
    id_cols <- as.character(match(id_vars, names(input_data))) # WORKAROUND for dplyr bug
    preformatted <- mutate_at(preformatted, vars(one_of(id_cols)), funs(fmt_id))
  }

  if (length(int_vars) > 0) {
    if (verbose) message("Formatting as integer:\t", paste_csv(int_vars))
    int_cols <- as.character(match(int_vars, names(input_data))) # WORKAROUND for dplyr bug
    preformatted <- mutate_at(preformatted, vars(one_of(int_cols)), funs(fmt_int))
  }

  if (length(num_vars) > 0) {
    if (verbose) message("Formatting as numeric:\t", paste_csv(num_vars))
    num_cols <- as.character(match(num_vars, names(input_data))) # WORKAROUND for dplyr bug
    preformatted <- mutate_at(preformatted, vars(one_of(num_cols)), funs(fmt_num))
  }

  if (length(str_vars) > 0) {
    if (verbose) message("Formatting as string:\t", paste_csv(str_vars))
    str_cols <- as.character(match(str_vars, names(input_data))) # WORKAROUND for dplyr bug
    preformatted <- mutate_at(preformatted, vars(one_of(str_cols)), funs(fmt_str))
    if (non_breaking) {
      preformatted <- preformatted %>% mutate_at(vars(one_of(str_cols)), funs(format_nonbreaking))
    }
  }

  preformatted %>% kable(
    format = "pandoc",
    caption = caption,
    align = align,
    ...)

}
