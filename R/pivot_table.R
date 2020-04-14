#' pivot_table() and pivot_chart()
#'
#' Create an interactive pivot table (or chart). Tailored for emission-inventory data.
#'
#' @param rows (character) required (can be a single value or a vector)
#' @param columns (character) looks for `year`
#' @param values (character) looks for `ems_qty` or `tput_qty`
#' @param aggregator (character) passed to [rpivotTable()][rpivotTable::rpivotTable()]
#' @param renderer (character) passed to [rpivotTable()][rpivotTable::rpivotTable()]
#' @param skip_years (optional) drop some years
#' @param inclusions (list) passed to [rpivotTable::rpivotTable()]
#' @param subtotals (logical) passed to [rpivotTable::rpivotTable()]
#' @param menuLimit (integer) passed to [rpivotTable::rpivotTable()]
#' @param sorters (list) expert use only
#' @param ... further arguments to [rpivotTable::rpivotTable()]
#' @param verbose (logical)
#'
#' @seealso
#' - `inventory` [conventions](https://paper.dropbox.com/doc/BY2015-Conventions--Anuj_YBX9ORYU6LdCnlzOo_TAg-lbtgRJluUoJdnflc2VyKx) for tabular data
#' - the [rpivotTable](https://github.com/smartinsightsfromdata/rpivotTable/blob/master/README.md) R package
#' - the Javascript [PivotTable.js](https://pivottable.js.org/examples/) library
#'
#' @export
pivot_table <- function (
  input_data,
  rows,
  columns,
  values,
  aggregator = "Sum",
  renderer = "Heatmap",
  skip_years,
  inclusions = list(),
  subtotals = FALSE,
  menuLimit = 6000,
  file = NULL,
  sorters = list(pol_abbr = c("PM", "PM2.5", "PM10", "TOG", "ROG", "NOx", "SO2", "CO", "CO2", "CH4", "N2O", "HFC+PFC", "NH3")),
  ...,
  verbose = getOption("verbose")
) {

  require(rpivotTable)

  year_var <- "year" # FIXME: don't hardcode!

  if (missing(columns)) {
    # year_var <- "year" # WAS: first(names(input_data) %>% select_vars(ends_with("_yr")) %>% union("year"))
    columns <-
      if ("pol_abbr" %in% names(input_data)) {
        c("pol_abbr", year_var)
      } else {
        year_var
      }
  }

  if (missing(values)) {
    if ("ems_qty" %in% names(input_data)) {
      values <- "ems_qty"
    } else if ("tput_qty" %in% names(input_data)) {
      values <- "tput_qty"
    } else {
      values <- first(tidyselect::vars_select(names(input_data), matches("_qty$")))
    }
  }

  if (isTRUE(verbose)) message("Using ", paste_csv(columns), " as columns")
  if (isTRUE(verbose)) message("Using ", values, " as values")

  if (!missing(skip_years)) {
    year_values <- sort(unique(input_data[[year_var]]))
    if (is.null(inclusions[[year_var]])) { # only set if not already set
      inclusions$year <- every_nth(year_values, skip_years + 1)
    }
  }

  if (length(sorters) == 1) {
    str_quote <- function (x) str_c('"', x, '"')
    js_template <- 'function(attr) { var sortAs = $.pivotUtilities.sortAs; if (attr == "%s") { return sortAs(%s); } }'
    as_js_list <- function (x) {
      x %>% map_chr(str_quote) %>% str_c(collapse = ", ") %>% str_c("[", ., "]")
    }
    js_for <- function (var_levels, var_name) {
      var_levels_concatenated <-
        sprintf(js_template, var_name, as_js_list(var_levels))
    }
    sorter_js <- unname(unlist(imap(sorters, js_for)))
  } else if (is.null(sorters)) {
    sorter_js <- NULL
  } else {
    stop("Don't know how to handle multiple sorters yet, sorry")
  }

  if (isTRUE(subtotals)) {
    if (renderer == "Heatmap") {
      renderer <- "Table With Subtotal Heatmap"
    } else if (renderer == "Table") {
      renderer <- "Table With Subtotal"
    } else {
      msg("expecting a With Subtotal renderer; proceeding anyway")
      # pass
    }
  }

  pivot_args <- list(
    rows = rows,
    cols = columns,
    vals = values,
    aggregatorName = aggregator,
    rendererName = renderer,
    inclusions = inclusions,
    subtotals = subtotals,
    autoSortUnusedAttrs = TRUE,
    menuLimit = menuLimit,
    sorters = sorter_js,
    ...)

  pivot_obj <-
    do.call(
      rpivotTable,
      append(
        list(data = input_data),
        pivot_args))

  if (!is.null(file)) {
    require(htmlwidgets)
    saveWidget(pivot_obj, file = file)
  }

  return(pivot_obj)

}

