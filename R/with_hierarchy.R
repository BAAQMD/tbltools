#' Annotate tabular data with hierarchical headings
#'
#' @description
#' Join hierarchy to `input_data`, yielding additional columns ending in `h1`, `h2`, `h3`, etc.
#'
#' @param input_data tabular data
#' @param using tabular data, OR the name of a `.CSV` file found in `BY2015/Hierarchies/`.
#' @param depth (integer) use at most this many levels
#' @param warn_missing (logical) warn if any records in `input_data` do not have a match in `using`
#' @param verbose (logical)
#'
#' @details
#' - There must exist key column(s) in `input_data` (e.g. `cat_id` or `pol_id`) matching the key column(s) in `using`.
#' - If `using` is a filename/path, then `import_hierarchy()` will be used to import, tidy, reshape, and validate it.
#
#' @note For historical reasons, `with_hierarchy()` effectively defaults to `with_category_hierarchy(using = t1327)`.
#'
#' @seealso
#' - [inventory::with_category_hierarchy()]
#' - [inventory::with_pollutant_hierarchy()]
#' - [inventory::with_SIC_hierarchy()] and the [OSHA SIC system](https://www.osha.gov/pls/imis/sic_manual.html) ([search](https://www.osha.gov/pls/imis/sicsearch.html))
#' - [inventory::with_SCC8_hierarchy()], [with_SCC8_hierarchy()], and the [EPA SCC system](https://ofmpub.epa.gov/sccwebservices/sccsearch/docs/SCC-IntroToSCCs.pdf) ([search](https://ofmpub.epa.gov/sccwebservices/sccsearch/))
#'
#' @export
with_hierarchy <- function (
  input_data,
  using = NULL,
  depth = NULL,
  ...,
  warn_missing = FALSE,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[with_hierarchy] ", ...)

  #
  # Construct `hierarchy` (a data frame) based on `using`
  # (which may be a file path or a data frame)
  #
  if (is.null(using)) {

    # Handle the awkward legacy case.
    stop("[with_hierarchy] you must now pass a `using` argument. Try `using = Ingres::t1327`?")

  }

  is_tx327_data <- function (x) {
    tx327_names <- c("p", "c1_60", "c61_70", "j1", "j2")
    return(as.logical(setequal(names(x), tx327_names)))
  }

  if (is.character(using)) {

    # Handle the case where `using` is just a file name, by searching through
    # known repositories (just the `BY2015` directory in Dropbox for now)
    if (dirname(using) == ".") {
      dn <- droptools::my_dropbox("BY2015", "Hierarchies")
      warning("[with_hierarchy] defaulting to ", dn, ". Please be more specific if you can!")
      using <- file.path(dn, using)
    }

    # This can handle either a full path (character) or tabular data
    hierarchy_object <-
      import_hierarchy(
        file = using,
        verbose = verbose)

  } else if (is_tx327_data(using)) {

    # Handle the case where a t-table is provided directly
    # (it has to be reshaped!)
    hierarchy_object <-
      DataBank::DB_hierarchy(
        using = using,
        verbose = verbose)

  } else if (inherits(using, "data.frame")) {

    # Handle the case where `using` is tabular data
    #
    # Note: validate_hierarchy() is invoked below.
    #
    hierarchy_object <-
      using

  } else {

    stop("[with_hierarchy] sorry, don't know how to construct hierarchy using that!")

  }

  # Auto-detect id_var
  id_var <-
    vartools::find_var(
      hierarchy_object,
      suffix = "_id")

  # Check for (reasonably anticipated) issues
  hierarchy_object <-
    validate_hierarchy(
      hierarchy_object,
      id_var = id_var,
      verbose = verbose)

  # Auto-detect "heading" columns
  h_vars <-
    tidyselect::vars_select(
      names(hierarchy_object),
      dplyr::matches("_h[0-9]+$"))

  if (is.null(depth)) {
    depth <- length(h_vars)
  }

  # Sort "headings", and (if requested) drop any that are "too deep"
  h_prefix <-
    str_replace(id_var, "_id$", "_h")

  h_vars <-
    tidyselect::vars_select(
      h_vars,
      num_range(h_prefix, 0:depth))

  recode_h_var <- function (x) {
    return(as.character(x))
  }

  # Trim and coerce colums to integer/character, and sort by id_var
  tidied_hierarchy <-
    hierarchy_object %>%
    dplyr::select(
      id_var,
      h_vars) %>%
    mutate_at(
      vars(h_vars),
      ~ recode_h_var(.)) %>%
    arrange_at(
      vars(id_var))

  msg("md5 digest of tidied hierarchy is ", md5(tidied_hierarchy))

  # Strip any existing `_h1`, `_h2`, ... from input_data
  preexisting_h_vars <-
    names(input_data) %>%
    tidyselect::vars_select(
      tidyselect::matches("_h[0-9]+$")) %>%
    intersect(
      names(tidied_hierarchy))

  tidied_input_data <-
    input_data %>%
    drop_vars(
      preexisting_h_vars)

  if (isTRUE(warn_missing)) {

    msg("`warn_missing` is TRUE")

    unmatched_ids <-
      tidied_input_data %>%
      anti_join(
        tidied_hierarchy,
        by = id_var) %>%
      pull(
        id_var) %>%
      na.omit()

    if (length(unmatched_ids) > 0) {
      warning("[with_hierarchy] these ids have no match in the hierarchy: ", pack_integers(unmatched_ids))
    } else {
      msg("every id has a match in the hierarchy (good)")
    }

    unmatched_labels <-
      anti_join(
        tidied_hierarchy,
        tidied_input_data,
        by = id_var)

    if (nrow(unmatched_labels) > 0) {
      msg("extra records found in hierarchy (OK if your data is not a full inventory)")
    }

    msg("performing a left_join() on ", id_var)
    annotated_data <-
      tidied_input_data %>%
      left_join(
        tidied_hierarchy,
        by = id_var)

  } else {

    msg("performing an inner_join() on ", id_var, " (`warn_missing` is not TRUE)")
    annotated_data <-
      tidied_input_data %>%
      inner_join(
        tidied_hierarchy,
        by = id_var) %>%
      ensurer::ensure(
        !any(is.na(.[[id_var]])))

  }

  comment(annotated_data) <-
    comment(input_data)

  return(annotated_data)

}
