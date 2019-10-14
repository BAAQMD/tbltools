context("import_hierarchy")

test_that("BY2015_TAC_pollutant_hierarchy", {

  csv_path <-
    droptools::my_dropbox(
      "BY2015",
      "Hierarchies",
      "BY2015_TAC_pollutant_hierarchy.csv")

  test_hierarchy <-
    import_hierarchy(
      csv_path,
      verbose = TRUE)

  test_hierarchy %>%
    expect_is(
      "data.frame")

  id_vars <-
    names(test_hierarchy) %>%
    tidyselect::vars_select(
      tidyselect::matches("_id$"))

  expect_true(
    length(id_vars) == 1)

})
