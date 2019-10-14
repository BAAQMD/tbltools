context("with_hierarchy")

test_that("test_hierarchy", {

  test_hierarchy %>%
    dim() %>%
    expect_equal(
      c(3, 8))

})

test_that("with_hierarchy (using BY2011 and test_hierarchy)", {

  input_data <-
    BY2011::BY2011_annual_emission_data

  test_data <-
    input_data %>%
    with_hierarchy(
      test_hierarchy)

  test_data %>%
    distinct(cat_id) %>%
    nrow() %>%
    expect_equal(3)

  test_data %>%
    select_at(
      vars(tidyselect::one_of(names(test_hierarchy)))) %>%
    distinct() %>%
    expect_equal(
      test_hierarchy)

})