context("with_hierarchy")

test_that("test_hierarchy", {

  test_hierarchy %>%
    dim() %>%
    expect_equivalent(
      c(3, 8))

})

test_that("with_hierarchy (using BY2011 and test_hierarchy)", {

  test_data <-
    BY2011::BY2011_annual_emission_data %>%
    with_hierarchy(test_hierarchy) %>%
    arrange(cat_id)

  test_data %>%
    distinct(cat_id) %>%
    nrow() %>%
    expect_equivalent(3)

  test_data %>%
    select_at(
      vars(tidyselect::one_of(names(test_hierarchy)))) %>%
    distinct() %>%
    arrange(cat_id) %>%
    expect_equivalent(
      test_hierarchy)

})

test_that("using=t0327", {

  test_data <-
    tibble(cat_id = 838)

  expect_warning(
    with_hierarchy(
      test_data,
      using = Ingres::t0327),
    "deprecated")

})
