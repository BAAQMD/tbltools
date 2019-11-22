context("filter_categories")

test_data <-
  tibble(
    cat_id = 1:1000,
    cat_h0 = "Area")

test_that("unnamed categories", {

  expected <-
    test_data %>%
    filter(
      cat_id %in% c(283:285))

  test_data %>%
    filter_categories(
      283:285) %>%
    expect_equal(
      expected)

  test_data %>%
    filter_categories(
      283, 284, 285) %>%
    expect_equal(
      expected)

  test_data %>%
    filter_categories(
      c(283, 284), 285) %>%
    expect_equal(
      expected)

})

test_that("named categories", {

  test_categories <- c(
    "Space Heating" = 283L,
    "Water Heating" = 284L,
    "Cooking" = 285L)

  expected <-
    enframe(
      test_categories,
      name = "cat_abbr",
      value = "cat_id") %>%
    mutate_at(
      vars(cat_abbr),
      ~ factor(., levels = names(test_categories))) %>%
    mutate(
      cat_h0 = "Area")

  test_data %>%
    filter_categories(
      test_categories) %>%
    expect_equal(
      expected)

  test_data %>%
    filter_categories(
      "Space Heating" = 283L, "Water Heating" = 284L, "Cooking" = 285L) %>%
    expect_equal(
      expected)

  test_data %>%
    filter_categories(
      c("Space Heating" = 283L, "Water Heating" = 284L), "Cooking" = 285L) %>%
    expect_equal(
      expected)

})