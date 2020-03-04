BY2011_test_data <-
  tibble(
    cat_id = 10L,
    cnty_abbr = c("ALA", "CC", "MAR", "NAP", "SF", "SM", "SNC", "SOL", "SON"),
    ems_qty = rnorm(n = 9),
    ems_unit = "ton/yr")

#
# Here, `cat_id` is character, not integer.
# Also, there is no `cnty_abbr`.
#
BY2008_test_data <-
  tibble(
    cat_id = "P10",
    ems_qty = 12345,
    ems_unit = "ton/yr")

test_that("simple case works", {

  test_data <-
    bind_inventories(
      foo = BY2011_test_data,
      bar = BY2011_test_data) # just bind it to itself

  expect_equal(
    nrow(test_data),
    nrow(BY2011_test_data) * 2)

  expect_true(
    "inventory" %in% names(test_data))

  expect_setequal(
    unique(test_data[["inventory"]]),
    c("foo", "bar"))

})

test_that("all arguments must be named", {

  expect_error(

    bind_inventories(
      BY2011_test_data,
      BY2011_test_data),

    "all arguments must be named")

})

test_that("integer and character `cat_id` can be combined", {

  expect_message(

    test_data <-
      bind_inventories(
        BY2011 = BY2011_test_data,
        BY2008 = BY2008_test_data,
        verbose = TRUE),

    "coercing `cat_id` to character")

  expect_equal(
    nrow(test_data),
    nrow(BY2011_test_data) + nrow(BY2008_test_data))

  expect_true(
    "inventory" %in% names(test_data))

  #
  # FIXME: expect factor levels, in same order as provided to
  # `bind_inventories()`.
  #
  expect_setequal(
    unique(test_data[["inventory"]]),
    c("BY2011", "BY2008"))

})


test_that("must have same `ems_unit`", {

  expect_error(

    BY2011_test_data %>%
      mutate(
        ems_unit = "lb/day") %>%
      bind_inventories(
        BY2011 = .,
        BY2008 = BY2008_test_data),

    "must have same `ems_unit`")

})

test_that("names are converted to factor levels, in the order supplied", {

  test_data <-
    bind_inventories(
      BY2011 = BY2011_test_data,
      BY2008 = BY2008_test_data)

  expect_s3_class(
    test_data[["inventory"]],
    "factor")

  expect_equal(
    levels(test_data[["inventory"]]),
    c("BY2011", "BY2008"))

  #
  # Now try the opposite order
  #

  test_data <-
    bind_inventories(
      BY2008 = BY2008_test_data,
      BY2011 = BY2011_test_data)

  expect_equal(
    levels(test_data[["inventory"]]),
    c("BY2008", "BY2011"))

})
