context("validate_hierarchy")

test_that("nothing ends in _id", {
  input_data <- tibble(foo = c(1:3))
  expect_error(validate_hierarchy(input_data))
})

test_that("nothing ends in _h", {
  input_data <- tibble(cat_id = c(1:3), foo = "bar")
  expect_error(
    validate_hierarchy(input_data),
    "must have at least one column ending in h1, h2, etc.")
})

test_that("no _h0 or _h1", {
  input_data <- tibble(cat_id = c(1:3), cat_h2 = "bar")
  expect_error(
    validate_hierarchy(input_data),
    "should contain _h0 or _h1")
})

test_that("_h* vars not sequential", {
  input_data <- tibble(cat_id = c(1:3), cat_h1 = "foo", cat_h3 = "bar")
  expect_error(
    validate_hierarchy(input_data),
    "not sequential")
})

test_that("output is sorted and doesn't contain comments", {

  input_data <-
    tibble(
      cat_id = c(1:3),
      cat_h2 = LETTERS[1:3],
      cat_h1 = "foo",
      comment = c("apple", "orange", "pear"))

  validated_hierarchy <-
    validate_hierarchy(input_data)

  expect_equivalent(
    names(validated_hierarchy),
    c("cat_id", "cat_h1", "cat_h2"))

})

# test_that("packed IDs", {
#
#   input_data <-
#     tibble(
#       cat_id = c("1", "3", "c(1:4)"),
#       cat_h1 = c("foo", "bar", "baz"),
#       cat_h2 = LETTERS[1:3])
#
#   validated_hierarchy <-
#     validate_hierarchy(input_data)
#
#   expect_equivalent(
#     names(validated_hierarchy),
#     c("cat_id", "cat_h1", "cat_h2"))
#
# })
