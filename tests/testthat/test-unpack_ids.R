context("unpack_ids")

input_data <- data_frame(cat_ids = c("c(1, 3)", "c(1:4)"))

# N.B. expect `cat_ids` (plural) to become `cat_id` (singular)
expected <- data_frame(cat_id = as.integer(c(1, 3, 1, 2, 3, 4)))

test_that("explicit id_var", {

  expect_identical(
    unpack_ids(input_data, id_var = "cat_ids"),
    expected)

})

test_that("implicit id_var", {

  expect_identical(
    unpack_ids(input_data),
    expected)

})

test_that("verbosity", {

  expect_message(
    unpacked <- unpack_ids(input_data, verbose = TRUE),
    "unpacking cat_ids")

})

expect_identical(
  suppressMessages(unpack_ids(input_data)),
  expected)


