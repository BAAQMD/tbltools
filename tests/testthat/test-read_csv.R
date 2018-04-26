context("read_csv")

test_that("simple", {

  expect_equal(
    read_csv(test_csv_file),
    test_csv_data)

})

test_that("verbose", {

  expect_message(
    read_csv(test_csv_file, verbose = TRUE),
    "3 records")

  expect_equal(
    read_csv(test_csv_file, verbose = TRUE),
    test_csv_data)

})

test_that("nonexistent file", {

  nonexistent_file <- tempfile()
  expect_false(file.exists(nonexistent_file))

  expect_error(
    read_csv(nonexistent_file),
    "File not found")

})
