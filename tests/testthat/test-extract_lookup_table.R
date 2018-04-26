context("extract_lookup_table")

input_data <- tribble(
  ~ fac_id, ~ src_id, ~ src_name, ~ comment,
  10L, 1L, "foo", "hello world",
  10L, 2L, "bar", "bonjour monde",
  10L, 2L, "bar", "this doesn't matter",
  10L, 2L, "baz", "I'm a duplicate")

test_that("duplicates", {

  expect_error(
    extract_lookup_table(input_data, fac_id, src_id, src_name),
    "duplicates detected")

})

test_that("success", {

  sliced <- slice(input_data, 1:3)
  extracted <- extract_lookup_table(sliced, fac_id, src_id, src_name)

  expected <- data_frame(fac_id = 10L, src_id = 1:2L, src_name = c("foo", "bar"))
  expect_equal(extracted, expected)

})
