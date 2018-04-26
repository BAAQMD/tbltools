context("expand_grid")

test_that("simple case", {

  expect_equal(
    expand_grid(year = c(1990:1991), cat_id = c(77, 88)),
    tibble(year = as.integer(c(1990, 1991, 1990, 1991)), cat_id = c(77, 77, 88, 88)))

})
