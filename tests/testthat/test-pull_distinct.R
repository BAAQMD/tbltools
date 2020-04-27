context("pull_distinct")

test_that("mtcars", {

  expected <-
    c(4, 6, 8)

  expect_setequal(
    pull_distinct(mtcars, cyl),
    expected)

})
