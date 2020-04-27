context("divide_at")

test_that("single column, fixed value", {

  x <- 2

  expect_equal(
    divide_at(mtcars, vars(cyl), by = x),
    mutate_at(mtcars, vars(cyl), ~ . / x))

})

test_that("single column, variable in data", {

  x <- pull(mtcars, mpg)

  expect_equal(
    divide_at(mtcars, vars(cyl), by = mpg),
    mutate_at(mtcars, vars(cyl), ~ . / mpg))

})

test_that("everything, variable in data", {

  x <- pull(mtcars, cyl)

  expect_equal(
    divide_at(mtcars, vars(everything()), by = cyl),
    mutate_at(mtcars, vars(everything()), ~ . / x))

})

test_that("grouped data", {

  x <- pull(billboard, wk1)

  # expected result doesn't actually depend on grouping
  expected <-
    mutate_at(billboard, vars(wk1, wk2), ~ . / x)

  grouped_data <-
    group_by(billboard, artist)

  expect_equal(
    divide_at(grouped_data, c('wk1', 'wk2'), wk1),
    expected)

})
