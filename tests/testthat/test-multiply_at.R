context("multiply_at")

test_that("single column, fixed value", {

  x <- 2

  expect_equivalent(
    multiply_at(mtcars, vars(cyl), by = x),
    mutate_at(mtcars, vars(cyl), ~ . * x))

})

test_that("single column, variable in data", {

  x <- pull(mtcars, mpg)

  expected <-
    mutate_at(mtcars, vars(cyl), ~ . * x)

  expect_equivalent(
    multiply_at(mtcars, vars(cyl), by = mpg),
    expected)

})

test_that("everything, variable in data", {

  x <- pull(mtcars, cyl)

  expected <-
    mutate_at(mtcars, vars(everything()), ~ . * x)

  expect_equivalent(
    multiply_at(mtcars, vars(everything()), by = cyl),
    expected)

})

test_that("grouped data", {

  x <- pull(billboard, wk1)

  # expected result doesn't actually depend on grouping
  expected <-
    mutate_at(billboard, vars(wk1, wk2), ~ . * x)

  grouped_data <-
    group_by(billboard, artist)

  expect_equivalent(
    multiply_at(grouped_data, c('wk1', 'wk2'), wk1),
    expected)

})
