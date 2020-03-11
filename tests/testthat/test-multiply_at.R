context("multiply_at")

test_that("single column, fixed value", {

  x <- 2

  expect_equal(
    multiply_at(mtcars, vars(cyl), by = x),
    mutate_at(mtcars, vars(cyl), ~ . * x))

})

test_that("single column, variable in data", {

  x <- pull(mtcars, mpg)

  expect_equal(
    multiply_at(mtcars, vars(cyl), by = mpg),
    mutate_at(mtcars, vars(cyl), ~ . * mpg))

})

test_that("everything, variable in data", {

  x <- pull(mtcars, cyl)

  expect_equal(
    multiply_at(mtcars, vars(everything()), by = cyl),
    mutate_at(mtcars, vars(everything()), ~ . * x))

})
