test_that("x with one column", {
  x <- data.frame(cyl = 6)
  filtered <- filter_set(mtcars, x)
  expected <- filter(mtcars, cyl == 6)
  expect_identical(filtered, expected)
})

test_that("x with two columns", {
  x <- data.frame(cyl = 6, vs = 0)
  filtered <- filter_set(mtcars, x)
  expected <- filter(mtcars, cyl == 6, vs == 0)
  expect_identical(filtered, expected)
})

test_that("missing x", {
  filtered <- filter_set(mtcars)
  expect_identical(filtered, mtcars)
})

test_that("x with one column but zero rows (i.e., empty)", {
  expect_warning(filtered <- filter_set(mtcars, data.frame(mpg = numeric())))
  expect_identical(filtered, mtcars)
})

test_that("x not tabular", {
  expect_error(filter_set(mtcars, x = c(vs = 0)))
  expect_error(filter_set(mtcars, x = list(vs = 0)))
})

test_that("x and data with no columns in common", {
  expect_error(filter_set(mtcars, x = data.frame(foo = "bar")))
})
