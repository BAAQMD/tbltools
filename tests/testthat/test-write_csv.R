context("write_csv")

tmpfn <- tempfile()

test_that("return value", {

  expect_identical(
    write_csv(test_csv_data, tmpfn),
    test_csv_data)

})

test_that("date formatted as YYYY-mm-dd", {

  second_line <-
    readr::read_lines(tmpfn) %>%
    .[2]

  expect_match(
    second_line,
    "2019-10-09")

})

test_that("verbose", {

  expect_message(
    write_csv(test_csv_data, tmpfn, verbose = TRUE),
    "stripping non-printable characters")

  expect_message(
    write_csv(test_csv_data, tmpfn, verbose = TRUE),
    "wrote 3 records")

})

test_that("verbose signif", {

  expect_message(
    write_csv(test_csv_data, tmpfn, signif = 2, verbose = TRUE),
    "rounding to 2")

})

test_that("finite signif read back", {

  expect_message(
    write_csv(test_csv_data, tmpfn, verbose = TRUE, signif = 4),
    "rounding to 4")

  read_back <- read_csv(tmpfn)

  expect_equal(
    read_back,
    tibble(
      foo = as.character("bar"),
      bar = test_csv_data[["bar"]],
      baz = as.numeric(1:3),
      bap = c(147, NA, signif(1 / 128, 4))))

  csv_lines <- readLines(tmpfn)
  expect_equal(
    csv_lines,
    c('"foo","bar","baz","bap"',
      '"bar","2019-10-09",1,147',
      '"bar","2019-10-09",2,',
      '"bar","2019-10-09",3,0.007812')) # IEEE rules for rounding

})

test_that("Inf signif read back", {

  expect_message(
    write_csv(test_csv_data, tmpfn, verbose = TRUE, signif = Inf),
    "use `signif`")

  csv_lines <- readLines(tmpfn)
  expect_equal(
    csv_lines,
    c('"foo","bar","baz","bap"',
      '"bar","2019-10-09","1","147"',
      '"bar","2019-10-09","2",',
      '"bar","2019-10-09","3","0.0078125"'))

  read_back <- read_csv(tmpfn)
  expected <- tibble(
    foo = as.character("bar"),
    bar = test_csv_data[["bar"]],
    baz = as.numeric(1:3),
    bap = c(147, NA, 0.0078125))

  expect_equivalent(
    read_back,
    expected)

})

test_that("default signif", {

  options(digits = 5)

  write_csv(test_csv_data, tmpfn, verbose = TRUE)

  csv_lines <- readLines(tmpfn)
  expect_equal(
    csv_lines,
    c('"foo","bar","baz","bap"',
      '"bar","2019-10-09","1","147"',
      '"bar","2019-10-09","2",',
      '"bar","2019-10-09","3","0.0078125"'))

  read_back <- read_csv(tmpfn)
  expected <- tibble(
    foo = as.character("bar"),
    bar = test_csv_data[["bar"]],
    baz = as.numeric(1:3),
    bap = c(147, NA, signif(1 / 128, getOption("digits"))))
  expect_equal(read_back, expected)

})
