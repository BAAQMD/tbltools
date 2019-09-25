context("write_csv")

tmpfn <- tempfile()

test_that("return value", {

  expect_identical(
    write_csv(test_csv_data, tmpfn),
    test_csv_data)

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
    data_frame(
      foo = as.character("bar"),
      baz = as.numeric(1:3),
      bap = c(147, NA, signif(1 / 128, 4))))

  csv_lines <- readLines(tmpfn)
  expect_equal(
    csv_lines,
    c('"foo","baz","bap"',
      '"bar",1,147',
      '"bar",2,',
      '"bar",3,0.007812')) # IEEE rules for rounding

})

test_that("Inf signif read back", {

  expect_message(
    write_csv(test_csv_data, tmpfn, verbose = TRUE, signif = Inf),
    "use `signif`")

  read_back <- read_csv(tmpfn)
  expected <- data_frame(
    foo = as.character("bar"),
    baz = as.numeric(1:3),
    bap = c(147, NA, 0.0078125))

  expect_equivalent(
    read_back,
    expected)

  csv_lines <- readLines(tmpfn)
  expect_equal(
    csv_lines,
    c('"foo","baz","bap"',
      '"bar",1,"147"',
      '"bar",2,',
      '"bar",3,"0.0078125"'))

})

test_that("default signif", {

  options(digits = 5)

  write_csv(test_csv_data, verbose = TRUE, tmpfn)
  read_back <- read_csv(tmpfn)
  expected <- data_frame(
    foo = as.character("bar"),
    baz = as.numeric(1:3),
    bap = c(147, NA, signif(1 / 128, getOption("digits"))))
  expect_equal(read_back, expected)

  csv_lines <- readLines(tmpfn)
  expect_equal(
    csv_lines,
    c('"foo","baz","bap"',
      '"bar",1,"147"',
      '"bar",2,',
      '"bar",3,"0.0078125"'))

})
