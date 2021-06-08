test_that("select_last() works", {

  df <- tibble(foo = 1, bar = 2, baz = 3, bap = 4)

  expect_equal(
    names(select_last(df, foo)),
    c("bar", "baz", "bap", "foo"))

  expect_equal(
    names(select_last(df, foo, bar)),
    c("baz", "bap", "foo", "bar"))

  expect_equal(
    names(select_last(df, baz, foo, bar)),
    c("bap", "baz", "foo", "bar"))

  expect_equal(
    names(select_last(df, baz, bar, foo, bap)),
    c("baz", "bar", "foo", "bap"))

})
