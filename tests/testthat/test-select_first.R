test_that("select_first() works", {

  df <- tibble(foo = 1, bar = 2, baz = 3, bap = 4)

  expect_equal(
    names(select_first(df, baz)),
    c("baz", "foo", "bar", "bap"))

  expect_equal(
    names(select_first(df, baz, bar)),
    c("baz", "bar", "foo", "bap"))

  expect_equal(
    names(select_first(df, baz, foo, bap)),
    c("baz", "foo", "bap", "bar"))

  expect_equal(
    names(select_first(df, baz, bar, foo, bap)),
    c("baz", "bar", "foo", "bap"))

})
