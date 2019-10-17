context("pull_distinct")

require(tidyverse)

test_that("mtcars", {

  mtcars %>%
    pull_distinct(cyl) %>%
    expect_setequal(
      c(4, 6, 8))

})
