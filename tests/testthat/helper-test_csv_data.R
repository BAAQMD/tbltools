library(stringr)
library(tidyr)

test_csv_file <-
  here::here(
    "tests",
    "testthat",
    "helper-test_csv_data.csv")

set.seed(0)

test_csv_data <-
  tibble(
    foo = "bar",
    bar = as.Date("2019-10-09"),
    baz = as.numeric(1:3),
    bap = c(147, NA, 1 / 128))

# Use only the base::write.csv() function
# (not write_csv() from this package or from the readr package)
write.csv(
  test_csv_data,
  test_csv_file,
  row.names = FALSE)

