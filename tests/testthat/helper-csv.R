test_csv_file <-
  here::here("tests", "testthat", "random.csv")

set.seed(0)

test_csv_data <-
  data_frame(foo = "bar", baz = as.integer(1:3), bap = c(147, NA, 1 / 128))

# Use only the base::write.csv() function
# (not write_csv() from this package or from the readr package)
write.csv(
  test_csv_data,
  test_csv_file,
  row.names = FALSE)

