require(unittools)

test_that("units are restored", {

  test_data <-
    mutate(
      mtcars,
      mpg = set_units(mpg, "mi/gal"),
      disp = set_units(disp, "ft^3"))

  summed_data <-
    sum_across(test_data, mpg, disp)

  expect_equal(
    nrow(summed_data),
    1)

  with(summed_data, {

    expect_equal(round(mpg),  set_units(643,  "mi/gal"))
    expect_equal(round(disp), set_units(7383, "ft^3"))

  })

})
