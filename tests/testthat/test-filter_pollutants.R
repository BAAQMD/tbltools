context("filter_pollutants")

test_data <-
  tribble(
    ~ pol_id, ~ pol_abbr,
     990L, "TOG",
    1990L, "PM",
    2990L, "NOx",
    1350L, NA_character_,
      41L, NA_character_,
     293L, NA_character_,
     333L, NA_character_,
     307L, NA_character_)

BTEX_POLLUTANTS <-
  list(
    "Benzene" = 41,
    "Toluene" = 293,
    "Ethylbenzene" = 333,
    "Xylene" = 307)

test_that("unnamed pollutant ids", {

  expected <-
    test_data %>%
    filter(
      pol_id %in% c(990, 1990, 2990))

  test_data %>%
    filter_pollutants(
      c(990, 1990, 2990)) %>%
    expect_equal(
      expected)

  test_data %>%
    filter_pollutants(
      990, 1990, 2990) %>%
    expect_equal(
      expected)

  test_data %>%
    filter_pollutants(
      c(990, 1990), 2990) %>%
    expect_equal(
      expected)

})

test_that("pollutant abbreviations", {

  expected <-
    test_data %>%
    filter(
      pol_abbr %in% c("TOG", "PM", "NOx")) %>%
    mutate(
      pol_abbr = factor(pol_abbr, levels = c("TOG", "PM", "NOx")))

  test_data %>%
    filter_pollutants(
      c("TOG", "PM", "NOx")) %>%
    expect_equal(
      expected)

  test_data %>%
    filter_pollutants(
      "TOG", "PM", "NOx") %>%
    expect_equal(
      expected)

  test_data %>%
    filter_pollutants(
      c("TOG", "PM"), "NOx") %>%
    expect_equal(
      expected)

})

test_that("named pollutant ids (TOG, PM, NOx)", {

  expected <-
    test_data %>%
    filter(
      pol_abbr %in% c("TOG", "PM", "NOx")) %>%
    mutate(
      pol_abbr = factor(pol_abbr, levels = c("TOG", "PM", "NOx")))

  test_data %>%
    filter_pollutants(
      c(TOG = 990, PM = 1990, NOx = 2990)) %>%
    expect_equal(
      expected)

  test_data %>%
    filter_pollutants(
      c(TOG = 990, PM = 1990), NOx = 2990) %>%
    expect_equal(
      expected)

  test_data %>%
    filter_pollutants(
      TOG = 990, PM = 1990, NOx = 2990) %>%
    expect_equal(
      expected)

})

test_that("named pollutant ids (BTEX)", {

  expected <-
    test_data %>%
    filter(
      pol_id %in% BTEX_POLLUTANTS) %>%
    mutate(
      pol_abbr = pol_id %>%
        codec::decode(BTEX_POLLUTANTS) %>%
        factor(., levels = names(BTEX_POLLUTANTS)))

  test_data %>%
    filter_pollutants(
      BTEX_POLLUTANTS) %>%
    mutate(
      pol_abbr = pol_id %>%
        codec::decode(BTEX_POLLUTANTS) %>%
        factor(., levels = names(BTEX_POLLUTANTS))) %>%
    expect_equal(
      expected)

  test_data %>%
    filter_pollutants(
      "Benzene" = 41, "Toluene" = 293, "Ethylbenzene" = 333, "Xylene" = 307) %>%
    expect_equal(
      expected)

  test_data %>%
    filter_pollutants(
      c("Benzene" = 41, "Toluene" = 293), "Ethylbenzene" = 333, "Xylene" = 307) %>%
    expect_equal(
      expected)

})
