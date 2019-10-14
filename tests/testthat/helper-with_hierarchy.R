test_hierarchy <-
  tibble(
    cat_id = c(2199L, 1657L, 1597L),
    cat_h0 = c("Area sources", "Area sources", "Area sources"),
    cat_h1 = c(
      "COMBUSTION - MOBILE SOURCES",
      "COMBUSTION - MOBILE SOURCES",
      "COMBUSTION - STATIONARY SOURCES"
    ),
    cat_h2 = c(
      "OFF-HIGHWAY MOBILE SOURCES",
      "OFF-HIGHWAY MOBILE SOURCES",
      "FUELS COMBUSTION"
    ),
    cat_h3 = c(
      " Ships in Transit",
      "Construction & Mining Equipment",
      "Power Plants"
    ),
    cat_h4 = c(" Tanker - Aux. Engine", "Gasoline", "Power Imports"),
    cat_h5 = c(NA, "4 Stroke", NA),
    cat_h6 = c(NA_character_, NA_character_, NA_character_)
  )
