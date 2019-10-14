context("wildcard_join")

Hg_EFs_generic <-
  structure(list(pol_id = c(1190L, 1190L, 1190L, 1190L, 1190L, 1190L, 1190L),
                 src_code = c("C330X549", "C673X080", "CXXXX498",  "CXXXX754", "G8011791", "G8011791", "GXXXX498"),
                 ef_qty = c(3, 0.00435, 0, 0.0147, 0.0034, 0.0011, 0),
                 ef_date = structure(c(7334, 10848, 14294,  14295, 15848, 11053, 14294), class = "Date"),
                 ef_basis = c("Literature",  "Test (P#)", "Literature", "Literature", "Literature", "Literature",   "Literature"),
                 ef_staff = c("J Tomich", "J Lundquist", "H Lips", "H Lips", "J Lundquist", "J Lundquist", "H Lips"),
                 pol_abbr = c("Hg", "Hg", "Hg", "Hg", "Hg", "Hg", "Hg")),
            .Names = c("pol_id", "src_code", "ef_qty", "ef_date", "ef_basis", "ef_staff", "pol_abbr"),
            class = c("tbl_df",  "tbl", "data.frame"),
            row.names = c(NA, -7L))

test_that("contrived", {

  x <- tibble(foo = "bar", baz = "A1")
  y <- tibble(baz = "AX", city = "Chicago")
  joined <- wildcard_join(x, y, by = "baz", wildcard = "X", keep_pattern = FALSE)
  expect_identical(joined, tibble(foo = "bar", baz = "A1", city = "Chicago"))

})
