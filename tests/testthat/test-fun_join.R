context("fun_join")

df1 <- tibble(
  thing = c("mug", "plate", "plate", "donut", "donut"),
  owner = c("Alice", "Bob", "Chuck", "Alice", "Bob"))

df2 <- tibble(
  thing = c("plate", "mug", "mug", "donut", "mug"),
  owner = c(NA, "Bob", "Alice", NA, "Chuck"),
  color = c("white", "red", "white", "brown", "white"))

with(filter(df2, thing == "plate"),
     expect_true((is.na(owner) & (color == "white"))))

with(filter(df2, thing == "mug", owner == "Bob"),
     expect_true(color == "red"))

f <- function (e1, e2) (e1 == e2) | is.na(e2)

df3 <- fun_join(df1, df2, fun = f)

# There is nothing in df3 (the result) that does not correspond to a row in df1
expect_empty <- function (input_data) expect_true(nrow(input_data) == 0)
disjunction <- function (left, right) anti_join(left, right, by = intersect(names(left), names(right)))
expect_superset <- function (left, right) expect_empty(disjunction(left, right))
expect_superset(df1, df3)

with(filter(df3, thing == "donut"), expect_true(setequal(owner, c("Alice", "Bob"))))

with(filter(df3, thing == "donut"), expect_true(all(color == "brown")))
with(filter(df3, thing == "plate"), expect_true(all(color == "white")))
