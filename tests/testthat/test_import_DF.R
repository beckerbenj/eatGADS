###### test import from R data frame
test_that("Data frames directly from R are imported correctly", {
  iris2 <- import_DF(iris)
  expect_equal(dim(iris2$labels), c(7, 8))
  expect_equal(iris2$labels$valLabel,
               c(NA, NA, NA, NA, "setosa", "versicolor", "virginica"))
  expect_equal(iris2$labels$value,
               c(NA, NA, NA, NA, 1, 2, 3))
})

test_that("import_DF with factors with zero levels", {
  df <- data.frame(v1 = c(1, 2), v2 = factor(c(NA, NA)))
  expect_error(gads <- import_DF(df), "The following variables in the data are factors with zero valid levels: v2")

  df2 <- data.frame(v1 = c(1, 2), v2 = factor(c(NA, NA)), v3 = factor(c(NA, "a")), v4 = factor(c(NA, NA)))
  expect_error(gads <- import_DF(df2), "The following variables in the data are factors with zero valid levels: v2, v4")
})
