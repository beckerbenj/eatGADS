###### test import from R data frame
test_that("Data frames directly from R are imported correctly", {
  iris2 <- import_DF(iris)
  expect_equal(dim(iris2$labels), c(7, 8))
  expect_equal(iris2$labels$valLabel,
               c(NA, NA, NA, NA, "setosa", "versicolor", "virginica"))
  expect_equal(iris2$labels$value,
               c(NA, NA, NA, NA, 1, 2, 3))
})

