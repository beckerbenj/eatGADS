###### test import from R data frame
test_that("Data frames directly from R are imported correctly", {
  suppressMessages(iris2 <- import_DF(iris))
  expect_equal(dim(iris2$labels), c(7, 8))
  expect_equal(iris2$labels$valLabel,
               c(NA, NA, NA, NA, "setosa", "versicolor", "virginica"))
  expect_equal(iris2$labels$value,
               c(NA, NA, NA, NA, 1, 2, 3))
  expect_false(is.integer(iris2$labels$value))
  expect_false(is.integer(iris2$dat$Species))
})

test_that("import_DF with factors with zero levels", {
  df <- data.frame(v1 = c(1, 2), v2 = factor(c(NA, NA)))
  warns <- capture_warnings(gads <- import_DF(df))
  expect_equal(warns[1], "The following variables in the data are factors with zero valid levels: v2")
  expect_true(is.numeric(gads$dat$v2))
  expect_equal(gads$dat$v2, c(NA_real_, NA_real_))
  expect_equal(gads$labels[2, "value"], c(NA_real_))
  expect_equal(gads$labels[2, "valLabel"], c(NA_character_))

  df2 <- data.frame(v1 = c(1, 2), v2 = factor(c(NA, NA)), v3 = factor(c(NA, "a")), v4 = factor(c(NA, NA)))
  warns2 <- capture_warnings(gads2 <- import_DF(df2))
  expect_equal(warns2[1], "The following variables in the data are factors with zero valid levels: v2, v4")
  expect_true(is.numeric(gads2$dat$v2))
  expect_true(is.numeric(gads2$dat$v4))
  expect_equal(gads2$dat$v2, c(NA_real_, NA_real_))
  expect_equal(gads2$labels[2, "value"], c(NA_real_))
  expect_equal(gads2$labels[2, "valLabel"], c(NA_character_))
})
