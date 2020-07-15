
# rawDat <- import_spss(file = "tests/testthat/helper_spss.sav")
rawDat <- import_spss("helper_spss.sav")
# testM <- import_spss("tests/testthat/helper_spss_missings.sav")
testM <- import_spss("helper_spss_missings.sav")

lu1 <- createLookup(testM, recodeVars = c("VAR1", "VAR2"), addCols = c("r1", "r2"))
lu2 <- createLookup(testM, recodeVars = c("VAR1"), sort_by = "value")
lu3 <- createLookup(testM, recodeVars = c("VAR1", "VAR2"), sort_by = "value")


test_that("Create lookup for 1 character variables",{
  test <- createLookup(rawDat, recodeVars = c("VAR1"))
  expect_equal(test$variable, c("VAR1"))
  expect_equal(dim(test), c(1, 3))
})

test_that("Create lookup for 2 numeric variables",{
  test <- createLookup(rawDat, recodeVars = c("VAR1", "VAR2"), addCols = c("Rater_1", "Rater_2"))
  expect_equal(test$variable, c("VAR1", "VAR2"))
  expect_equal(names(test), c("variable", "value", "Rater_1", "Rater_2"))
})

test_that("Create lookup for 2 mixed variables",{
  test <- createLookup(rawDat, recodeVars = c("VAR1", "VAR3"))
  expect_equal(test$variable, c("VAR1", "VAR3"))
  expect_equal(test$value, c(1, "a"))
  expect_equal(test$value_new, c(NA, NA))
  expect_equal(dim(test), c(2, 3))
})


test_that("Test unique values functionality for Create lookups",{
  expect_equal(lu1$value, c(1, -99, -96, 2, 1))
  #expect_equal(test$value, c(1, 3))
  #expect_equal(test$newValue, c(NA, NA))
  #expect_equal(dim(test), c(2, 3))
})

test_that("Ordering by values",{
  expect_equal(lu2$value, c(-99, -96, 1, 2))

  expect_equal(lu3$value, c(-99, -96, 1, 1, 2))

  test2 <- createLookup(testM, recodeVars = c("VAR1", "VAR2"), sort_by = c("variable", "value"))
  expect_equal(test2$value, c(-99, -96, 1, 2, 1))
})
