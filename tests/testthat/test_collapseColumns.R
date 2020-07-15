
# testM <- import_spss("tests/testthat/helper_spss_missings.sav")
testM <- import_spss("helper_spss_missings.sav")

lu1 <- createLookup(testM, recodeVars = c("VAR1", "VAR2"), addCols = c("r1", "r2"))

test_that("Test unique values functionality for Create lookups",{
  lu1$r1 <- c(1, 2, NA, 4, NA)
  lu1$r2 <- c(NA, -2, 3, 4, 5)

  lu_r <- collapseColumns(lu1, recodeVars = c("r1", "r2"), prioritize = "r2")
  expect_equal(lu_r$value_new, c(1, -2, 3, 4, 5))
})
test_that("Test unique values functionality for Create lookups",{
  lu1$r1 <- c(1, 2, NA, 4, NA)

  lu_r <- collapseColumns(lu1[1:3], recodeVars = c("r1"), prioritize = "r1")
  expect_equal(names(lu_r), c("variable", "value", "value_new"))
  expect_equal(lu_r$value_new, c(1, 2, NA, 4, NA))
})
