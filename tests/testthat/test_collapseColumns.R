
# testM <- import_spss("tests/testthat/helper_spss_missings.sav")
testM <- import_spss("helper_spss_missings.sav")

lu1 <- createLookup(testM, recodeVars = c("VAR1", "VAR2"), addCols = c("r1", "r2"))

test_that("collapse columns for two recodeVars",{
  lu1$r1 <- c(1, 2, NA, 4, NA)
  lu1$r2 <- c(NA, -2, 3, 4, 5)

  lu_r <- collapseColumns(lu1, recodeVars = c("r1", "r2"), prioritize = "r2")
  expect_equal(lu_r$value_new, c(1, -2, 3, 4, 5))
})

test_that("collapse columns for two recodeVars and specific names",{
  lu1$r1 <- c(1, 2, NA, 4, NA)
  lu1$r2 <- c(NA, -2, 3, 4, 5)
  names(lu1)[3:4] <- c("value_new", "value_old")

  lu_r <- collapseColumns(lu1, recodeVars = c("value_new", "value_old"), prioritize = "value_old")
  expect_equal(lu_r$value_new, c(1, -2, 3, 4, 5))
})

test_that("collapse columns for one recodeVars",{
  lu1$r1 <- c(1, 2, NA, 4, NA)

  lu_r <- collapseColumns(lu1[1:3], recodeVars = c("r1"), prioritize = "r1")
  expect_equal(names(lu_r), c("variable", "value", "value_new"))
  expect_equal(lu_r$value_new, c(1, 2, NA, 4, NA))
})

test_that("collapse Columns errors",{
  expect_error(collapseColumns(lu1, recodeVars = c("r1", "r2", "value"), prioritize = "r2"),
               "More recode variables than 2 are currently not supported.")
  expect_error(collapseColumns(lu1, recodeVars = c("r1", "r2"), prioritize = c("r1", "r2")),
               "'prioritize' must be of length 1.")
  expect_error(collapseColumns(lu1, recodeVars = c("r1", "r3"), prioritize = c("r1")),
               "All variables names in 'recodeVars' need to be variables in 'lookup'.")
  expect_error(collapseColumns(lu1, recodeVars = c("r1", "r2"), prioritize = c("value")),
               "All variables names in 'prioritize' need to be in 'recodeVars'.")

  lu2 <- lu1
  lu2$r1 <- as.factor(lu2$r1)
  expect_error(collapseColumns(lu2, recodeVars = c("r1", "r2"), prioritize = c("r1")),
               "Variables specified under 'recodeVars' must not be factors.")

})
