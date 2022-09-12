
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

test_that("check_single_varName", {
  expect_error(check_single_varName(var = c("VAR1", "VAR1")),
               "'varName' must be of length 1.")
  expect_error(check_single_varName(var = 1),
               "'varName' is not a character vector.")
  expect_error(check_single_varName(var = 1, argumentName = "someArgument"),
               "'someArgument' is not a character vector.")
})


test_that("check_vars_in_GADSdat", {
  expect_error(check_vars_in_GADSdat(dfSAV, vars = c("VAR1", "VAR1")),
               "There are duplicates in 'vars': VAR1")
  expect_error(check_vars_in_GADSdat(dfSAV, vars = c("VAR4")),
               "The following 'vars' are not variables in the GADSdat: VAR4")
  expect_error(check_vars_in_GADSdat(dfSAV, vars = c("VAR4", "VAR6")),
               "The following 'vars' are not variables in the GADSdat: VAR4, VAR6")
})