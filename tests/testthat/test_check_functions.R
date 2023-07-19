
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

  expect_error(check_vars_in_GADSdat("helper_dataBase.db", vars = c("VAR4", "VAR6"), GADSdatName = "GADSdat1"),
               "The following 'vars' are not variables in the GADSdat1: VAR4, VAR6")
})

test_that("check_vars_in_vec", {
  expect_error(check_vars_in_vector(namesGADS(dfSAV), vars = c("VAR1", "VAR1"), vec_nam = "GADSdat"),
               "There are duplicates in 'vars': VAR1")
  expect_error(check_vars_in_vector(namesGADS(dfSAV), vars = c("VAR4"), vec_nam = "GADSdat"),
               "The following 'vars' are not variables in the GADSdat: VAR4")
  expect_error(check_vars_in_vector(dfSAV, vars = c("VAR4", "VAR6"), vec_nam = "GADSdats"),
               "The following 'vars' are not variables in the GADSdats: VAR4, VAR6")
})

test_that("check_logicalArgument", {
  expect_error(check_logicalArgument(1, "test"),
               "'test' needs to be a logical vector of length 1.")
  expect_error(check_logicalArgument(c(TRUE, FALSE), "test"),
               "'test' needs to be a logical vector of length 1.")
  expect_silent(check_logicalArgument(TRUE, "test"))

})

test_that("check_characterArgument", {
  expect_error(check_characterArgument(1, "test"),
               "'test' needs to be a character vector of length 1.")
  expect_error(check_characterArgument(c("a", "b"), "test"),
               "'test' needs to be a character vector of length 1.")
  expect_silent(check_characterArgument("a", "test"))
})

test_that("check_numericArgument", {
  expect_error(check_numericArgument("1", "test"),
               "'test' needs to be a numeric vector of length 1.")
  expect_error(check_numericArgument(1:2, "test"),
               "'test' needs to be a numeric vector of length 1.")
  expect_silent(check_numericArgument(1, "test"))
})
