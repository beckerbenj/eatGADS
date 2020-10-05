
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

test_that("check_vars_in_GADSdat", {
  expect_error(check_vars_in_GADSdat(dfSAV, vars = c("VAR1", "VAR1")),
               "There are duplicates in 'vars': VAR1")
  expect_error(check_vars_in_GADSdat(dfSAV, vars = c("VAR4")),
               "The following 'vars' are not variables in the GADSdat: VAR4")
  expect_error(check_vars_in_GADSdat(dfSAV, vars = c("VAR4", "VAR6")),
               "The following 'vars' are not variables in the GADSdat: VAR4, VAR6")
})

test_that("Removing variables from GADSdat", {
  expect_error(removeVars(dfSAV, c("VAR4")),
               "The following 'vars' are not variables in the GADSdat: VAR4")
  test <- removeVars(dfSAV, c("VAR1"))
  expect_equal(namesGADS(test), c("VAR2", "VAR3"))
  test2 <- removeVars(dfSAV, c("VAR1", "VAR3"))
  expect_equal(namesGADS(test2), c("VAR2"))
})

test_that("Extracting variables from GADSdat", {
  expect_error(extractVars(dfSAV, c("VAR4")),
               "The following 'vars' are not variables in the GADSdat: VAR4")
  test <- extractVars(dfSAV, c("VAR1"))
  expect_equal(namesGADS(test), c("VAR1"))
  test2 <- extractVars(dfSAV, c("VAR1", "VAR3"))
  expect_equal(namesGADS(test2), c("VAR1", "VAR3"))
})
