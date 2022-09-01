
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

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
