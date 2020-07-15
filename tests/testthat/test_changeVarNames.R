

# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

#### Wrapper for Variable Names
test_that("Errors are called for changeVarNames", {
  expect_error(changeVarNames(dfSAV, oldNames = c("VA2"), newNames = c("test")), "varName in oldNames is not a real variable name.")
  expect_error(changeVarNames(dfSAV, oldNames = c("VAR2"), newNames = c("test", "2")), "oldNames and newNames are not of identical length.")
  expect_error(changeVarNames(dfSAV, oldNames = c("VAR2"), newNames = 1), "oldNames and newNames are not character vectors.")
})

test_that("changeVarNames for GADSdat", {
  out_single <- changeVarNames(dfSAV, oldNames = c("VAR2"), newNames = c("test"))
  expect_equal(names(out_single$dat), unique(out_single$labels$varName))
  expect_equal(names(out_single$dat), c("VAR1", "test", "VAR3"))
  out_double <- changeVarNames(dfSAV, oldNames = c("VAR1", "VAR3"), newNames = c("test", "test2"))
  expect_equal(names(out_double$dat), unique(out_double$labels$varName))
  expect_equal(names(out_double$dat), c("test", "VAR2", "test2"))
})

test_that("changeVarNames for all_GADSdat", {
  out_single <- changeVarNames(expected_bigList, oldNames = c("V1"), newNames = c("var1"))
  expect_equal(names(out_single$datList$df1), c("ID1", "var1"))
  expect_equal(out_single$allLabels$varName, c("ID1", "var1", "ID1", "V2"))

  out_multiple <- changeVarNames(expected_bigList, oldNames = c("V1", "V2"), newNames = c("var1", "var2"))
  expect_equal(names(out_multiple$datList$df1), c("ID1", "var1"))
  expect_equal(names(out_multiple$datList$df2), c("ID1", "var2"))
  expect_equal(out_multiple$allLabels$varName, c("ID1", "var1", "ID1", "var2"))

  out_double <- changeVarNames(expected_bigList, oldNames = c("ID1"), newNames = c("idstud"))
  expect_equal(names(out_double$datList$df1), c("idstud", "V1"))
  expect_equal(names(out_double$datList$df2), c("idstud", "V2"))
  expect_equal(out_double$allLabels$varName, c("idstud", "V1", "idstud", "V2"))

})
