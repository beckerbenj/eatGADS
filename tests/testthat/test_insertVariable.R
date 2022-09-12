
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")


test_that("compare_variables", {
  out <- insertVariable(dfSAV, var = "VAR3", varBefore = "VAR1")
  expect_equal(namesGADS(out), c("VAR1", "VAR3", "VAR2"))

  out <- insertVariable(dfSAV, var = "VAR2", varBefore = "VAR3")
  expect_equal(namesGADS(out), c("VAR1", "VAR3", "VAR2"))

  out <- insertVariable(dfSAV, var = "VAR1", varBefore = "VAR3")
  expect_equal(namesGADS(out), c("VAR2", "VAR3", "VAR1"))
})

