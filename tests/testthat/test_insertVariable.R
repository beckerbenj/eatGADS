
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")


test_that("insert variable in different order", {
  out <- insertVariable(dfSAV, var = "VAR3", after = "VAR1")
  expect_equal(namesGADS(out), c("VAR1", "VAR3", "VAR2"))

  out <- insertVariable(dfSAV, var = "VAR2", after = "VAR3")
  expect_equal(namesGADS(out), c("VAR1", "VAR3", "VAR2"))

  out <- insertVariable(dfSAV, var = "VAR1", after = "VAR3")
  expect_equal(namesGADS(out), c("VAR2", "VAR3", "VAR1"))

  out <- insertVariable(dfSAV, var = "VAR3", after = NULL)
  expect_equal(namesGADS(out), c("VAR3", "VAR1", "VAR2"))
})

