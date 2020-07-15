

# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

test_that("changevarlabel wrapper", {
  out <- changeVarLabels(dfSAV, varName = "VAR1", varLabel = "test label")
  expect_equal(out$labels$varLabel[1:3], rep("test label", 3))
  expect_equal(out$dat, dfSAV$dat)

  out <- changeVarLabels(dfSAV, varName = c("VAR3", "VAR2"), varLabel = c("label 3", "label 2"))
  expect_equal(out$labels$varLabel, c(rep("Variable 1", 3), rep("label 2", 2), rep("label 3", 2)))
  expect_equal(out$dat, dfSAV$dat)
})
