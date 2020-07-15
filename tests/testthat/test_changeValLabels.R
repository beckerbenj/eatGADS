
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

test_that("changevallabel wrapper", {
  out <- changeValLabels(dfSAV, varName = "VAR1", value = 1, valLabel = "test label")
  expect_equal(out$labels[3, "valLabel"], "test label")
  expect_equal(out$dat, dfSAV$dat)

  out2 <- changeValLabels(dfSAV, varName = "VAR2", value = c(-99, -96), valLabel = c("label 3", "label 2"))
  expect_equal(out2$labels[c(4, 5), "valLabel"], c("label 2", "label 3"))
  expect_equal(out2$dat, dfSAV$dat)

  expect_error(changeValLabels(dfSAV, varName = c("VAR1", "VAR2"), value = 1, valLabel = "test label"))
  expect_error(changeValLabels(dfSAV, varName = c("VAR4"), value = 1, valLabel = "test label"))
})
