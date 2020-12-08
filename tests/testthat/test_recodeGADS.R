
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

test_that("Recode wrapper", {
  out <- recodeGADS(dfSAV, varName = "VAR1", oldValues = c(1), newValues = c(10))
  expect_equal(out$dat$VAR1, c(10, -99, -96, 2))
  allG <- mergeLabels(dfSAV = dfSAV, df2 = df2)
  out2 <- recodeGADS(allG, varName = "VAR1", oldValues = c(1), newValues = c(10))
  expect_equal(out2$datList$dfSAV$VAR1, c(10, -99, -96, 2))
})

test_that("Recode wrapper errors", {
  df <- data.frame(v1 = 1:2, v2 = c("a", "b"), stringsAsFactors = FALSE)
  g <- import_DF(df)
  expect_error(recodeGADS(g, varName = "v3", oldValues = c(1), newValues = c(10)), "'varName' is not a real variable name.")
  expect_error(recodeGADS(g, varName = "v2", oldValues = c(1), newValues = c(10)), "'varName' needs to be a labeled variable in the GADS.")

})

