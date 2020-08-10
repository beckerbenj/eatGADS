
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


test_that("Recode wrapper with new value labels", {
  expect_error(recodeGADS(dfSAV, varName = "VAR1", oldValues = c(1), newValues = c(10), newValueLabels = c('10' = "la", '11' = "muh")), "The following variables are not in set2: 11")
  expect_error(recodeGADS(dfSAV, varName = "VAR1", oldValues = c(1, 2), newValues = c(10, 11), newValueLabels = c('11' = "muh")), "The following variables are not in set1: 10")
  expect_error(recodeGADS(dfSAV, varName = "VAR1", oldValues = c(1), newValues = c(10), newValueLabels = c('10' = "muh")), "The following variables are not in set1: -99, -96")
  expect_error(recodeGADS(dfSAV, varName = "VAR1", oldValues = c(1), newValues = c(10), newValueLabels = c("muh")), "newValueLabels needs to be named.")

  out <- recodeGADS(dfSAV, varName = "VAR1", oldValues = c(1, -99, -96), newValues = c(1, -9, -9), newValueLabels = c('1' = "new_one", '-9' = "new_miss"))
  expect_equal(nrow(out$labels), 6)
  expect_equal(out$labels$value[1:2], c(1, -9))
  expect_equal(out$labels$valLabel[1:2], c("new_one", "new_miss"))
})
