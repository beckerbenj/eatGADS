
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

dfUn <- import_DF(data.frame(v1 = 1, v2 = 2))

test_that("Errors", {
  expect_error(removeEmptyValLabels(dfSAV, vars = "VAR1", whichValLabels = "other"),
               "'arg' should be one of “miss”, “valid”, “all”")
})

test_that("removeEmptyValLabels for value labels", {
  dfSAV2 <- changeValLabels(dfSAV, varName = "VAR2", value = c(-99, 5), valLabel = c("someMiss", "test"))
  dfSAV2 <- changeValLabels(dfSAV2, varName = "VAR3", value = c(-98, 5), valLabel = c("someMiss", "test"))
  dfSAV2 <- changeMissings(dfSAV2, varName = "VAR2", value = -96, missings = "miss")

  out <- removeEmptyValLabels(dfSAV2, vars = namesGADS(dfSAV))
  expect_equal(out$labels$value, c(-99, -96, 1, 5, -98, 5))

  out2 <- removeEmptyValLabels(dfSAV2, vars = namesGADS(dfSAV), whichValLabels = "valid")
  expect_equal(out2$labels$value, c(-99, -96, 1, -99, -96, -99, -98))

  out3 <- removeEmptyValLabels(dfSAV2, vars = namesGADS(dfSAV), whichValLabels = "all")
  expect_equal(out3$labels$value, c(-99, -96, 1, NA, -98))
})

test_that("removeEmptyValLabels also for missing tags", {
  out <- removeEmptyValLabels(dfSAV, vars = namesGADS(dfSAV), whichValLabels = "all")
  expect_equal(out$labels$value, c(-99, -96, 1, NA, -98))
})
