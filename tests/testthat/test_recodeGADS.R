
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


test_that("Recode wrapper for unlabeled values", {
  dfSAV2 <- dfSAV
  dfSAV2$dat[2, 2] <- 2

  out <- recodeGADS(dfSAV, varName = "VAR1", oldValues = c(2), newValues = c(10))
  expect_equal(out$dat$VAR1, c(1, -99, -96, 10))
  allG <- mergeLabels(dfSAV = dfSAV, df2 = df2)
  out2 <- recodeGADS(allG, varName = "VAR1", oldValues = c(2), newValues = c(10))
  expect_equal(out2$datList$dfSAV$VAR1, c(1, -99, -96, 10))
})

test_that("Recode wrapper for unlabeled variables", {
  out <- recodeGADS(df1, varName = "V1", oldValues = c(3, 5), newValues = c(30, 50))
  expect_equal(out$dat$V1, c(30, 50))
})

test_that("Recode wrapper errors", {
  df <- data.frame(v1 = 1:2, v2 = c("a", "b"), stringsAsFactors = FALSE)
  g <- import_DF(df)
  expect_error(recodeGADS(g, varName = "v3", oldValues = c(1), newValues = c(10)),
               "'varName' is not a real variable name.")
  #expect_error(recodeGADS(g, varName = "v2", oldValues = c(1), newValues = c(10)),
  #             "'varName' needs to be a labeled variable in the GADS.")

  expect_error(recodeGADS(dfSAV, varName = "VAR1", oldValues = c(1), newValues = c(NA)),
               "Missing value(s) in 'newValues'. Recode to NA using recodeString2NA() if required.", fixed = TRUE)

  expect_error(recodeGADS(dfSAV, varName = "VAR1", oldValues = c(-99), newValues = c(1)),
               "Values in 'value_new' with existing meta data in variable VAR1: 1")
  expect_warning(out <- recodeGADS(dfSAV, varName = "VAR1", oldValues = c(3), newValues = c(10)),
               "The following value in 'oldValues' is neither a labeled value in the meta data nor an actual value in VAR1: 3")
  expect_equal(out, dfSAV)
})


test_that("Recode wrapper with NA in oldValues", {
  dfSAV2 <- dfSAV
  dfSAV2$dat[1, 1] <- NA
  out <- recodeGADS(dfSAV2, varName = "VAR1", oldValues = c(NA), newValues = c(10))
  expect_equal(out$dat$VAR1, c(10, -99, -96, 2))

  out2 <- recodeGADS(dfSAV2, varName = "VAR1", oldValues = c(NA, -96), newValues = c(10, 20))
  expect_equal(out2$dat$VAR1, c(10, -99, 20, 2))
})
