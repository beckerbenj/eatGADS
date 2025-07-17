
load(file = test_path("helper_data.rda"))
dfSAV <- import_spss(file = test_path("helper_spss_missings.sav"))

allG <- mergeLabels(dfSAV = dfSAV, df2 = df2)

test_that("Recode wrapper errors", {
  df <- data.frame(v1 = 1:2, v2 = c("a", "b"), stringsAsFactors = FALSE)
  g <- import_DF(df)
  expect_error(recodeGADS(g, varName = "v3", oldValues = c(1), newValues = c(10)),
               "The following 'varName' are not variables in the GADSdat: v3")
  expect_error(recodeGADS(allG, varName = c("VAR4", "VAR5"), oldValues = c(1), newValues = c(10)),
               "The following 'varName' are not variables in the GADSdat: VAR4, VAR5")

  expect_error(recodeGADS(dfSAV, varName = "VAR1", oldValues = c(1), newValues = c(NA)),
               "Missing value(s) in 'newValues'. Recode to NA using recodeString2NA() if required.", fixed = TRUE)
  expect_error(recodeGADS(dfSAV, varName = "VAR1", oldValues = c(-99), newValues = c(1)),
               "Values in 'value_new' with existing meta data in variable VAR1: 1")
  expect_error(recodeGADS(dfSAV, "VAR1", oldValues = c(1, 2, 1, 2), newValues = c(6, 6, 5, 5)),
               "The are duplicate values in 'oldValues': 1, 2")
  expect_warning(out <- recodeGADS(dfSAV, varName = "VAR1", oldValues = c(3), newValues = c(10)),
                 "The following values in 'oldValues' are neither a labeled value in the meta data nor an actual value in VAR1: 3")
  expect_equal(out, dfSAV)
})


test_that("Recode wrapper", {
  out <- recodeGADS(dfSAV, varName = "VAR1", oldValues = c(1), newValues = c(10))
  expect_equal(out$dat$VAR1, c(10, -99, -96, 2))

  out2 <- recodeGADS(allG, varName = "VAR1", oldValues = c(1), newValues = c(10))
  expect_equal(out2$datList$dfSAV$VAR1, c(10, -99, -96, 2))
})


test_that("Recode wrapper for unlabeled values", {
  dfSAV2 <- dfSAV
  dfSAV2$dat[2, 2] <- 2

  out <- recodeGADS(dfSAV, varName = "VAR1", oldValues = c(2), newValues = c(10))
  expect_equal(out$dat$VAR1, c(1, -99, -96, 10))

  out2 <- recodeGADS(allG, varName = "VAR1", oldValues = c(2), newValues = c(10))
  expect_equal(out2$datList$dfSAV$VAR1, c(1, -99, -96, 10))
})

test_that("Recode wrapper for unlabeled variables", {
  out <- recodeGADS(df1, varName = "V1", oldValues = c(3, 5), newValues = c(30, 50))
  expect_equal(out$dat$V1, c(30, 50))
})

test_that("Recode wrapper for unlabeled variables avoids sequential recoding bug", {
  gads <- import_DF(data.frame(x = 1:6, y = 1:6))
  out <- recodeGADS(GADSdat = gads, var = "x", oldValues = 3:5,
                    newValues = c(5, 14, 15))

  expect_equal(out$dat$x, c(1, 2, 5, 14, 15, 6))
})

test_that("Recode wrapper for unlabeled variables including NAs avoids sequential recoding bug", {
  df <- data.frame(x = c(NA, 1:5))
  gads <- import_DF(df)
  gads_re <- recodeGADS(gads, "x",
                        oldValues = c(NA, 2, 3),
                        newValues = c(3, 12, 13))

  expect_equal(gads_re$dat$x, c(3, 1, 12, 13, 4, 5))
})

test_that("Recode wrapper with NA in oldValues", {
  dfSAV2 <- dfSAV
  dfSAV2$dat[1, 1] <- NA
  out <- recodeGADS(dfSAV2, varName = "VAR1", oldValues = c(NA), newValues = c(10))
  expect_equal(out$dat$VAR1, c(10, -99, -96, 2))

  out2 <- recodeGADS(dfSAV2, varName = "VAR1", oldValues = c(NA, -96), newValues = c(10, 20))
  expect_equal(out2$dat$VAR1, c(10, -99, 20, 2))
})


test_that("Recode wrapper for multiple variables at once", {
  out <- recodeGADS(dfSAV, varName = c("VAR1", "VAR2"), oldValues = c(1), newValues = c(10))
  expect_equal(out$dat$VAR1, c(10, -99, -96, 2))
  expect_equal(out$dat$VAR2, c(10, 10, 10, 10))

  out2 <- recodeGADS(allG, varName = c("VAR1", "VAR2", "ID1"), oldValues = c(1), newValues = c(10))
  expect_equal(out2$datList$dfSAV$VAR1, c(10, -99, -96, 2))
  expect_equal(out2$datList$dfSAV$VAR2, c(10, 10, 10, 10))
  expect_equal(out2$datList$df2$ID1, c(10, 10))
})
