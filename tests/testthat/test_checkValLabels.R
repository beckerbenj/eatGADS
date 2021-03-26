
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

df3 <- df2
df3$dat[1, 1:2] <- 8

test_that("Input validation", {
  expect_error(checkEmptyValLabels(df1, vars = 3:4),
               "The following 'vars' are not variables in the GADSdat: 3, 4")
  expect_error(checkEmptyValLabels(df1, valueRange = 3:5),
               "'valueRange' needs to be a numeric vector of length 2.")
  expect_error(checkEmptyValLabels(df1, valueRange = letters[3:4]),
               "'valueRange' needs to be a numeric vector of length 2.")
  expect_error(checkEmptyValLabels(df1, output = "List"))
  expect_error(checkMissingValLabels(df1, vars = 3:4),
               "The following 'vars' are not variables in the GADSdat: 3, 4")
  expect_error(checkMissingValLabels(df1, valueRange = 3:5),
               "'valueRange' needs to be a numeric vector of length 2.")
  expect_error(checkMissingValLabels(df1, valueRange = letters[3:4]),
               "'valueRange' needs to be a numeric vector of length 2.")
})

test_that("checkEmptyValLabels", {
  out <- checkEmptyValLabels(dfSAV)
  expect_equal(names(out), paste0("VAR", 1:3))
  expect_equal(out[[1]], NULL)
  expect_equal(names(out[[2]]), c("value", "valLabel", "missings"))
  expect_equal(out[[2]]$value, c(-99, -96))
  expect_equal(out[[2]]$valLabel, c(NA, "missing"))
  expect_equal(out[[2]]$missings, c("miss", "valid"))
  expect_equal(out[[3]]$value, c(-99))
})

test_that("checkEmptyValLabels data.frame", {
  out <- checkEmptyValLabels(dfSAV, output = "data.frame")
  expect_equal(names(out), c("variable", "value", "valLabel", "missings"))
  expect_equal(nrow(out), 3)
  expect_equal(out$value, c(-99, -96, -99))
  expect_equal(out$valLabel, c(NA, "missing", "missing"))
  expect_equal(out$missings, c("miss", "valid", "miss"))
  expect_equal(out$variable, c("VAR2", "VAR2", "VAR3"))
})

test_that("checkMissingValLabels", {
  out <- checkMissingValLabels(dfSAV)
  expect_equal(names(out), paste0("VAR", 1:3))
  expect_equal(names(out[[1]]), c("varLabel", "missing_labels"))
  expect_equal(names(out[[2]]), c("varLabel", "missing_labels"))
  expect_equal(out[[1]]$varLabel, "Variable 1")
  expect_equal(out[[3]]$varLabel, "Variable 3")
  expect_equal(out[[1]]$missing_labels, 2)
  expect_equal(out[[2]]$missing_labels, 1)

  dfSAV2 <- removeValLabels(dfSAV, "VAR1", value = c(-99, -96, 1))
  out2 <- checkMissingValLabels(dfSAV2)
  expect_equal(out2[[1]]$missing_labels, c(-99, -96, 1, 2))
})


test_that("With NAs", {
  suppressMessages(dfSAV2 <- recode2NA(dfSAV, value = 1))
  out <- checkEmptyValLabels(dfSAV2)
  expect_equal(out[[2]], checkEmptyValLabels(dfSAV)[[2]])
  expect_equal(out[[3]], checkEmptyValLabels(dfSAV)[[3]])
  expect_equal(out[[1]]$value, 1)

  out <- checkMissingValLabels(dfSAV2)
  expect_equal(out[[1]], checkMissingValLabels(dfSAV)[[1]])
  expect_equal(out[[2]], NULL)
  expect_equal(out[[3]], NULL)
})


test_that("with specific value range", {
  out <- checkEmptyValLabels(dfSAV, valueRange = c(-97, -100))
  expect_equal(out[[1]], NULL)
  expect_equal(nrow(out[[2]]), 1)
  expect_equal(nrow(out[[3]]), 1)

  out2 <- checkMissingValLabels(dfSAV, valueRange = c(3, -100))
  expect_equal(out2, checkMissingValLabels(dfSAV))

  out3 <- checkMissingValLabels(dfSAV, valueRange = c(1, -100))
  expect_equal(out3[[2]], out2[[2]])
  expect_equal(out3[[1]], NULL)
})
