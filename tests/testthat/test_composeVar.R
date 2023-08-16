
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

dfSAV2 <- reuseMeta(dfSAV, varName = "VAR2", other_GADSdat = dfSAV, other_varName = "VAR1")
dfSAV2$dat[2, "VAR2"] <- -96


test_that("Errors", {
  expect_error(composeVar(df1, sourceVars = "ID1", newVar = "newVar", primarySourceVar = "ID1"),
               "'sourceVars' must be a character vector of length 2.")
  expect_error(composeVar(dfSAV, sourceVars = c("VAR1", "VAR2"), newVar = "newVar", primarySourceVar = "VAR1"),
               "Meta data on value level ('value', 'valLabel', 'missings') of 'sourceVars' must be identical.", fixed = TRUE)
})


test_that("compose a variable", {
  out <- composeVar(dfSAV2, sourceVars = c("VAR1", "VAR2"), newVar = "newVar", primarySourceVar = "VAR1")

  expect_equal(out$dat$newVar, c(1, -99, 1, 2))
  expect_equal(out$labels[1:3, "value"], c(-99, -96, 1))
})

test_that("composeVar with invalid varName", {
  expect_message(out <- composeVar(dfSAV2, sourceVars = c("VAR1", "VAR2"), newVar = "Alter", primarySourceVar = "VAR1"),
                 "Alter has been renamed to AlterVar")
  expect_equal(out$dat$AlterVar, c(1, -99, 1, 2))

  out2 <- composeVar(dfSAV2, sourceVars = c("VAR1", "VAR2"), newVar = "Alter", primarySourceVar = "VAR1", checkVarName = FALSE)
  expect_equal(out2$dat$Alter, c(1, -99, 1, 2))
})
