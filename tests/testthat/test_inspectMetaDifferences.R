# load test data (df1, df2, pkList, fkList)
load(file = test_path("helper_data.rda"))

test_that("Errors",{
  df1_5 <- df1_4 <- df1_2 <- df1_3 <- df1
  expect_error(inspectMetaDifferences(df1, varName = c("1", "2"), other_GADSdat = df1_2),
               "'varName' needs to be a character vector of length 1.")
  expect_error(inspectMetaDifferences(df1, varName = "v1", other_GADSdat = df1_2),
               "The following 'varName' are not variables in the GADSdat: v1")
  expect_error(inspectMetaDifferences(df1, varName = "V1", other_GADSdat = df2),
               "The following 'other_varName' are not variables in the other_GADSdat: V1")
})

test_that("Compare two identical GADSdat objects",{
  out <- inspectMetaDifferences(df1, varName = "V1")
  expect_equal(out$varDiff, "all.equal")
  expect_equal(out$valDiff, "all.equal")

  out <- inspectMetaDifferences(df1, varName = "V1", other_GADSdat = df2, other_varName = "ID1")
  expect_equal(out$varDiff, "all.equal")
  expect_equal(out$valDiff, "all.equal")

  out2 <- inspectMetaDifferences("helper_dataBase.db", varName = "V1")
  expect_equal(out2$varDiff, "all.equal")
  expect_equal(out2$valDiff, "all.equal")
})

test_that("Differences on variable level",{
  df1_2 <- changeVarLabels(df1, "V1", "some label")
  out <- inspectMetaDifferences(df1, varName = "V1", other_GADSdat = df1_2)
  expect_equal(names(out), c("varDiff", "valDiff"))
  expect_equal(names(out$varDiff), c("GADSdat_varLabel", "other_GADSdat_varLabel"))
  expect_equal(as.character(out$varDiff[1, ]), c(NA, "some label"))
  expect_equal(out$valDiff, "all.equal")

  out2 <- inspectMetaDifferences(df1_2, varName = "ID1", other_varName = "V1")
  expect_equal(names(out2$varDiff), c("ID1_varLabel", "V1_varLabel"))
  expect_equal(as.character(out2$varDiff[1, ]), c(NA, "some label"))
})


test_that("Differences on value level",{
  df1_2 <- changeValLabels(df1, "V1", value = c(1, 3), valLabel = c("test 1", "test 2"))
  out <- inspectMetaDifferences(df1, varName = "V1", other_GADSdat = df1_2)
  expect_equal(dim(out$valDiff), c(2, 5))
  expect_equal(out$valDiff[, "value"], c(1, 3))
  expect_equal(names(out$valDiff),
               c("value", "GADSdat_valLabel", "GADSdat_missings", "other_GADSdat_valLabel", "other_GADSdat_missings"))
  expect_equal(out$valDiff[, "GADSdat_valLabel"], c(NA_character_, NA))
  expect_equal(out$valDiff[, "other_GADSdat_valLabel"], c("test 1", "test 2"))

  df1_3 <- changeValLabels(df1, "V1", value = c(1, 3), valLabel = c("test 1", "test 2b"))
  out2 <- inspectMetaDifferences(df1_2, varName = "V1", other_GADSdat = df1_3)
  expect_equal(out2$valDiff[, "value"], 3)
  expect_equal(out2$valDiff[, "GADSdat_valLabel"], c("test 2"))
  expect_equal(out2$valDiff[, "other_GADSdat_valLabel"], c("test 2b"))

  out2b <- inspectMetaDifferences(df1_3, varName = "V1", other_varName = "ID1")
  expect_equal(out2b$valDiff[, "value"], c(1, 3))
  expect_equal(names(out2b$valDiff),
               c("value", "V1_valLabel", "V1_missings", "ID1_valLabel", "ID1_missings"))


  df1_4 <- changeMissings(df1_3, "V1", value = c(1), missings = "miss")
  out3 <- inspectMetaDifferences(df1_4, varName = "V1", other_GADSdat = df1_3)
  expect_equal(out3$valDiff[, "value"], 1)
  expect_equal(out3$valDiff[, "GADSdat_missings"], c("miss"))
  expect_equal(out3$valDiff[, "other_GADSdat_missings"], c("valid"))
})

test_that("Differences after recoding",{
  pisa2 <- changeVarLabels(pisa, varName = "sameteach", varLabel = "Same math teacher")
  pisa2 <- recodeGADS(pisa2, varName = "sameteach", oldValues = c(1, 2), newValues = c(0, 1))
  out <- inspectMetaDifferences(pisa, varName = "sameteach", other_GADSdat = pisa2)

  expect_equal(dim(out$valDiff), c(3, 5))
  expect_equal(out$valDiff[, "value"], 0:2)
  expect_equal(out$valDiff[, "GADSdat_valLabel"], c(NA, "No", "Yes"))
  expect_equal(out$valDiff[, "other_GADSdat_valLabel"], c("No", "Yes", NA))
})

test_that("Multiple differences",{
  df1_2 <- changeSPSSformat(df1, "V1", "F4")
  df1_2 <- changeVarLabels(df1_2, "V1", "some label")
  out <- inspectMetaDifferences(df1, varName = "V1", other_GADSdat = df1_2)
  expect_equal(names(out$varDiff),
               c("GADSdat_varLabel", "GADSdat_format", "other_GADSdat_varLabel", "other_GADSdat_format"))
})
