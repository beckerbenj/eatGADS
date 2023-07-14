# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

test_that("Errors",{
  df1_5 <- df1_4 <- df1_2 <- df1_3 <- df1
  expect_error(inspectMetaDifferences(c("1", "2"), df1, df1_2),
               "'varName' needs to be a character vector of length 1.")
  expect_error(inspectMetaDifferences("v1", df1, df1_2),
               "The following 'varName' are not variables in the GADSdat1: v1")
})

test_that("Compare two identical GADSdat objects",{
  out <- inspectMetaDifferences("V1", df1, df1)
  expect_null(out$varDiff)
  expect_null(out$valDiff)

  out2 <- inspectMetaDifferences("V1", "helper_dataBase.db",
                                "helper_dataBase.db")
  expect_null(out2$varDiff)
  expect_null(out2$valDiff)
})

test_that("Differences on variable level",{
  df1_2 <- changeVarLabels(df1, "V1", "some label")
  out <- inspectMetaDifferences("V1", df1, df1_2)
  expect_equal(names(out), c("varDiff", "valDiff"))
  expect_equal(dim(out$varDiff), c(1, 5))
  expect_equal(out$varDiff[1, 2], NA_character_)
  expect_equal(out$varDiff[1, 4], "some label")
  expect_null(out$valDiff)
})


test_that("Differences on value level",{
  df1_2 <- changeValLabels(df1, "V1", value = c(1, 3), valLabel = c("test 1", "test 2"))
  out <- inspectMetaDifferences("V1", df1, df1_2)

  expect_equal(dim(out$valDiff), c(2, 6))
  expect_equal(out$valDiff[, "value"], c(1, 3))
  expect_equal(out$valDiff[, "GADS1.valLabel"], c(NA_character_, NA))
  expect_equal(out$valDiff[, "GADS2.valLabel"], c("test 1", "test 2"))

  df1_3 <- changeValLabels(df1, "V1", value = c(1, 3), valLabel = c("test 1", "test 2b"))
  out2 <- inspectMetaDifferences("V1", df1_2, df1_3)

  expect_equal(dim(out2$valDiff), c(1, 6))
  expect_equal(out2$valDiff[, "value"], 3)
  expect_equal(out2$valDiff[, "GADS1.valLabel"], c("test 2"))
  expect_equal(out2$valDiff[, "GADS2.valLabel"], c("test 2b"))

  df1_4 <- changeMissings(df1_3, "V1", value = c(1), missings = "miss")
  out3 <- inspectMetaDifferences("V1", df1_4, df1_3)

  expect_equal(out3$valDiff[, "value"], 1)
  expect_equal(out3$valDiff[, "GADS1.missings"], c("miss"))
  expect_equal(out3$valDiff[, "GADS2.missings"], c("valid"))
})

test_that("Differences after recoding",{
  pisa2 <- changeVarLabels(pisa, varName = "sameteach", varLabel = "Same math teacher")
  pisa2 <- recodeGADS(pisa2, varName = "sameteach", oldValues = c(1, 2), newValues = c(0, 1))
  out <- inspectMetaDifferences("sameteach", pisa, pisa2)

  expect_equal(dim(out$valDiff), c(3, 6))
  expect_equal(out$valDiff[, "value"], 0:2)
  expect_equal(out$valDiff[, "GADS1.valLabel"], c(NA, "No", "Yes"))
  expect_equal(out$valDiff[, "GADS2.valLabel"], c("No", "Yes", NA))
})
