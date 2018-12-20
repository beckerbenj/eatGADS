
context("Handle data")

# load data with missings
# testM <- import_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav")
# load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_data.rda")
testM <- import_spss("helper_spss_missings.sav")
load(file = "helper_data.rda")

expected_v1 <- c(1, NA, NA, 2)

test_that("Missing Conversion Single Variable", {
  expect_equal(recodeVar(testM$dat$VAR1, testM$labels), expected_v1)
})

test_that("Missing Conversion Data frame", {
  expected <- data.frame(VAR1 = expected_v1)
  expect_equal(miss2NA(testM)[, 1, drop = F], expected)
})

##### extractMeta
test_that("Extract Metainformation", {
  expected <- testM$labels[testM$labels$varName %in% c("VAR1", "VAR2"), ]
  out <- extractMeta(testM, c("VAR1", "VAR2"))
  expect_equal(out, expected)
  expect_error(extractMeta(testM, "var4"), "The following vars are not a variable in the GADSdat:\nvar4")
  expect_equal(extractMeta(testM), testM$labels)
})

# for further class
test_that("Extract Meta from labels DF", {
  expected <- c("df1", "df1", "df2")
  out <- extractMeta(expected_labels, c("ID1", "V1"))$data_table
  expect_error(extractMeta(expected_labels[, -1], c("ID1", "V1")))
  expect_error(extractMeta(expected_labels, c("ID1", "v1")))
  expect_equal(extractMeta(expected_labels), expected_labels)
})

# for further class
test_that("Extract Meta from all_GADSdat", {
  expected <- c("df1", "df1", "df2")
  out <- extractMeta(expected_bigList, c("ID1", "V1"))$data_table
  expect_error(extractMeta(expected_bigList[, -1], c("ID1", "V1")))
  expect_error(extractMeta(expected_bigList, c("ID1", "v1")))
  expect_equal(extractMeta(expected_bigList), expected_bigList$allLabels)
})

# for further class
test_that("Extract Meta from DB path", {
  expect_error(extractMeta(c("helper_dataBase.db", "helper_dataBase.db"), c("ID1", "V1")))
  expected <- c("df1", "df1", "df2")
  out <- extractMeta("helper_dataBase.db", c("ID1", "V1"))$data_table
  expect_error(extractMeta(expected_bigList[, -1], c("ID1", "V1")))
  expect_error(extractMeta(expected_bigList, c("ID1", "v1")))
  expect_equal(extractMeta(expected_bigList), expected_bigList$allLabels)
})


######## extractData
testM2 <- testM
testM2$dat[, "Var_char"] <- c("a", "b", "c", "d")
testM2$dat[, "Var_char2"] <- c("b", "b", "b", "b")
testM2$labels[8, ] <- c("Var_char", NA, NA, NA, NA, NA, NA, NA)
testM2$labels[9, ] <- c("Var_char2", NA, NA, NA, "labeled", "b", "b_value", NA)

test_that("Warnings and errors for Extract Data",  {
  w <- capture_warnings(extractData(testM))
  expect_equal(w[[1]], "Variable VAR1 is partially labeled. Value labels will be dropped for this variable variable.\nLabeled values are: 1")
  expect_equal(w[[2]], "Variable VAR2 is partially labeled. Value labels will be dropped for this variable variable.\nLabeled values are: -96")
  expect_error(extractData(testM, convertLabels = "integer"), "Argument convertLabels incorrectly specified.")
})

test_that("Extract data", {
  out <- suppressWarnings(extractData(testM))
  expect_equal(out[, 1], c(1, NA, NA, 2))
  out2 <- suppressWarnings(extractData(testM, convertMiss = FALSE))
  expect_equal(out2[, 1], c(1, -99, -96, 2))
})

test_that("Extract data for strings", {
  out <- suppressWarnings(extractData(testM2))
  expect_equal(class(out$Var_char), "character")
  expect_equal(out$Var_char, c("a", "b", "c", "d"))
})

test_that("Extract data for strings into factors", {
  out <- suppressWarnings(extractData(testM2, convertLabels = "factor"))
  expect_equal(class(out$Var_char), "character")
  expect_equal(class(out$Var_char2), "factor")
  expect_equal(out$Var_char2, as.factor(c("b_value", "b_value", "b_value", "b_value")))
})


