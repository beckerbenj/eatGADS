
context("Handle data")

# load data with missings
# testM <- import_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav")
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
  expect_error(extractMeta(testM, "var4"))
})

# for further class
test_that("Extract Meta from labels DF", {
  expected <- c("df1", "df1", "df2")
  out <- extractMeta(expected_labels, c("ID1", "V1"))$data_table
  expect_error(extractMeta(expected_labels[, -1], c("ID1", "V1")))
  expect_error(extractMeta(expected_labels, c("ID1", "v1")))
})

# for further class
test_that("Extract Meta from all_GADSdat", {
  expected <- c("df1", "df1", "df2")
  out <- extractMeta(expected_bigList, c("ID1", "V1"))$data_table
  expect_error(extractMeta(expected_bigList[, -1], c("ID1", "V1")))
  expect_error(extractMeta(expected_bigList, c("ID1", "v1")))
})
