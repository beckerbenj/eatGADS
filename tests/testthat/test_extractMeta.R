
# load data with missings
# testM <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
# load(file = "tests/testthat/helper_data.rda")
# testM <- import_spss("tests/testthat/helper_spss_missings.sav")
testM <- import_spss("helper_spss_missings.sav")
load(file = "helper_data.rda")

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

test_that("Extract meta from trendGADS", {
  trend_g <- getTrendGADS(filePaths = c("helper_dataBase.db", "helper_dataBase2.db"), years = c(2012, 2018),
                      fast = FALSE, verbose = FALSE)
  out <- extractMeta(trend_g)
  expect_equal(dim(out), c(8, 9))
  expect_equal(out$data_table, c(rep("gads2012", 4), rep("gads2018", 4)))

})
