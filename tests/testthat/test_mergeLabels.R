
# load test data (df1, df2, pkList, fkList)
# load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_data.rda")
load(file = "helper_data.rda")


### Merging dataframes and labels to one big list
test_that("Merging labels", {
  expect_equal(mergeLabels(df1 = df1, df2 = df2), expected_bigList)
})

test_that("Errors for missing/wrong data table names", {
  expect_error(mergeLabels(df1 = df1, df2),
               "All input has to be named! See help for further clarification.")
  expect_error(mergeLabels(df1 = df1, df1 = df2),
               "Names for data frames are duplicated!")
})

test_that("Adding data frame identifier", {
  expect <- data.frame(ID1 = c(1, 2), V1 = c(3, 5), data_table = "df1", stringsAsFactors = FALSE)
  expect_identical(add_DFname(df1$dat, "df1"), expect)
})


test_that("Check manually created all_GADSdat objects", {
  expected_bigList2 <- expected_bigList
  names(expected_bigList2$datList$df1) <- c("ID1", "X1")
  expect_error(check_all_GADSdat(expected_bigList2))
  expected_bigList$allLabels <- expected_bigList$allLabels[, -ncol(expected_bigList$allLabels)]
  expect_error(eatGADS:::check_all_GADSdat(expected_bigList), "data_table column is missing in labels data frame.")
})

