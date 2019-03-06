
context("Prepare data for Data Base")

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


### Extracting GADSdat from all_GADSdat (reverse to merge above)
test_that("Checks performed correctly", {
  expect_error(extractGADSdat(df1, "df1"))
  expect_error(extractGADSdat(expected_bigList, "df3"), "name has to be the name of a GADSdat element of all_GADSdat.")
  expect_error(extractGADSdat(expected_bigList, c("df1", "df2")), "name has to be a character vector of length 1.")
})

test_that("Extract GADSdat performed correctly", {
  expect_equal(extractGADSdat(expected_bigList, "df1"), df1)
  expect_equal(extractGADSdat(expected_bigList, "df2"), df2)
})

# gBig <- getGADS(filePath = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase.db")
gBig <- getGADS(filePath = "helper_dataBase.db")

### Split GADS into all_GADSdat
test_that("drop duplicates", {
  df <- data.frame(a = c(1, 1, 2))
  expect_message(drop_duplicates(df, df_name = "df"), "Rows have been dropped from df")
  mt2 <- mtcars
  rownames(mt2) <- NULL
  expect_equal(drop_duplicates(mtcars, df_name = "mtcars"), mt2)
})

test_that("Checks for splitGADS performed correctly", {
  expect_error(splitGADS(gBig, nameList = list(df2 = c("ID1", "V2"))), "nameList needs to be a list at least of length 2.")
  expect_error(splitGADS(gBig, nameList = list(c("ID1", "V1"), df2 = c("ID1", "V2"))), "All elements of nameList must be named.")
  expect_error(splitGADS(gBig, nameList = list(df1 = c("ID1", "x1"), df2 = c("ID1", "V2"))), "All names used in nameList vectors have to be names in the GADSdat.")
})

test_that("Split GADSdat performed correctly", {
  expect_message(splitGADS(gBig, nameList = list(df1 = c("ID1", "V1"), df2 = c("ID1", "V2"))), "Rows have been dropped from df1")
  out <- splitGADS(gBig, nameList = list(df1 = c("ID1", "V1"), df2 = c("ID1", "V2")))
  expect_equal(out$datList[[1]], df1[["dat"]])
  expect_equal(out$datList[[2]][1:2,], df2[["dat"]])
  expect_equal(names(out$datList), c("df1", "df2"))
  expect_equal(out$allLabels, expected_labels)
})
