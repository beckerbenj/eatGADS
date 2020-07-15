
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# gBig <- getGADS(filePath = "tests/testthat/helper_dataBase.db")
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
