# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")


### Update Meta
newDat <- df1$dat
newDat$v3 <- c(4, 5)
newDat$V1 <- NULL

test_that("Remove rows meta helper", {
  expect_message(remove_rows_meta(df1$labels, names(newDat)), "Removing the following rows from meta data: V1")
  expect_equal(suppressMessages(remove_rows_meta(df1$labels, names(newDat))), df1$labels[df1$labels$varName == "ID1", ])

  expect_message(remove_rows_meta(df1$labels, names(df1$dat)), "No rows removed from meta data.")
  expect_equal(suppressMessages(remove_rows_meta(df1$labels, names(df1$dat))), df1$labels)
})

test_that("Add rows to meta helper", {
  expect_message(out <- add_rows_meta(df1$labels, newDat, checkVarNames = FALSE),
                 "Adding meta data for the following variables: v3")
  expect_equal(out, import_DF(newDat[, "v3", drop = F]))

  expect_message(out2 <- add_rows_meta(df1$labels, df1$dat, checkVarNames = FALSE),
                 "No rows added to meta data.")
  expect_equal(out2, new_GADSdat(dat = data.frame(), labels = data.frame()))
})


test_that("Update Meta GADSdat", {
  out_both <- updateMeta(df1, newDat)
  expect_equal(suppressMessages(updateMeta(df1, df1$dat)), df1)

  newTibble <- tibble::as_tibble(newDat)
  expect_error(updateMeta(df1, newTibble), "newDat needs to be a data.frame. Use as.data.frame is necessary.")
})

test_that("Update Meta all_GADSdat", {
  no_changes_messages <- capture_messages(updateMeta(expected_bigList, expected_bigList$datList))
  expect_equal(no_changes_messages,
               c("Analyzing data table df1:\n", "No rows removed from meta data.\n", "No rows added to meta data.\n", "Analyzing data table df2:\n", "No rows removed from meta data.\n", "No rows added to meta data.\n"))
  expect_equal(suppressMessages(updateMeta(expected_bigList, expected_bigList$datList)), expected_bigList)

  new_datList <- expected_bigList$datList
  new_datList$df1 <- newDat
  new_datList$df2$v5 <- as.factor(c("a", "b"))
  changes_messages <- capture_messages(updateMeta(expected_bigList, new_datList))
  expect_equal(changes_messages, c("Analyzing data table df1:\n", "Removing the following rows from meta data: V1\n", "Adding meta data for the following variables: v3\n",
                                   "Analyzing data table df2:\n", "No rows removed from meta data.\n", "Adding meta data for the following variables: v5\n"))

  changes_out <- suppressMessages(updateMeta(expected_bigList, new_datList))
  expect_equal(changes_out$datList$df2, data.frame(df2$dat, v5 = c(1, 2)))
  expect_equal(changes_out$allLabels$varName, c("ID1", "v3", "ID1", "V2", "v5", "v5"))
})

test_that("Invalid variable names are checked and changed", {
  newDat_ill <- df1$dat
  newDat_ill[, "Alter"] <- NA
  mess <- capture_messages(out_both <- updateMeta(df1, newDat_ill))
  expect_equal(mess[2], "Alter has been renamed to AlterVar\n")
  expect_equal(names(out_both$dat), c("ID1", "V1", "AlterVar"))
  #out_both <- updateMeta(df1, newDat_ill)
  #expect_equal(names(out_both$dat), c("ID1", "V1", "Alter"))
})

test_that("updateMeta extractData combination", {
  suppressWarnings(newDat_ill <- extractData2(dfSAV, labels2character = namesGADS(dfSAV), dropPartialLabels = FALSE))
  suppressMessages(out_both <- updateMeta(dfSAV, newDat_ill))
  expect_equal(attr(out_both$dat[[1]], "label"), NULL)
  expect_equal(attr(out_both$dat[[3]], "label"), NULL)
})
