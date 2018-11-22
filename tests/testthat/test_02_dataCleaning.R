
context("Data Cleaning")

# load test data (df1, df2, pkList, fkList)
# load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_data.rda")
load(file = "helper_data.rda")


### check missings
df4 <- df3 <- df1
df3$labels[1, "missings"] <- "miss"
df4$labels[2, "valLabel"] <- "missing"


test_that("Missing checks raise no false alarms", {
  expect_equal(df1, checkMissings(df1))
  expect_equal(df2, checkMissings(df2, "mis"))
})

test_that("Missing labels are correctly checked and added", {
  expect_message(checkMissings(df3, addMissingLabel = FALSE),
                 "The following variables have values coded as missing but value label does not include the term 'missing':\nID1")
  all_messages <- capture_messages(checkMissings(df3))
  expect_equal(all_messages[2],
               "'generic missing' is inserted into column valLabel for 1 rows.\n")
  expect_equal(checkMissings(df3)$labels[1, "valLabel"], "generic missing")
})

test_that("Missing codes are correctly checked and added", {
  expect_message(checkMissings(df4, addMissingCode = FALSE),
                 "The following variables have value labels including the term 'missing' which are not coded as missing:\nV1")
  all_messages <- capture_messages(checkMissings(df4))
  expect_equal(all_messages[2],
               "'miss' is inserted into column missings for 1 rows.\n")
  expect_equal(checkMissings(df4)$labels[2, "missings"], "miss")
})


### Meta changes
# dfSAV <- import_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")
changes <- getChangeMeta(dfSAV, c("varName", "valLabel"))
changes_var <- changes_val <- changes

test_that("Extract meta change table", {
  out <- c(names(df1$labels), "varName_new", "varLabel_new", "valLabel_new")
  expect_equal(names(getChangeMeta(df1, changeCol = c("varName", "varLabel", "valLabel"))), out)
  expect_equal(dim(getChangeMeta(df1)), c(2, 15))
  expect_error(getChangeMeta(df1, changeCol = c("varName", "value")))
  expect_error(getChangeMeta(df1, changeCol = c("varName", "vallabel")),
               "At least on variable name supplied in changeCol is not an actual column of the meta data table.")
})

test_that("Check changeTable function", {
  changes5 <- changes4 <- changes2 <- changes3 <- changes
  changes2[, "some_changes"] <- NA
  changes3[, "value_new"] <- NA
  changes4[1, "varLabel"] <- "sth"
  changes5[1, "varLabel_new"] <- "sth"
  expect_error(applyChangeMeta(dfSAV, changes2), "Illegal additional column names in changeTable.")
  expect_error(applyChangeMeta(dfSAV, changes3), "Illegal additional column names in changeTable.")
  expect_error(applyChangeMeta(dfSAV, changes4), "GADSdat and changeTable are not compatible. Columns without '_new' should not be changed in the changeTable.")
  expect_error(applyChangeMeta(dfSAV, changes5), "Variable VAR1 has varying changes on variable level.")
})

test_that("Changes to GADSdat on variable level", {
  changes_var[1:3, "varName_new"] <- "new1"
  g1 <- applyChangeMeta(dfSAV, changes_var)
  expect_equal(g1$labels[, -1], dfSAV$labels[, -1])
  expect_equal(g1$labels$varName, c(rep("new1", 3), rep("VAR2", 2), rep("VAR3", 2)))
  expect_equal(names(g1$dat), c("new1", "VAR2", "VAR3"))
})

test_that("Changes to GADSdat on value level", {
  changes_val[1, "valLabel_new"] <- "new_miss"
  changes_val[2, "valLabel_new"] <- "new_miss2"
  g2 <- applyChangeMeta(dfSAV, changes_val)
  expect_equal(g2$labels[, -7], dfSAV$labels[, -7])
  expect_equal(g2$labels$valLabel, c("new_miss", "new_miss2", "One", "missing", NA, "missing", NA))
  expect_equal(names(g2$dat), names(dfSAV$dat))
})

