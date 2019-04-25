
context("Data Cleaning")

# load test data (df1, df2, pkList, fkList)
# load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_data.rda")
load(file = "helper_data.rda")


### check missings
df4 <- df3 <- df1
df3$labels[1, "missings"] <- "miss"
df4$labels[2, "valLabel"] <- "missing"
df4$labels[3, ] <- df4$labels[2, ]
# wrong valid value
df4$labels[3, "value"] <- -99
df4$labels[3, "valLabel"] <- "missing by design"
df4$labels[3, "missings"] <- "valid"

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
               "'miss' is inserted into column missings for 2 rows.\n")
  expect_equal(checkMissings(df4)$labels[2, "missings"], "miss")
  expect_equal(checkMissings(df4)$labels[3, "missings"], "miss")
})


### Meta changes
# dfSAV <- import_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")
changes_var <- getChangeMeta(dfSAV)
changes_val <- getChangeMeta(dfSAV, level = "value")

test_that("Extract variable level meta change table", {
  out <- c(names(df1$labels)[1:5], paste0(names(df1$labels)[1:5], "_new"))
  expect_equal(names(getChangeMeta(df1)), out)
  expect_equal(dim(getChangeMeta(df1)), c(2, 10))
  names(changes_var)[8] <- "lala_new"
  expect_error(check_varChanges(changes_var), "Irregular column names in changeTable.")
})

test_that("Extract value level meta change table", {
  out <- c("varName", "value", "valLabel", "missings", "value_new", "valLabel_new", "missings_new")
  expect_equal(names(getChangeMeta(df1, level = "value")), out)
  expect_equal(dim(getChangeMeta(df1, level = "value")), c(2, 7))
  expect_equal(dim(changes_val), c(7, 7))
  expect_silent(check_valChanges(changes_val))
  changes_val2 <- changes_val3 <- changes_val
  names(changes_val2)[7] <- "vab_new"
  changes_val3$missings_new <- "test"
  changes_val$value_new <- "test"

  expect_error(check_valChanges(changes_val2), "Irregular column names in changeTable.")
  expect_error(check_valChanges(changes_val3), "Irregular values in 'missings_new' column.")
  expect_error(check_valChanges(changes_val), "String values can not be given value labels.")
})

test_that("Check changeTable function", {
  changes_var1 <- changes_var
  changes_var1[1, "varLabel"] <- "sth"
  changes_val1 <- changes_val
  changes_val1[1, "varName"] <- "sth"

  expect_error(check_changeTable(dfSAV, changes_var1), "GADSdat and changeTable are not compatible. Columns without '_new' should not be changed in the changeTable.")
  expect_error(check_changeTable(dfSAV, changes_val1), "GADSdat and changeTable are not compatible. Columns without '_new' should not be changed in the changeTable.")
  expect_silent(check_changeTable(dfSAV, changes_var))
  expect_silent(check_changeTable(dfSAV, changes_val))
})

test_that("Changes to GADSdat on variable level", {
  # varName
  changes_var[1, "varName_new"] <- "new1"
  g1 <- applyChangeMeta(changes_var, dfSAV)
  expect_equal(g1$labels[, -1], dfSAV$labels[, -1])
  expect_equal(g1$labels$varName, c(rep("new1", 3), rep("VAR2", 2), rep("VAR3", 2)))
  expect_equal(names(g1$dat), c("new1", "VAR2", "VAR3"))
  # others
  changes_var[2, "varLabel_new"] <- "new1"
  g2 <- applyChangeMeta(changes_var, dfSAV)
  expect_equal(g2$labels$varLabel, c(rep("Variable 1", 3), rep("new1", 2), rep("Variable 3", 2)))
})

test_that("Changes to GADSdat on value level", {
  changes_val[1, "valLabel_new"] <- "new_miss"
  changes_val[2, "valLabel_new"] <- "new_miss2"
  g2 <- applyChangeMeta(changes_val, dfSAV)
  expect_equal(g2$labels[, -7], dfSAV$labels[, -7])
  expect_equal(g2$labels$valLabel, c("new_miss", "new_miss2", "One", "missing", NA, "missing", NA))
  expect_equal(names(g2$dat), names(dfSAV$dat))
  changes_val[4, "value_new"] <- 5
  expect_error(applyChangeMeta(changes_val, dfSAV), "Changes to values are not implemented yet.")
})


#### Wrapper for Variable Names
test_that("Errors are called for changeVarNames", {
  expect_error(changeVarNames(dfSAV, oldNames = c("VA2"), newNames = c("test")), "varName in oldNames is not a real variable name.")
  expect_error(changeVarNames(dfSAV, oldNames = c("VAR2"), newNames = c("test", "2")), "oldNames and newNames are not of identical length.")
  expect_error(changeVarNames(dfSAV, oldNames = c("VAR2"), newNames = 1), "oldNames and newNames are not character vectors.")
})

test_that("changeVarNames for GADSdat", {
  out_single <- changeVarNames(dfSAV, oldNames = c("VAR2"), newNames = c("test"))
  expect_equal(names(out_single$dat), unique(out_single$labels$varName))
  expect_equal(names(out_single$dat), c("VAR1", "test", "VAR3"))
  out_double <- changeVarNames(dfSAV, oldNames = c("VAR1", "VAR3"), newNames = c("test", "test2"))
  expect_equal(names(out_double$dat), unique(out_double$labels$varName))
  expect_equal(names(out_double$dat), c("test", "VAR2", "test2"))
})

test_that("changeVarNames for all_GADSdat", {
  out_single <- changeVarNames(expected_bigList, oldNames = c("V1"), newNames = c("var1"))
  expect_equal(names(out_single$datList$df1), c("ID1", "var1"))
  expect_equal(out_single$allLabels$varName, c("ID1", "var1", "ID1", "V2"))

  out_multiple <- changeVarNames(expected_bigList, oldNames = c("V1", "V2"), newNames = c("var1", "var2"))
  expect_equal(names(out_multiple$datList$df1), c("ID1", "var1"))
  expect_equal(names(out_multiple$datList$df2), c("ID1", "var2"))
  expect_equal(out_multiple$allLabels$varName, c("ID1", "var1", "ID1", "var2"))

  out_double <- changeVarNames(expected_bigList, oldNames = c("ID1"), newNames = c("idstud"))
  expect_equal(names(out_double$datList$df1), c("idstud", "V1"))
  expect_equal(names(out_double$datList$df2), c("idstud", "V2"))
  expect_equal(out_double$allLabels$varName, c("idstud", "V1", "idstud", "V2"))

})



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
  expect_message(add_rows_meta(df1$labels, newDat), "Adding meta data for the following variables: v3")
  expect_equal(suppressMessages(add_rows_meta(df1$labels, newDat)), import_DF(newDat[, "v3", drop = F]))

  expect_message(add_rows_meta(df1$labels, df1$dat), "No rows added to meta data.")
  expect_equal(suppressMessages(add_rows_meta(df1$labels, df1$dat)), new_GADSdat(dat = data.frame(), labels = data.frame()))
})


test_that("Update Meta GADSdat", {
  out_both <- updateMeta(df1, newDat)
  expect_equal(suppressMessages(updateMeta(df1, df1$dat)), df1)

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


### Check VarNames
dot_df <- import_DF(iris, checkVarNames = FALSE)

test_that("Check varNames", {
  # if no changes
  expect_equal(checkVarNames(df1), df1)
  # if changes
  expect_message(checkVarNames(dot_df))
  changed_df <- suppressMessages(checkVarNames(dot_df))
  imported_df <- suppressMessages(import_DF(iris))
  expect_equal(changed_df, imported_df)
})


test_that("Check varNames all_GADSdat", {
  # if no changes
  expect_equal(checkVarNames(expected_bigList), expected_bigList)
  # if changes
  names(expected_bigList$datList$df1)[1] <- "group"
  names(expected_bigList$datList$df2)[1] <- "group"
  expected_bigList$allLabels[expected_bigList$allLabels$varName == "ID1", "varName"] <- "group"

  expect_message(checkVarNames(expected_bigList))
  #changed_df <- suppressMessages(checkVarNames(expected_bigList))
  #imported_df <- suppressMessages(import_DF(iris))
  #expect_equal(changed_df, imported_df)
})

# dfSAV <- import_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav")
test_that("Drop missing labels from meta", {
  # if no changes
  expect_equal(drop_missing_labels(df1$labels[df1$labels$varName == "ID1", ]), df1$labels[df1$labels$varName == "ID1", ])
  # if changes
  out <- dfSAV$labels[3, ]
  row.names(out) <- NULL
  expect_equal(drop_missing_labels(dfSAV$labels[dfSAV$labels$varName == "VAR1", ]), out)
  # if no value labels left
  expect_equal(drop_missing_labels(dfSAV$labels[dfSAV$labels$varName == "VAR3", ]),
               data.frame(varName = "VAR3", varLabel = "Variable 3", format = "F8.2", display_width = NA_real_, labeled = "no", value = NA_real_, valLabel = NA_character_, missings = NA_character_, stringsAsFactors = FALSE))
})


test_that("Transfer meta information from one GADSdat to another", {
  dat2 <- import_DF(dfSAV$dat)
  dat3 <- reuseMeta(dat2, varName = "VAR1", dfSAV)
  dat3 <- reuseMeta(dat3, varName = "VAR2", dfSAV)
  dat3 <- reuseMeta(dat3, varName = "VAR3", dfSAV)
  expect_equal(dfSAV, dat3)
  dat4 <- changeVarNames(dat2, oldNames = "VAR1", newNames = "v1")
  dat5 <- reuseMeta(dat4, varName = "v1", dfSAV, other_varName = "VAR1")
  expect_silent(check_GADSdat(dat5))
})











