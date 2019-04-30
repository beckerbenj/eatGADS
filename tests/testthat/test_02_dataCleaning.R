
context("Data Cleaning")

# load test data (df1, df2, pkList, fkList)
# load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# dfSAV <- import_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

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



test_that("Merge to GADSdat objects",{
  df3 <- df1
  mod_dat <- df3$dat
  mod_dat[, "v3"] <- c(8, 7)
  df3 <- suppressMessages(updateMeta(df3, mod_dat))
  expect_error(merge(df1, df3, by = 2))
  expect_error(merge(df1, df3, by = "x2"))
  expect_error(merge(df1, df1, by = "V1"))
  out <- merge(df1, df3, by = "V1")
  expect_equal(out$dat[, 3], c(8, 7))
  expect_equal(nrow(out$labels), 3)
})

test_that("compare_variables", {
  out <- compare_and_order(1:3, 3:1)
  expect_equal(out$in_both_ordered, 3:1)
  expect_warning(compare_and_order(1:3, 2:3, name2 = "set2"), "The following variables are not in set2: 1")
  out2 <- suppressWarnings(compare_and_order(1:3, 2:3, name2 = "set2"))
  expect_equal(out2$not_in_set2, 1)
  expect_warning(compare_and_order(1:3, 4:1, name1 = "set1"), "The following variables are not in set1: 4")
  out3 <- suppressWarnings(compare_and_order(1:3, 4:1, name1 = "set1"))
  expect_equal(out3$in_both_ordered, c(3:1))

})


test_that("Variable ordering in GADS", {
  expect_error(orderLike(df1, c()), "newOrder is not a character vector.")
  out <- orderLike(df1, c("V1", "ID1"))
  expect_equal(out$dat, df1$dat[, c(2, 1)])
  expect_equal(out$labels, df1$labels[2:1, ])
  out2 <- orderLike(dfSAV, c("VAR2", "VAR3", "VAR1"))
  expect_equal(out2$dat, dfSAV$dat[, c(2, 3, 1)])
  expect_equal(out2$labels, dfSAV$labels[c(4:5, 6:7, 1:3), ])
  # warnings
  warn1 <- capture_warnings(orderLike(dfSAV, c("VAR2", "VAR4")))
  expect_equal(warn1[[1]], "The following variables are not in GADSdat: VAR4")
  expect_equal(warn1[[2]], "The following variables are not in new Order: VAR1, VAR3")
  expect_warning(capture.output(orderLike(dfSAV, c("VAR2", "VAR3"))), "The following variables are not in new Order: VAR1")
  out3 <- suppressWarnings(orderLike(dfSAV, c("VAR2", "VAR3")))
  expect_equal(out3$dat, dfSAV$dat[, c(2, 3, 1)])
})





