
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

### Meta changes
changes_var <- getChangeMeta(dfSAV)
changes_val <- getChangeMeta(dfSAV, level = "value")

var_changes_list <- getChangeMeta(expected_bigList, level = "variable")
val_changes_list <- getChangeMeta(expected_bigList, level = "value")


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
  changes_val3 <- changes_val2 <- changes_val
  changes_val2[1, "valLabel_new"] <- "new_miss"
  changes_val2[2, "valLabel_new"] <- "new_miss2"
  g2 <- applyChangeMeta(changes_val2, dfSAV)
  expect_equal(g2$labels[, -7], dfSAV$labels[, -7])
  expect_equal(g2$labels$valLabel, c("new_miss", "new_miss2", "One", "missing", NA, "missing", NA))
  expect_equal(names(g2$dat), names(dfSAV$dat))

  changes_val3[3, "missings_new"] <- "miss"
  g3 <- applyChangeMeta(changes_val3, dfSAV)
  expect_equal(g3$labels[, -8], dfSAV$labels[, -8])
  expect_equal(g3$labels$missings[1:3], rep("miss", 3))
})

test_that("Changes to GADSdat: recoding", {
  changes_val[3, "value_new"] <- 10
  g1 <- applyChangeMeta(changes_val, dfSAV)
  expect_equal(g1$labels$value[3], 10)
  expect_equal(g1$dat[1, 1], 10)
  changes_val[4, "value_new"] <- "test"
  expect_error(applyChangeMeta(changes_val, dfSAV))
})

test_that("recoding if potential danger of overwriting old values!", {
  df_rec <- data.frame(v1 = c("x", "y", "z"), b = c("b", "a", "d"), stringsAsFactors = TRUE)
  df_rec <- import_DF(df_rec)
  chang <- getChangeMeta(df_rec, level = "value")
  chang2 <- chang
  chang2[, "value_new"] <- c(3, 4, 1, NA, NA, NA)
  out <-  applyChangeMeta(chang2, df_rec)
  expect_equal(out$labels$value, c(1, 3, 4, 1, 2, 3))
  expect_equal(out$labels$valLabel, c("z", "x", "y", "a", "b", "d"))
  expect_equal(out$dat$v1, c(3, 4, 1))
  ## partial recoding, multiple variables
  chang[, "value_new"] <- c(3, NA, 1, 2, 1, 5)
  out <-  applyChangeMeta(chang, df_rec)
  expect_equal(out$labels$value, c(1, 2, 3, 1, 2, 5))
  expect_equal(out$labels$valLabel, c("z", "y", "x", "b", "a", "d"))
  expect_equal(out$dat$v1, 3:1)
  expect_equal(out$dat$b, c(1, 2, 5))

  changes_val2 <- changes_val
  changes_val2[1:2, "value_new"] <- c(-96, -95)
  out3 <- applyChangeMeta(changes_val2, dfSAV)
  expect_equal(out3$labels$value[1:3], c(-96, -95, 1))
})

test_that("Recoding with value meta data conflicts", {
  changes_val2 <- changes_val
  changes_val2[1, "value_new"] <- 1
  expect_error(out <- recode_labels(dfSAV$labels, changes_val2, existingMeta = "stop"),
               "Values in 'value_new' with existing meta data in variable VAR1: 1")
  out <- recode_labels(dfSAV$labels, changes_val2, existingMeta = "value")
  comp1 <- dfSAV$labels[-3, ]
  comp1[1, "value"] <- 1
  comp1 <- comp1[c(2, 1, 3:6),]
  rownames(comp1) <- NULL
  expect_equal(comp1, out)

  out2 <- recode_labels(dfSAV$labels, changes_val2, existingMeta = "value_new")
  comp2 <- dfSAV$labels[-3, ]
  comp2[1, "value"] <- 1
  comp2[1, "valLabel"] <- "One"
  comp2[1, "missings"] <- "valid"
  comp2 <- comp2[c(2, 1, 3:6),]
  rownames(comp2) <- NULL
  expect_equal(comp2, out2)
})

test_that("Recoding multiple value into the same value (without meta data conflicts)", {
  changes_val2 <- changes_val
  changes_val2[1:3, "value_new"] <- 10
  expect_error(out <- recode_labels(dfSAV$labels, changes_val2, existingMeta = "stop"),
               "Duplicated values in 'value_new' causing conflicting meta data in variable VAR1: 10. Use 'existingMeta' = 'drop' to drop all related meta data.")
  expect_error(out <- recode_labels(dfSAV$labels, changes_val2, existingMeta = "value"),
               "Duplicated values in 'value_new' causing conflicting meta data in variable VAR1: 10. Use 'existingMeta' = 'drop' to drop all related meta data.")
  expect_error(out <- recode_labels(dfSAV$labels, changes_val2, existingMeta = "value_new"),
               "Duplicated values in 'value_new' causing conflicting meta data in variable VAR1: 10. Use 'existingMeta' = 'drop' to drop all related meta data.")

  out2 <- recode_labels(dfSAV$labels, changes_val2, existingMeta = "drop")
  expect_equal(out2[1, "value"], 10)
  expect_equal(out2[1, "valLabel"], NA_character_)
  expect_equal(out2[1, "missings"], "valid")
  expect_equal(out2[2, "varName"], "VAR2")
})


test_that("Recoding multiple value into the same value (with meta data conflicts)", {
  changes_val2 <- changes_val
  changes_val2[1:2, "value_new"] <- 1
  expect_error(out <- recode_labels(dfSAV$labels, changes_val2, existingMeta = "stop"),
               "Values in 'value_new' with existing meta data in variable VAR1: 1")
  expect_error(out <- recode_labels(dfSAV$labels, changes_val2, existingMeta = "value"),
               "Multiple values are recoded into 1 for variable VAR1. Value meta data can thus not be used from 'value'. Set 'existingMeta' to 'value_new'.")

  out2 <- recode_labels(dfSAV$labels, changes_val2, existingMeta = "value_new")
  comp2 <- dfSAV$labels[-(2:3), ]
  comp2[1, "value"] <- 1
  comp2[1, "valLabel"] <- "One"
  comp2[1, "missings"] <- "valid"
  rownames(comp2) <- NULL
  expect_equal(comp2, out2)

  out2b <- recode_labels(dfSAV$labels, changes_val2, existingMeta = "drop")
  expect_equal(out2b[1, "value"], 1)
  expect_equal(out2b[1, "valLabel"], NA_character_)
  expect_equal(out2b[1, "missings"], "valid")

  out3 <- applyChangeMeta(changes_val2, GADSdat = dfSAV, existingMeta = "value_new")
  expect_equal(out3$dat$VAR1, c(1, 1, 1, 2))
  expect_equal(comp2, out3$labels)
})


test_that("Recoding multiple value into the same value (with and without meta data conflicts)", {
  dfSAVb <- changeValLabels(dfSAV, "VAR1", value = 2, valLabel = "two")
  changes_valb <- getChangeMeta(dfSAVb, level = "value")
  changes_valb[1:2, "value_new"] <- 1
  changes_valb[3:4, "value_new"] <- 10
  expect_error(out <- recode_labels(dfSAVb$labels, changes_valb, existingMeta = "stop"),
               "Duplicated values in 'value_new' causing conflicting meta data in variable VAR1: 1, 10. Use 'existingMeta' = 'drop' to drop all related meta data.")
  expect_error(out <- recode_labels(dfSAVb$labels, changes_valb, existingMeta = "value"),
               "Duplicated values in 'value_new' causing conflicting meta data in variable VAR1: 1, 10. Use 'existingMeta' = 'drop' to drop all related meta data.")
  expect_error(out <- recode_labels(dfSAVb$labels, changes_valb, existingMeta = "value_new"),
               "Duplicated values in 'value_new' causing conflicting meta data in variable VAR1: 1, 10. Use 'existingMeta' = 'drop' to drop all related meta data.")

  out2 <- recode_labels(dfSAVb$labels, changes_valb, existingMeta = "drop")
  expect_equal(out2[1, "value"], 1)
  expect_equal(out2[2, "value"], 10)
  expect_equal(out2[1, "valLabel"], NA_character_)
  expect_equal(out2[2, "valLabel"], NA_character_)
  expect_equal(out2[1, "missings"], "valid")
  expect_equal(out2[2, "missings"], "valid")
  expect_equal(out2[3, "varName"], "VAR2")
})

changes_val2 <- rbind(changes_val, data.frame(varName = "VAR1", value = NA, valLabel = NA, missings = NA, value_new = 2, valLabel_new = "Two", missings_new = "valid", stringsAsFactors = FALSE))
changes_val3 <- rbind(changes_val2, data.frame(varName = "VAR1", value = NA, valLabel = NA, missings = NA, value_new = 3, valLabel_new = "Three", missings_new = "valid", stringsAsFactors = FALSE))

test_that("Expand labels", {
  out <- expand_labels(df1$labels, new_varName_vec = c("ID1", "ID1", "V1"))
  expect_equal(out$varName, c("ID1", "ID1", "V1"))
  expect_equal(out$labeled, rep("no", 3))
  out2 <- expand_labels(df2$labels, new_varName_vec = c("ID1", "V2", "V2"))
  expect_equal(out2$varName, c("ID1", "V2", "V2"))
  expect_equal(out2$labeled, c("no", "yes", "yes"))
  expect_equal(out2$varLabel, c(NA, "Variable 2", "Variable 2"))
  expect_equal(out2$value, c(NA, 99, NA))
  out3 <- expand_labels(dfSAV$labels, changes_val2$varName)
  expect_equal(out3[1:4, "value"], c(-99, -96, 1, NA))
  expect_equal(out3[1:4, "varName"], rep("VAR1", 4))
})

test_that("Adding value labels for values without labels", {
  out <- recode_labels(dfSAV$labels, changes_val2)
  expect_equal(out$value[1:4], c(-99, -96, 1, 2))
  expect_equal(dim(out), c(8, 8))

  df1_changes <- getChangeMeta(df1, level = "value")
  df1_changes[2, "value_new"] <- 99
  df1_changes[2, "valLabel_new"] <- "test"
  out2 <- recode_labels(df1$labels, df1_changes)
  expect_equal(out2$value, c(NA, 99))
  expect_equal(dim(out2), c(2, 8))
  expect_equal(out2$labeled, c("no", "yes"))
  expect_equal(out2$missings, c(NA, "valid"))

  # multiple new value labels
  out3 <- applyChangeMeta(changes_val3, dfSAV)
  expect_equal(dim(out3$labels), c(9, 8))
  expect_equal(out3$labels$value[3:5], 1:3)
  expect_equal(out3$labels$valLabel[3:5], c("One", "Two", "Three"))
})

test_that("update labeled helper", {
  g <- import_DF(mtcars)
  g$labels[1, "value"] <- 1
  out <- update_labeled_col(g$labels)
  expect_equal(out[1, "labeled"], "yes")
  expect_equal(out[2, "labeled"], "no")
})

test_that("update of labeled column", {
  dfSAV2 <- removeValLabels(dfSAV, "VAR3", value = c(-99, -98))
  dfSAV2$labels[3, "value"] <- NA
  expect_silent(check_GADSdat(dfSAV2))

  changeTab <- getChangeMeta(dfSAV2, "value")
  changeTab[6, "value_new"] <- -99
  changeTab[6, "valLabel_new"] <- "a label"

  out <- applyChangeMeta(changeTab, dfSAV2)
})

test_that("Adding value labels to an unlabeled variable", {
  iris2 <- as.data.frame(iris, stringsAsFactors = TRUE)
  suppressMessages(g <- import_DF(iris2))
  changer <- getChangeMeta(g, level = "value")

  changer[1, "value_new"] <- 99
  changer[1, "valLabel_new"] <- "some label"
  changer[1, "missings_new"] <- "valid"
  out <- applyChangeMeta(changer, g)

  expect_equal(out[[2]][1, ][6:8], data.frame(value = 99, valLabel = "some label", missings = "valid", stringsAsFactors = FALSE))
  expect_equal(out[[2]][1, ][5], data.frame(labeled = "yes", stringsAsFactors = FALSE))
})

test_that("Changes to all_GADSdat on variable level", {
  var_changes_list2 <- var_changes_list
  var_changes_list2$df1[1, "varName_new"] <- "test1"
  var_changes_list2$df2[2, "varName_new"] <- "test2"
  g1 <- applyChangeMeta(var_changes_list2, expected_bigList)
  expect_equal(g1$allLabels$varName, c("test1", "V1", "ID1", "test2"))
  expect_equal(names(g1$datList$df1), c("test1", "V1"))
  var_changes_list$df1[1, "varLabel_new"] <- "test1"
  var_changes_list$df2[2, "varLabel_new"] <- "test2"
  g1 <- applyChangeMeta(var_changes_list, expected_bigList)
  expect_equal(g1$allLabels$varLabel, c("test1", NA, NA, "test2"))
})

test_that("Changes to all_GADSdat on value level", {
  val_changes_list2 <- val_changes_list
  val_changes_list2$df2[2, "valLabel_new"] <- "test1"
  g1 <- applyChangeMeta(val_changes_list2, expected_bigList)
  expect_equal(g1$allLabels$valLabel, c(NA, NA, NA, "test1"))
  # values
  val_changes_list$df2[2, "value_new"] <- -99
  g1 <- applyChangeMeta(val_changes_list, expected_bigList)
  expect_equal(g1$allLabels$value, c(NA, NA, NA, -99))
  # values that don't have a label
  val_changes_list$df2[2, "value_new"] <- -99
})


test_that("Changes to GADSdat if tibble or data.frame, varNames", {
  # varName
  changes_var_tbl <- changes_var
  changes_var_tbl[1, "varName_new"] <- "new1"
  changes_var_tbl <- tibble::as_tibble(changes_var_tbl)

  g1 <- applyChangeMeta(changes_var_tbl, dfSAV)
  expect_equal(g1$labels[, -1], dfSAV$labels[, -1])
  expect_equal(g1$labels$varName, c(rep("new1", 3), rep("VAR2", 2), rep("VAR3", 2)))
  expect_equal(names(g1$dat), c("new1", "VAR2", "VAR3"))

  changes_var_df <- as.data.frame(changes_var_tbl)
  g3 <- applyChangeMeta(changes_var_df, dfSAV)
  expect_equal(g3$labels[, -1], dfSAV$labels[, -1])

})

test_that("Changes to GADSdat if tibble or data.frame, value labels", {
  changes_val_tbl <- changes_val
  changes_val_tbl[1, "valLabel_new"] <- "new_miss"
  changes_val_tbl[2, "valLabel_new"] <- "new_miss2"
  changes_val_tbl <- tibble::as_tibble(changes_val_tbl)

  g2 <- applyChangeMeta(changes_val_tbl, dfSAV)
  expect_equal(g2$labels[, -7], dfSAV$labels[, -7])
  expect_equal(g2$labels$valLabel, c("new_miss", "new_miss2", "One", "missing", NA, "missing", NA))
  expect_equal(names(g2$dat), names(dfSAV$dat))

  changes_val_df <- as.data.frame(changes_val_tbl)
  g4 <- applyChangeMeta(changes_val_df, dfSAV)
  expect_equal(g4$labels[, -7], dfSAV$labels[, -7])
})


test_that("sort value labels", {
  unsorted <- dfSAV$labels[c(3:1), ]
  out <- sort_value_labels(unsorted)

  expect_equal(out, dfSAV$labels[1:3,])
})

