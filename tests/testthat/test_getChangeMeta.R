# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

### Meta changes
changes_var <- getChangeMeta(dfSAV)
changes_val <- getChangeMeta(dfSAV, level = "value")

var_changes_list <- getChangeMeta(expected_bigList, level = "variable")
val_changes_list <- getChangeMeta(expected_bigList, level = "value")

test_that("Extract variable level meta change table", {
  out <- c(names(df1$labels)[1:4], paste0(names(df1$labels)[1:4], "_new"))
  expect_equal(names(getChangeMeta(df1)), out)
  expect_equal(dim(getChangeMeta(df1)), c(2, 8))
})

test_that("check_varChanges", {
  changes_var2 <-changes_var1 <- changes_var
  changes_var1$varName_new[1] <- "alter"
  expect_message(out <- check_varChanges(changes_var1, checkVarNames = TRUE),
                 "alter has been renamed to alterVar")
  expect_equal(out[1, "varName_new"], "alterVar")

  names(changes_var2)[8] <- "lala_new"
  expect_error(check_varChanges(changes_var2), "Irregular column names in changeTable.")
})

test_that("Extract value level meta change table", {
  out <- c("varName", "value", "valLabel", "missings", "value_new", "valLabel_new", "missings_new")
  expect_equal(names(getChangeMeta(df1, level = "value")), out)
  expect_equal(dim(getChangeMeta(df1, level = "value")), c(2, 7))
  expect_equal(dim(changes_val), c(7, 7))
  expect_silent(check_valChanges(changes_val))

  changes_val4 <- changes_val2 <- changes_val3 <- changes_val
  names(changes_val2)[7] <- "vab_new"
  changes_val3$missings_new <- "test"
  changes_val$value_new <- "test"
  changes_val4$value_new[1] <- NA
  changes_val4[8, ] <- changes_val4[1, ]
  changes_val4[8, c("value", "valLabel", "missings", "missings_new")] <- c(rep(NA, 3), "miss")

  expect_error(check_valChanges(changes_val2), "Irregular column names in changeTable.")
  expect_error(check_valChanges(changes_val3), "Irregular values in 'missings_new' column.")

  expect_error(check_valChanges(changes_val4), "Value 'NA' can not receive a value label.")
})

test_that("Characters in value columns check_valChanges", {
  changes_val4 <- changes_val2 <- changes_val3 <- changes_val1 <- changes_val0 <- changes_val
  changes_val1$value_new[5] <- "test"
  changes_val2$value[4] <- "test"
  expect_error(check_valChanges(changes_val1),
               "Column 'value_new' in 'changeTable' is character and can not be transformed to numeric.")
  expect_error(check_valChanges(changes_val2),
               "Column 'value' in 'changeTable' is character and can not be transformed to numeric.")

  changes_val3[1, "value_new"] <- changes_val0[1, "value_new"] <- 5
  changes_val3$value_new <- as.character(changes_val3$value_new)
  changes_val4$value <- as.character(changes_val4$value)
  out <- check_valChanges(changes_val3)
  expect_equal(out, changes_val0)
  out2 <- check_valChanges(changes_val4)
  expect_equal(out2, changes_val)
})

test_that("Extract list of meta change tables for all_GADSdat", {
  expect_equal(var_changes_list[[1]], getChangeMeta(df1))
  expect_equal(var_changes_list[[2]], getChangeMeta(df2))
  expect_equal(names(var_changes_list), c("df1", "df2"))
  expect_equal(val_changes_list[[1]], getChangeMeta(df1, "value"))
  expect_equal(val_changes_list[[2]], getChangeMeta(df2, "value"))
  expect_equal(names(val_changes_list), c("df1", "df2"))
})

test_that("Check changeTable different variable sets", {
  suppressMessages(dfSAV2 <- removeVars(dfSAV, c("VAR1", "VAR2")))

  expect_error(check_changeTable(dfSAV2, changes_var),
               "The following variables are not in the 'GADSdat' but in the 'changeTable': VAR1, VAR2")
  expect_error(check_changeTable(dfSAV2, changes_val),
               "The following variables are not in the 'GADSdat' but in the 'changeTable': VAR1, VAR2")

  changes_var2 <- changes_var[-1, ]
  expect_error(check_changeTable(dfSAV, changes_var2),
               "The following variables are not in the 'changeTable' but in the 'GADSdat': VAR1")
  changes_val2 <- changes_val[-c(1:3), ]
  expect_error(check_changeTable(dfSAV, changes_val2),
               "The following variables are not in the 'changeTable' but in the 'GADSdat': VAR1")
  # tbd:
  #changes_val2 <- changes_val[-c(1), ]
  #expect_error(check_changeTable(dfSAV, changes_val2),
  #             "The following variable-value pair is not in the 'changeTable' but in the 'GADSdat': VAR1")
})

test_that("Check changeTable function", {
  changes_var5 <- changes_var4 <- changes_var3 <- changes_var2 <- changes_var1 <- changes_var
  dfSAV2 <- dfSAV
  changes_var1[c(1, 3), "varLabel"] <- c("sth", "sth")
  changes_val1 <- changes_val
  changes_val1[1, "value"] <- -999

  expect_error(check_changeTable(dfSAV, changes_var1),
               "GADSdat and changeTable are not compatible in column 'varLabel' and row(s) 1, 3. Columns without '_new' should not be changed in the changeTable.",
               fixed = TRUE)
  expect_error(check_changeTable(dfSAV, changes_val1),
               "GADSdat and changeTable are not compatible in column 'value' and row(s) 1. Columns without '_new' should not be changed in the changeTable.",
               fixed = TRUE)

  changes_var4[2:3, "format"] <- c("F10.0", "F10.3")
  expect_error(check_changeTable(dfSAV, changes_var4),
               "GADSdat and changeTable are not compatible in column 'format' and row(s) 2, 3. Columns without '_new' should not be changed in the changeTable.",
               fixed = TRUE)
  changes_var5[2:3, "format"] <- c(NA, NA)
  expect_error(check_changeTable(dfSAV, changes_var5),
               "GADSdat and changeTable are not compatible in column 'format' and row(s) 2, 3. Columns without '_new' should not be changed in the changeTable.",
               fixed = TRUE)
  expect_silent(check_changeTable(dfSAV, changes_var))
  expect_silent(check_changeTable(dfSAV, changes_val))
  expect_silent(check_changeTable(df1, getChangeMeta(df1, "value")))

  changes_var2[, "display_width"] <- 0L
  dfSAV2$labels$display_width <- 0
  expect_silent(check_changeTable(dfSAV2, changes_var2))

  #changes_var3[, "display_width"] <- NA_character_
  #expect_silent(check_changeTable(dfSAV2, changes_var3))
})
