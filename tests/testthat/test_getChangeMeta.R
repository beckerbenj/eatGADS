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
  names(changes_var)[8] <- "lala_new"
  expect_error(check_varChanges(changes_var), "Irregular column names in changeTable.")
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
  changes_val4$missings_new[1] <- "miss"

  expect_error(check_valChanges(changes_val2), "Irregular column names in changeTable.")
  expect_error(check_valChanges(changes_val3), "Irregular values in 'missings_new' column.")
  expect_error(check_valChanges(changes_val), "String values can not be given value labels.")

  expect_error(check_valChanges(changes_val4), "Value 'NA' can not receive a value label.")
})

test_that("Extract list of meta change tables for all_GADSdat", {
  expect_equal(var_changes_list[[1]], getChangeMeta(df1))
  expect_equal(var_changes_list[[2]], getChangeMeta(df2))
  expect_equal(names(var_changes_list), c("df1", "df2"))
  expect_equal(val_changes_list[[1]], getChangeMeta(df1, "value"))
  expect_equal(val_changes_list[[2]], getChangeMeta(df2, "value"))
  expect_equal(names(val_changes_list), c("df1", "df2"))
})

test_that("Check changeTable function", {
  changes_var2 <- changes_var1 <- changes_var
  dfSAV2 <- dfSAV
  changes_var1[1, "varLabel"] <- "sth"
  changes_val1 <- changes_val
  changes_val1[1, "varName"] <- "sth"

  expect_error(check_changeTable(dfSAV, changes_var1),
               "GADSdat and changeTable are not compatible. Columns without '_new' should not be changed in the changeTable.")
  expect_error(check_changeTable(dfSAV, changes_val1),
               "GADSdat and changeTable are not compatible. Columns without '_new' should not be changed in the changeTable.")
  expect_silent(check_changeTable(dfSAV, changes_var))
  expect_silent(check_changeTable(dfSAV, changes_val))
  expect_silent(check_changeTable(df1, getChangeMeta(df1, "value")))

  changes_var2[, "display_width"] <- 0L
  dfSAV2$labels$display_width <- 0
  expect_silent(check_changeTable(dfSAV2, changes_var2))
})
