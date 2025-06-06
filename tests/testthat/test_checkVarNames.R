### Name transformation
rawDat_names <- haven::read_spss(test_path("helper_spss_names.sav"), user_na = TRUE)

# load test data (df1, df2, pkList, fkList)
load(file = test_path("helper_data.rda"))

### check missings
df4 <- df3 <- df1
df3$labels[1, "missings"] <- "miss"
df4$labels[2, "valLabel"] <- "missing"
df4$labels[3, ] <- df4$labels[2, ]
# wrong valid value
df4$labels[3, "value"] <- -99
df4$labels[3, "valLabel"] <- "missing by design"
df4$labels[3, "missings"] <- "valid"
df4$labels[2:3, "labeled"] <- "yes"


test_that("Variable name are transformed correctly for character vectors", {
  expect_error(checkVarNames(c("group", "var.1", "Select", NA)),
               "Column names can not be NA.")
  all_messages <- capture_messages(out <- checkVarNames(c("group", "var.1", "Select", "test", "Test")))

  expect_equal(all_messages[1], "group has been renamed to groupVar\n")
  expect_equal(all_messages[2], "var.1 has been renamed to var_1\n")
  expect_equal(all_messages[3], "Select has been renamed to SelectVar\n")
  expect_equal(all_messages[4], "Test has been renamed to Test_2\n")
  expect_identical(out, c("groupVar", "var_1", "SelectVar", "test", "Test_2"))

  input <- c("group", "var.1", "Select", "test", "Test")
  out2 <- checkVarNames(input, checkDuplicates = FALSE, checkDots = FALSE, checkKeywords = FALSE)
  expect_equal(out2, input)
})

test_that("Variable name are transformed correctly for data.frames", {
  all_messages2 <- capture_messages(out2 <- checkVarNames(rawDat_names))

  expect_equal(all_messages2[1], "group has been renamed to groupVar\n")
  expect_equal(all_messages2[2], "var.1 has been renamed to var_1\n")
  expect_identical(names(out2), c("groupVar", "var_1"))
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

test_that("Unduplicate", {
  out <- unduplicate(c("v1", "V1", "v2"))
  expect_equal(out, c("v1", "V1_2", "v2"))
  out <- unduplicate(c("v1", "V1", "v2"))

  df <- data.frame("var1" = 1, "Var1" = 1, "vAr1" = 1)
  out2 <- prepare_labels(df, TRUE, TRUE)
  expect_equal(names(out2$dat), c("var1", "Var1_2", "vAr1_2_2"))
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
