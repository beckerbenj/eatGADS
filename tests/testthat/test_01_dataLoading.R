
context("Load data from SPSS and R for Data Base")

### Name transformation
# rawDat_names <- haven::read_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_names.sav", user_na = TRUE)
rawDat_names <- haven::read_spss("helper_spss_names.sav", user_na = TRUE)

test_that("Variable name are transformed correctly ", {
  expect_identical(transf_names(names(rawDat_names)[1]), "groupVar")
  expect_identical(transf_names(names(rawDat_names)[2]), "var_1")
  expect_identical(transf_names("Select"), "SelectVar")
})

test_that("Variable name transformation is reported correctly ", {
 expect_message(transf_names(names(rawDat_names)[1]), "group has been renamed to groupVar")
 expect_message(transf_names(names(rawDat_names)[2]), "var.1 has been renamed to var_1")
})


### Data loading
test_that("savDat object created correctly", {
  expect_equal(class(load_spss("helper_spss.sav")), c("savDat", "data.frame"))
})



################# Attribute extracting ---------------------------------------------------
# rawDat <- load_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss.sav")
rawDat <- load_spss("helper_spss.sav")

label_out1 <- data.frame(varName = c("VAR1", "VAR2", "VAR3"),
                          varLabel = c("Variable 1", "Variable 2", "Variable 3"),
                          format = c("F8.2", "F8.0", "A8"),
                          display_width = c(NA, 10, NA),
                          labeled = c("yes", "yes", "no"),
                          stringsAsFactors = FALSE)
label_out2 <- data.frame(varName = c("VAR1", "VAR2"), value = c(1, 2),
                         valLabel = c("One", "Two"), missings = c("valid", "valid"), stringsAsFactors = FALSE)
label_out_all <- merge(label_out1, label_out2, by = "varName", all = TRUE)
##
class_test <- rawDat$VAR3
attributes(class_test)$format <- c("F8.0")


######### Attribute extracting on variable level
test_that("Extract attribute from data frame", {
  expect_equal(extract_attribute(rawDat$VAR1, "label"), "Variable 1")
  expect_equal(extract_attribute(class_test, "format"), "F8.0")
})

test_that("Attributes on variable level extracted correctly ", {
  expect_equal(extract_variable_level(rawDat), label_out1)
})

rawDat_exc <- rawDat
attributes(rawDat_exc[[3]]) <- NULL
test_that("Variable remains even when no attributes are present", {
  label_out_exc <- label_out1
  label_out_exc[3, c(-1, -5)] <- NA
  expect_equal(extract_variable_level(rawDat_exc), label_out_exc)
})


######### Attribute extracting on value level
# string_test <- haven::read_sav(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_exceptions.sav")
# string_test <- load_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_exceptions.sav")
string_test <- suppressWarnings(load_spss("helper_spss_exceptions.sav"))
test_that("Value label of single variable extracted for SPSS types", {
  expect_equal(extract_value_level(rawDat$VAR1, "VAR1"),
               data.frame(varName = "VAR1", value = 1, valLabel = "One", missings = "valid", stringsAsFactors = FALSE))
  expect_warning(extract_value_level(string_test$string_var, "test"),
                 "Some or all values for test cannot be coerced to numeric and are therefore changed to NA.")
  labeled_string_with_warning <- suppressWarnings(extract_value_level(string_test$string_var, "test"))
  expect_equal(labeled_string_with_warning[ ,"value"], c(NA, 99))
  expect_equal(extract_value_level(string_test$string_var, "string_var", labeledStrings = TRUE),
               data.frame(varName = "string_var", value = c("a", "99"), valLabel = c("alpha", "99"), missings = "valid", stringsAsFactors = FALSE))
})


test_that("Backward compatability to older haven classes", {
  class(rawDat$VAR1) <- "labelled_spss"
  expect_warning(extract_variable_level(rawDat),
                 "You are using an old version of haven. Please download the current version from GitHub. \n Correct importing from SPSS-files can not be guaranteed.")
  expect_equal(extract_value_level(rawDat$VAR1, "VAR1"),
               data.frame(varName = "VAR1", value = 1, valLabel = "One", missings = "valid", stringsAsFactors = FALSE))
})

test_that("Value label of single variable extracted correctly for R type variables", {
  expect_equal(extract_value_level(c(1, 2), "VAR1"), NULL)
  expect_equal(extract_value_level(c("a", "b"), "VAR1"), NULL)
  expect_equal(extract_value_level(factor(c("a", "b"), levels = c("a", "b")), "fac_var"),
               data.frame(varName = rep("fac_var", 2), value = c(1, 2), valLabel = c("a", "b"), missings = c("valid", "valid"), stringsAsFactors = FALSE))
})


test_that("Value label of multiple variables extracted correctly ", {
  expect_equal(call_extract_values(rawDat, labeledStrings = FALSE), label_out2)
})

test_that("All labels extracted correctly ", {
  expect_equal(extract_labels(rawDat, labeledStrings = FALSE),
               label_out_all)
})




### Missing Label extracting
# rawDat_missings <- haven::read_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav", user_na = TRUE)
rawDat_missings <- haven::read_spss("helper_spss_missings.sav", user_na = TRUE)

label_miss <- data.frame(varName = rep("VAR1", 3), value = c(-99, -96, 1),
                        valLabel = c("By design", "Omission", "One"), missings = c("miss", "miss", "valid"), stringsAsFactors = FALSE)
label_miss2 <- data.frame(varName = rep("VAR2", 2), value = c(-96, -99),
                         valLabel = c("missing", NA), missings = c("valid", "miss"), stringsAsFactors = FALSE)
# value labels or empirical values are used correctly for missing code generation
label_miss3 <- data.frame(varName = rep("VAR3", 2), value = c(-99, -98),
                          valLabel = c("missing", NA), missings = c("miss", "miss"), stringsAsFactors = FALSE)

test_that("Missings of single variable extracted correctly ", {
  expect_equal(extract_value_level(rawDat_missings[, 1, drop = T], "VAR1"), label_miss)
  expect_equal(extract_value_level(rawDat_missings[, 2, drop = T], "VAR2"), label_miss2)
  expect_equal(extract_value_level(rawDat_missings[, 3, drop = T], "VAR3"), label_miss3)
})

string_miss <- haven::labelled_spss(c("a", "b"), labels = c(b = "2"), na_values = c(0, "a"))
string_miss_labs <- data.frame(varName = "x", value = 2, valLabel = "b")

test_that("Missing codes for string values ", {
  expect_warning(extract_Miss_SPSS(string_miss, "x", label_df = string_miss_labs, labeledStrings = FALSE),
                 "Some or all missing codes for x cannot be coerced to numeric and are therefore changed to NA.")
  expect_equal(suppressWarnings(extract_Miss_SPSS(string_miss, "x", label_df = string_miss_labs, labeledStrings = FALSE))[, "value"], c(2, 0, NA))
  expect_equal(extract_Miss_SPSS(string_miss, "x", label_df = string_miss_labs, labeledStrings = TRUE)[, "value"], c(2, 0, "a"))
})


test_that("Haven missing lavel bug precautions", {
  expect_warning(checkValues_havenBug(c("", "la"), varName = "test"))
  expect_warning(checkValues_havenBug(c(NA, "la"), varName = "test"))
  expect_equal(suppressWarnings(checkValues_havenBug(c(NA, "", "la"), varName = "test")), "la")
})


### All SPSS importing in once
test_that("User SPSS importing function works ", {
  expected <- list(dat = data.frame(VAR1 = 1, VAR2 = 3, VAR3 = "a", stringsAsFactors = FALSE),
                   labels = label_out_all)
  class(expected) <- c("GADSdat", "list")
  expect_equal(import_spss("helper_spss.sav"), expected)
})


exceptions <- suppressWarnings(import_spss("helper_spss_exceptions.sav", labeledStrings = TRUE))
### SPSS importing exceptions
test_that("Order of variables in label df is retained", {
  expect_identical(exceptions$labels$varName[1:2], c("V2", "V1"))
})

test_that("Columns are added if not used for data for label df", {
  attr_vec <- c("varName", "varLabel", "format", "display_width", "labeled", "value", "valLabel", "missings")
  expect_identical(names(exceptions$labels), attr_vec)
})

### haven bug warning
test_that("Haven bug for value labels of long string variables does no longer exist", {
  out <- suppressWarnings(import_spss("helper_spss_havenbug.sav"))
  expect_equal(out$labels$valLabel, rep(c("one", "missing"), 4))
})

test_that("Warning for haven bug causing loss of missing codes for long strings", {
  warns <- capture_warnings(import_spss("helper_spss_havenbug.sav"))
  # warns <- capture_warnings(import_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_havenbug.sav"))
  expect_equal(warns[[1]],
                 paste("Due to a bug in haven, missing codes of character variables can be lost. Checking missing codes via checkMissings is recommended. The following variables might be affected: \n v2, v3, v4"))
})

###### test import from R data frame
test_that("Data frames directly from R are imported correctly", {
  iris2 <- import_DF(iris)
  expect_equal(dim(iris2$labels), c(7, 8))
  expect_equal(iris2$labels$valLabel,
               c(NA, NA, NA, NA, "setosa", "versicolor", "virginica"))
  expect_equal(iris2$labels$value,
               c(NA, NA, NA, NA, 1, 2, 3))
})

###### test import from eatTools convertLabel
# convertLabel_df <- readRDS(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_convertLabel.RDS")
convertLabel_df <- readRDS(file = "helper_convertLabel.RDS")

test_that("Data frames from eatTools convertLabel imported correctly", {
  out <- import_convertLabel(convertLabel_df)
  expect_equal(extractMeta(out, "year")[["varLabel"]], "year of assessment")
  expect_equal(extractMeta(out, "mig")[["varLabel"]], NA_character_)
  expect_equal(extractMeta(out, "sex")[["valLabel"]], c("female", "male"))

  expect_equal(dim(out$dat), c(5, 22))
})

###### test import from R data frame with explicit meta information
df_raw <- data.frame(a = 1:2, b = 2:3)
varLabels_raw <- data.frame(varName = c("a", "b"), varLabel = c("variable a", "variable b"), stringsAsFactors = FALSE)
valLabels_raw <- data.frame(varName = c("a", "a", "b", "b"), value = c(1, 2, 2, 3), valLabel = c("one", "two", "very", "few"), missings = rep("valid", 4), stringsAsFactors = FALSE)

test_that("Checks for import_raw", {
  iris$Species <- as.factor(iris$Species)
  expect_error(import_raw(df = iris), "One of the variables in df is a factor. All meta information on value level has to be stored in valLabels.")
  varLabels_raw_fac <- data.frame(varName = c("a", "b"), varLabel = c("variable a", "variable b"), stringsAsFactors = TRUE)
  expect_error(import_raw(df = df_raw, varLabels = varLabels_raw_fac), "One of the variables in varLabels is a factor.")
  valLabels_raw <- data.frame(varName = c("a", "a", "b", "b"), value = c(1, 2, 2, 3), valLabel = c("one", "two", "very", "few"), missings = rep("valid", 4))
  expect_error(import_raw(df = df_raw, varLabels = varLabels_raw_fac, valLabels = valLabels_raw), "One of the variables in varLabels is a factor.")

  expect_error(import_raw(df = df_raw, mtcars), "varLabels needs to contain the variables 'varName' and 'varLabel'.")
  expect_error(import_raw(df = df_raw, varLabels_raw, mtcars), "valLabels needs to contain the variables 'varName', 'value', 'varLabel' and 'missings'.")

  varLabels_raw_nam <- data.frame(varName = c("a", "d"), varLabel = c("variable a", "variable b"), stringsAsFactors = FALSE)
  expect_error(import_raw(df = df_raw, varLabels = varLabels_raw_nam), "The following variables are not in the data df: d")
  valLabels_raw_nam <- data.frame(varName = c("a", "d"), value = c(1, 2, 2, 3), valLabel = c("one", "two", "very", "few"), missings = rep("valid", 4), stringsAsFactors = FALSE)
  expect_error(import_raw(df = df_raw, varLabels = varLabels_raw, valLabels = valLabels_raw_nam), "The following variables are not in the data df: d")

  varLabels_raw_dup <- data.frame(varName = c("a", "b", "a"), varLabel = c("variable a", "variable b", NA), stringsAsFactors = FALSE)
  expect_error(import_raw(df = df_raw, varLabels = varLabels_raw_dup), "The following variables have duplicated rows in varLabels: a")

  valLabels_raw_miss <- data.frame(varName = c("a", "a", "b", "b"), value = c(1, 2, 2, 3), valLabel = c("one", "two", "very", "few"), missings = rep("vali", 4), stringsAsFactors = FALSE)
  expect_error(import_raw(df = df_raw, varLabels = varLabels_raw, valLabels = valLabels_raw_miss), "All values in column 'missings' of valLabels must be either 'valid' or 'miss'.")
})

test_that("import_raw", {
  out1 <- import_raw(df = df_raw, varLabels = varLabels_raw)
  expect_equal(out1$dat, df_raw)
  expect_equal(out1$labels$varLabel, c("variable a", "variable b"))

  out <- import_raw(df = df_raw, varLabels = varLabels_raw, valLabels = valLabels_raw)
  expect_equal(out$dat, df_raw)
  expect_equal(out$labels$varLabel, c(rep("variable a", 2), rep("variable b", 2)))

  df <- data.frame(ID = 1:4, sex = c(0, 0, 1, 1), forename = c("Tim", "Bill", "Ann", "Chris"), stringsAsFactors = FALSE)
  varLabels <- data.frame(varName = c("ID", "sex", "forename"), varLabel = c("Person Identifier", "Sex as self reported", "forename provided by teacher"), stringsAsFactors = FALSE)
  valLabels <- data.frame(varName = rep("sex", 3), value = c(0, 1, -99), valLabel = c("male", "female", "missing - omission"), missings = c("valid", "valid", "miss"), stringsAsFactors = FALSE)

  out2 <- import_raw(df = df, varLabels = varLabels, valLabels = valLabels)
  expect_equal(out2$labels$value, c(NA, 0, 1, -99, NA))
})

test_that("import_raw with tibbles", {
  out1 <- import_raw(df = df_raw, varLabels = varLabels_raw, valLabels = valLabels_raw)
  df_raw <- tibble::as_tibble(df_raw)
  varLabels_raw <- tibble::as_tibble(varLabels_raw)
  valLabels_raw <- tibble::as_tibble(valLabels_raw)
  out2 <- import_raw(df = df_raw, varLabels = varLabels_raw, valLabels = valLabels_raw)
  expect_equal(out1, out2)
})

###### check_GADSdat
# testM <- import_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav")
testM <- import_spss("helper_spss_missings.sav")
# load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_data.rda")
load(file = "helper_data.rda")


test_that("Object validater for GADSdat objects",{
  testM3 <- testM2 <- testM
  testM$dat[, "newVar"] <- NA
  expect_error(check_GADSdat(testM), "The following variables are in the data but do not have meta data: newVar")
  testM2$labels[7, "varName"] <- "newVar"
  expect_error(check_GADSdat(testM2), "The following variables have meta data but are not in the actual data: newVar")
  testM3$labels[2, "varLabel"] <- "other label"
  expect_error(check_GADSdat(testM3), "The following variable has inconsistent meta information on variable level: VAR1")

  df1_3 <- df1_1 <- df1_2 <- df1
  df1_1$labels[2, c("value")] <- -99
  df1_2$labels[2, c("valLabel")] <- "some"
  expect_error(check_GADSdat(df1_1), "The following variable has value labels but is not marked as labeled: V1")
  expect_error(check_GADSdat(df1_2), "The following variable has value labels but is not marked as labeled: V1")

  df1_3$labels[2, c("value")] <- "-99"
  expect_error(check_GADSdat(df1_3), "Column 'value' in the meta data is not numeric.")
})


# dates and times
# ------------------------------------------------------------------------
# testT <- haven::read_sav("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_times_error.sav", user_na = TRUE)
# testT <- haven::read_sav("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_times.sav", user_na = TRUE)
testT <- haven::read_sav("helper_spss_times.sav", user_na = TRUE)

test_that("Modifiying of variables of class date/time", {
  warns <- capture_warnings(out <- times2character.savDat(testT))
  expect_equal(warns[[1]], "Value labels and missing codes for 'TIMES' variables are not supported by eatGADS. Missing values are converted to NA and labels and missing codes are dropped from meta data for variable VAR1")
  expect_equal(warns[[2]], "Value labels and missing codes for 'TIMES' variables are not supported by eatGADS. Missing values are converted to NA and labels and missing codes are dropped from meta data for variable VAR2")

  expect_equal(attributes(out$VAR1)$na_values, NULL)
  expect_equal(attributes(out$VAR1)$labels, NULL)

  expect_equal(attributes(out$VAR3_1)$format.spss, c("A11"))
})

test_that("Import of variables of class date/time", {
  # out <- import_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_times.sav")
  suppressWarnings(out <- import_spss("helper_spss_times.sav"))
  expect_equal(out$dat$VAR1, c("13:00:00", NA))
  expect_equal(out$dat$VAR2, c("13:00:00", NA))
  expect_equal(out$dat$VAR1_1, c("13:00:00", "-99:00:00"))
  expect_equal(out$dat$VAR3_1, c("1989-08-12", "2009-01-27"))

  expect_equal(out$labels[1, "format"], c("A8"))
})

test_that("Import of variables of class date with labels", {
  # out <- import_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_date_labeled.sav")
  warns <- capture_warnings(out <- import_spss("helper_spss_date_labeled.sav"))
  expect_equal(warns[[1]], "Value labels and missing codes for 'DATE' variables are not supported by eatGADS and current implementation is experimental. Missing values are converted to NA and labels and missing codes are dropped from meta data for variable VAR3_1")
  expect_equal(out$dat$VAR3_1, c(NA, "2009-01-27"))

})

test_that("Errors for import of variables of class date/time", {
  #expect_error(out <- import_spss("helper_spss_times_error.sav"), "Labelled dates are currently not supported by eatGADS.")
  vec <- as.POSIXct(strptime("2011-03-27 01:30:00", "%Y-%m-%d %H:%M:%S"))
  df <- data.frame(vec)
  expect_error(out <- import_DF(df), "POSIXct and POSIXlt are currently not supported by eatGADS.")
})
