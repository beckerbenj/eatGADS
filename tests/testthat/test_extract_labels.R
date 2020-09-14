
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
  expect_equal(extract_value_level(factor(c(NA, NA)), "VAR1"), NULL)
})


test_that("Value label of multiple variables extracted correctly ", {
  expect_equal(call_extract_values(rawDat, labeledStrings = FALSE), label_out2)
})

test_that("All labels extracted correctly ", {
  expect_equal(extract_labels(rawDat, labeledStrings = FALSE),
               label_out_all)
})




### Missing Label extracting
# rawDat_missings <- haven::read_spss(file = "tests/testthat/helper_spss_missings.sav", user_na = TRUE)
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

