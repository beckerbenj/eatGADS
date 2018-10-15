
context("Load data from SPSS and R for Data Base")

### Name transformation
rawDat_names <- haven::read_spss("helper_spss_names.sav", user_na = TRUE)

test_that("Variable name are transformed correctly ", {
  expect_identical(transf_names(names(rawDat_names)[1]), "groupVar")
  expect_identical(transf_names(names(rawDat_names)[2]), "var_1")
})

test_that("Variable name transformation is reported correctly ", {
 expect_message(transf_names(names(rawDat_names)[1]), "group has been renamed to groupVar")
 expect_message(transf_names(names(rawDat_names)[2]), "var.1 has been renamed to var_1")
})



### Label extracting
rawDat <- haven::read_spss("helper_spss.sav", user_na = TRUE)

label_out1 <- data.frame(varName = c("VAR1", "VAR2", "VAR3"),
                          varLabel = c("Variable 1", "Variable 2", "Variable 3"),
                          format = c("F8.2", "F8.0", "A8"),
                          display_width = c(NA, 10, NA),
                          class = c("haven_labelled", "haven_labelled", NA),
                          stringsAsFactors = FALSE)

label_out2 <- data.frame(varName = c("VAR1", "VAR2"), value = c(1, 2),
                         valLabel = c("One", "Two"), missings = c(NA, NA), stringsAsFactors = FALSE)

label_out_all <- merge(label_out1, label_out2, by = "varName", all = TRUE)

##
class_test <- rawDat$VAR3
attributes(class_test)$class <- c("spss_labelled", "labelled")

test_that("Extract attribute from data frame", {
  expect_equal(extract_attribute(rawDat$VAR1, "label"), "Variable 1")
  expect_equal(extract_attribute(class_test, "class"), "spss_labelled, labelled")
})

test_that("Attributes except value labels extracted correctly ", {
  expect_equal(extract_variable_level(rawDat), label_out1)
})

test_that("Value label of single variable extracted correctly ", {
  expect_equal(extract_VL_SPSS(rawDat$VAR1, "VAR1"),
               data.frame(varName = "VAR1", value = 1, valLabel = "One", missings = NA, stringsAsFactors = FALSE))
})

string_test <- rawDat$VAR3
attributes(string_test)$labels <- c("One" = "One")

test_that("Value label of single variable extracted correctly ", {
  expect_equal(extract_VL_SPSS(rawDat$VAR1, "VAR1"),
               data.frame(varName = "VAR1", value = 1, valLabel = "One", missings = NA, stringsAsFactors = FALSE))
  expect_warning(extract_VL_SPSS(string_test, "test"),
                 "Values for test cannot be coerced to numeric and have been dropped.")
  expect_equal(extract_VL_SPSS(string_test, "VAR3", labeled_strings = TRUE),
               data.frame(varName = "VAR3", value = "One", valLabel = "One", missings = NA, stringsAsFactors = FALSE))
})

test_that("Value label of multiple variables extracted correctly ", {
  expect_equal(extract_value_level(rawDat, labeledStrings = FALSE), label_out2)
})

test_that("All labels extracted correctly ", {
  expect_equal(extract_labels(rawDat, type = "SPSS", labeledStrings = FALSE),
               label_out_all)
})

### Missing Label extracting
rawDat_missings <- haven::read_spss("helper_spss_missings.sav", user_na = TRUE)

label_miss <- data.frame(varName = rep("VAR1", 3), value = c(-99, -96, 1),
                        valLabel = c("By design", "Omission", "One"), missings = c("miss", "miss", NA), stringsAsFactors = FALSE)

test_that("Value label of single variable extracted correctly ", {
  expect_equal(extract_VL_SPSS(rawDat_missings[, 1, drop = T], "VAR1"), label_miss)
})


### All SPSS importing in once
test_that("User SPSS importing function works ", {
  expected <- list(dat = data.frame(VAR1 = 1, VAR2 = 3, VAR3 = "a", stringsAsFactors = FALSE),
                   labels = label_out_all)
  expect_equal(import_spss("helper_spss.sav"), expected)
})


exceptions <- import_spss("helper_spss_exceptions.sav")
### SPSS importing exceptions
test_that("Order of variables in label df is retained", {
  expect_identical(exceptions$labels$varName, c("V2", "V1"))
})

test_that("Columns are added if not used for data for label df", {
  attr_vec <- c("varName", "varLabel", "format", "display_width", "class", "value", "valLabel", "missings")
  expect_identical(names(exceptions$labels), attr_vec)
})



### Importing via R/RDS
# tbd (when R importing is done)








