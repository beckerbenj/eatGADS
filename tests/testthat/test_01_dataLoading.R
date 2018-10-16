
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


### Data loading
test_that("savDat object created correctly", {
  expect_equal(class(load_spss("helper_spss.sav")), c("savDat", "data.frame"))
})



################# Attribute extracting ---------------------------------------------------
rawDat <- load_spss("helper_spss.sav")

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


######### Attribute extracting on variable level
test_that("Extract attribute from data frame", {
  expect_equal(extract_attribute(rawDat$VAR1, "label"), "Variable 1")
  expect_equal(extract_attribute(class_test, "class"), "spss_labelled, labelled")
})

test_that("Attributes on variable level extracted correctly ", {
  expect_equal(extract_variable_level(rawDat), label_out1)
})

rawDat_exc <- rawDat
attributes(rawDat_exc[[3]]) <- NULL
test_that("Variable remains even when no attributes are present", {
  label_out_exc <- label_out1
  label_out_exc[3, -1] <- NA
  expect_equal(extract_variable_level(rawDat_exc), label_out_exc)
})


######### Attribute extracting on value level
string_test <- load_spss("helper_spss_exceptions.sav")
test_that("Value label of single variable extracted correctly for SPSS type variables", {
  expect_equal(extract_value_level(rawDat$VAR1, "VAR1"),
               data.frame(varName = "VAR1", value = 1, valLabel = "One", missings = NA, stringsAsFactors = FALSE))
  expect_warning(extract_value_level(string_test$string_var, "test"),
                 "Values for test cannot be coerced to numeric and have been dropped.")
  expect_equal(extract_value_level(string_test$string_var, "string_var", labeledStrings = TRUE),
               data.frame(varName = "string_var", value = "a", valLabel = "alpha", missings = NA, stringsAsFactors = FALSE))
})

test_that("Value label of single variable extracted correctly for R type variables", {
  expect_equal(extract_value_level(c(1, 2), "VAR1"), NULL)
  expect_equal(extract_value_level(c("a", "b"), "VAR1"), NULL)
  expect_equal(extract_value_level(factor(c("a", "b"), levels = c("a", "b")), "fac_var"),
               data.frame(varName = rep("fac_var", 2), value = rep(1, 2), valLabel = c("a", "b"), missings = NA_character_, stringsAsFactors = FALSE))
})


test_that("Value label of multiple variables extracted correctly ", {
  expect_equal(call_extract_values(rawDat, labeledStrings = FALSE), label_out2)
})

test_that("All labels extracted correctly ", {
  expect_equal(extract_labels(rawDat, labeledStrings = FALSE),
               label_out_all)
})

### Missing Label extracting
rawDat_missings <- haven::read_spss("helper_spss_missings.sav", user_na = TRUE)

label_miss <- data.frame(varName = rep("VAR1", 3), value = c(-99, -96, 1),
                        valLabel = c("By design", "Omission", "One"), missings = c("miss", "miss", NA), stringsAsFactors = FALSE)

test_that("Missings of single variable extracted correctly ", {
  expect_equal(extract_value_level(rawDat_missings[, 1, drop = T], "VAR1"), label_miss)
})


### All SPSS importing in once
test_that("User SPSS importing function works ", {
  expected <- list(dat = data.frame(VAR1 = 1, VAR2 = 3, VAR3 = "a", stringsAsFactors = FALSE),
                   labels = label_out_all)
  class(expected) <- "GADSdat"
  expect_equal(import_spss("helper_spss.sav"), expected)
})




exceptions <- import_spss("helper_spss_exceptions.sav", labeledStrings = TRUE)
### SPSS importing exceptions
test_that("Order of variables in label df is retained", {
  expect_identical(exceptions$labels$varName[1:2], c("V2", "V1"))
})

test_that("Columns are added if not used for data for label df", {
  attr_vec <- c("varName", "varLabel", "format", "display_width", "class", "value", "valLabel", "missings")
  expect_identical(names(exceptions$labels), attr_vec)
})



###### test import from R data frame
test_that("Data frames directly from R are imported correctly", {
  iris2 <- import_DF(iris)
  expect_equal(dim(iris2$labels), c(7, 8))
  expect_equal(iris2$labels$valLabel,
               c(NA, NA, NA, NA, "setosa", "versicolor", "virginica"))
})






