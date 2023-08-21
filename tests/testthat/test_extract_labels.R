
################# Attribute extracting ---------------------------------------------------
# rawDat <- load_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss.sav")
rawDat <- load_spss("helper_spss.sav")

# rawDat_missings <- haven::read_spss(file = "tests/testthat/helper_spss_missings.sav", user_na = TRUE)
rawDat_missings <- haven::read_spss("helper_spss_missings.sav", user_na = TRUE)

# rawDat_miss_noValLabel <- haven::read_spss(file = "tests/testthat/helper_spss_missings_no_valLabels.sav", user_na = TRUE)
rawDat_miss_noValLabel <- haven::read_spss("helper_spss_missings_no_valLabels.sav", user_na = TRUE)



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
  expect_equal(extract_attribute(rawDat$VAR1, "labels"), c("One" = 1))
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
})

test_that("Conflicting value label for string variable from SPSS", {
  f1 <- haven::labelled("a",
                       labels = c("a value" = "99", "a value" = "99.0", "a value" = "99.00",
                                  "another value" = "98", "another value" = "98.0"))
  f2 <- haven::labelled("a", labels = c("a value" = "99", "another value" = "99.0" ))

  expect_warning(out1 <- extract_value_level(f1, varName = "test"),
                 "Duplicate value labels or missing tags are dropped for variable 'test'. Only the respective first value label or missing tag is preserved and zero-decimals are dropped.")
  expect_equal(out1$value, c(99, 98))
  expect_equal(out1$valLabel, c("a value", "another value"))

  expect_warning(out2 <- extract_value_level(f2, varName = "test"),
                 "Duplicate value labels or missing tags are dropped for variable 'test'. Only the respective first value label or missing tag is preserved and zero-decimals are dropped.")
  expect_equal(out2$value, c(99))
  expect_equal(out2$valLabel, c("a value"))

  expect_warning(out3 <- extract_value_level(rawDat_miss_noValLabel$VAR3, varName = "test"),
                 "Duplicate value labels or missing tags are dropped for variable 'test'. Only the respective first value label or missing tag is preserved and zero-decimals are dropped.")
  expect_equal(out3$value, c(99))
  expect_equal(out3$valLabel, NA_character_)
  expect_equal(out3$missings, "miss")
})

test_that("Backward compatability to older haven classes", {
  class(rawDat$VAR1) <- "labelled_spss"
  expect_warning(extract_variable_level(rawDat),
                 "You are using an old version of haven. Please download the current version from CRAN. \n Correct importing from SPSS-files can not be guaranteed.")
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
  expect_equal(call_extract_values(rawDat), label_out2)
})

test_that("All labels extracted correctly ", {
  expect_equal(extract_labels(rawDat),
               label_out_all)
})




### Missing Label extracting
test_that("Missings of single variable extracted correctly ", {
  label_miss <- data.frame(varName = rep("VAR1", 3), value = c(-99, -96, 1),
                           valLabel = c("By design", "Omission", "One"), missings = c("miss", "miss", "valid"), stringsAsFactors = FALSE)
  expect_equal(extract_value_level(rawDat_missings[, 1, drop = T], "VAR1"), label_miss)

  label_miss2 <- data.frame(varName = rep("VAR2", 2), value = c(-96, -99),
                            valLabel = c("missing", NA), missings = c("valid", "miss"), stringsAsFactors = FALSE)
  expect_equal(extract_value_level(rawDat_missings[, 2, drop = T], "VAR2"), label_miss2)

  # value labels or empirical values are used correctly for missing code generation
  label_miss3 <- data.frame(varName = rep("VAR3", 2), value = c(-99, -98),
                            valLabel = c("missing", NA), missings = c("miss", "miss"), stringsAsFactors = FALSE)
  expect_equal(extract_value_level(rawDat_missings[, 3, drop = T], "VAR3"), label_miss3)
})

test_that("Haven missing lavel bug precautions", {
  expect_warning(checkValues_havenBug(c("", "la"), varName = "test"))
  expect_warning(checkValues_havenBug(c(NA, "la"), varName = "test"))
  expect_equal(suppressWarnings(checkValues_havenBug(c(NA, "", "la"), varName = "test")), "la")
})

test_that("Missings of single variable extracted correctly ", {
  miss_noValLabel <- data.frame(varName = rep("VAR1", 2), value = c(-99, -88),
                                valLabel = c(NA_character_, NA_character_), missings = c("miss", "miss"), stringsAsFactors = FALSE)
  expect_equal(extract_value_level(rawDat_miss_noValLabel[, 1, drop = T], "VAR1"), miss_noValLabel)
  miss_noValLabel2 <- data.frame(varName = rep("VAR2", 2), value = c(-99, "miss"),
                                valLabel = c(NA_character_, NA_character_), missings = c("miss", "miss"), stringsAsFactors = FALSE)
  expect_equal(extract_value_level(rawDat_miss_noValLabel[, 2, drop = T], "VAR2"), miss_noValLabel2)
})




