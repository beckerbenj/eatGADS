rawDat <- load_spss(test_path("helper_spss.sav"))

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

### All SPSS importing in once
test_that("User SPSS importing function works ", {
  expected <- list(dat = data.frame(VAR1 = 1, VAR2 = 3, VAR3 = "a", stringsAsFactors = FALSE),
                   labels = label_out_all)
  class(expected) <- c("GADSdat", "list")
  expect_equal(import_spss("helper_spss.sav"), expected)
})


exceptions <- suppressWarnings(import_spss("helper_spss_exceptions.sav", labeledStrings = "keep"))
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
  out <- import_spss(test_path("helper_spss_havenbug.sav"))
  expect_equal(out$labels[["format"]], c("F8.2", "F8.2", "A8", "A8", "A9", "A9", "A10", "A10", "A200", "A200"))
  expect_equal(extractMeta(out, "v5")[["missings"]], c("valid", "miss"))
  expect_equal(extractMeta(out, "v5")[["valLabel"]], c("one", "missing"))
  expect_equal(extractMeta(out, "v5")[["value"]], c(1, 99))
})


### Data loading
test_that("savDat object created correctly", {
  expect_equal(class(load_spss("helper_spss.sav")), c("savDat", "data.frame"))
})
