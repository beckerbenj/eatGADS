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


### Data loading
test_that("savDat object created correctly", {
  expect_equal(class(load_spss("helper_spss.sav")), c("savDat", "data.frame"))
})
