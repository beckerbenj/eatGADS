
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

label_out <- data.frame(varName = c("VAR1", "VAR2"), varLabel = c("Variable 1", "Variable 2"), value = c(1, 2),
                        label = c("One", "Two"), missings = c(NA, NA), stringsAsFactors = FALSE)

test_that("Variable label extracted correctly ", {
  expect_equal(extract_varLabels(rawDat),
                   data.frame(varName = c("VAR1", "VAR2"), varLabel = c("Variable 1", "Variable 2"), stringsAsFactors = FALSE))
})

test_that("Value label of single variable extracted correctly ", {
  expect_equal(extract_VL_SPSS(rawDat$VAR1, "VAR1"),
               data.frame(varName = "VAR1", value = 1, label = "One", missings = NA, stringsAsFactors = FALSE))
})

test_that("Value label of multiple variables extracted correctly ", {
  expect_equal(extract_valueLabels(rawDat),
               data.frame(varName = c("VAR1", "VAR2"), value = c(1, 2),
                          label = c("One", "Two"), missings = c(NA, NA), stringsAsFactors = FALSE))
})

test_that("All labels extracted correctly ", {
  expect_equal(extract_labels(rawDat, type = "SPSS"), label_out)
})

### Missing Label extracting
rawDat_missings <- haven::read_spss("helper_spss_missings.sav", user_na = TRUE)

label_miss <- data.frame(varName = rep("VAR1", 3), value = c(-99, -96, 1),
                        label = c("By design", "Omission", "One"), missings = c("miss", "miss", NA), stringsAsFactors = FALSE)

test_that("Value label of single variable extracted correctly ", {
  expect_equal(extract_VL_SPSS(rawDat_missings[, 1, drop = T], "VAR1"), label_miss)
})


### All SPSS importing in once
test_that("User SPSS importing function works ", {
  expected <- list(dat = data.frame(VAR1 = 1, VAR2 = 2),
                   labels = label_out)
  expect_equal(import_spss("helper_spss.sav"), expected)
})



### Importing via R/RDS
# tbd (when R importing is done)

