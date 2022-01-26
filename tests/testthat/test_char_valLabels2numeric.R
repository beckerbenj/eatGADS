
num_values <- haven::labelled_spss(1:3, labels = c(alpha = 1, beta = 2), na_values = -99)
num_values_df <- tibble::tibble(v1 = 1:3, v2 = num_values)

string_values <- haven::labelled_spss(c("a", "b", "c", 5), labels = c(alpha = "a", beta = "b", gamma = 5))
string_values_df <- tibble::tibble(v1 = 1:4, v2 = string_values)

string_miss <- haven::labelled_spss(c(1, 2, "m"), labels = c(alpha = "1", beta = "2"), na_values = "m")
string_miss_df <- tibble::tibble(v1 = 1:3, v2 = string_miss)

string_combi <- haven::labelled_spss(c("a", "b", "c", 5), labels = c(alpha = "a", beta = "b", gamma = 5), na_values = c("c"))
string_combi_df <- tibble::tibble(v1 = 1:4, v2 = string_combi)

test_that("No labeled character values", {
  expect_equal(char_valLabels2numeric.savDat(num_values_df, labeledStrings = "drop"), num_values_df)
})

test_that("Labeled character values and keep value labels", {
  expect_warning(out1 <- char_valLabels2numeric.savDat(string_values_df, labeledStrings = "keep"),
                 "Some values with value labels or missing tags of variable v2 cannot be coerced to numeric.")
  expect_equal(out1, string_values_df)
})

test_that("Labeled character values and drop value labels", {
  expect_warning(out1 <- char_valLabels2numeric.savDat(string_values_df, labeledStrings = "drop"),
                 "Some values with value labels or missing tags of variable v2 cannot be coerced to numeric and are therefore changed to NA.")
  expect_equal(attributes(out1$v1), attributes(string_values_df$v1))
  expect_equal(attributes(out1$v2)$labels, c(alpha = NA_real_, beta = NA_real_, gamma = 5))
})

test_that("Labeled character values and transform value labels", {
  expect_warning(out1 <- char_valLabels2numeric.savDat(string_values_df, labeledStrings = "transform"),
                 "Some values with value labels or missing tags of variable v2 cannot be coerced to numeric. Therefore all underlying values are recoded to numeric.")
  expect_equal(attributes(out1$v1), attributes(string_values_df$v1))
  expect_equal(attributes(out1$v2)$labels, c(alpha = 1, beta = 2, gamma = 3))
  expect_equal(as.character(out1[["v2"]]), c(1, 2, "c", 3))
})

test_that("Missing tagged character values and keep value labels", {
  expect_warning(out1 <- char_valLabels2numeric.savDat(string_miss_df, labeledStrings = "keep"),
                 "Some values with value labels or missing tags of variable v2 cannot be coerced to numeric.")
  expect_equal(out1, string_miss_df)
})

test_that("Missing tagged character values and drop value labels", {
  expect_warning(out1 <- char_valLabels2numeric.savDat(string_miss_df, labeledStrings = "drop"),
                 "Some values with value labels or missing tags of variable v2 cannot be coerced to numeric and are therefore changed to NA.")
  expect_equal(attributes(out1$v1), attributes(string_values_df$v1))
  expect_equal(attributes(out1$v2)$labels, c(alpha = 1, beta = 2))
  expect_equal(attributes(out1$v2)$na_values, NA_real_)
})

test_that("Missing tagged character values and transform value labels", {
  expect_warning(out1 <- char_valLabels2numeric.savDat(string_miss_df, labeledStrings = "transform"),
                 "Some values with value labels or missing tags of variable v2 cannot be coerced to numeric. Therefore all underlying values are recoded to numeric.")
  expect_equal(attributes(out1$v1), attributes(string_miss_df$v1))
  expect_equal(attributes(out1$v2)$labels, c(alpha = 1, beta = 2))
  expect_equal(attributes(out1$v2)$na_values, 3)
  expect_equal(as.numeric(out1[["v2"]]), c(1, 2, 3))
})

test_that("Missing tagged and labeled character values and transform value labels", {
  expect_warning(out1 <- char_valLabels2numeric.savDat(string_combi_df, labeledStrings = "transform"),
                 "Some values with value labels or missing tags of variable v2 cannot be coerced to numeric. Therefore all underlying values are recoded to numeric.")
  expect_equal(attributes(out1$v1), attributes(string_combi_df$v1))
  expect_equal(attributes(out1$v2)$labels, c(alpha = 1, beta = 2, gamma = 3))
  expect_equal(attributes(out1$v2)$na_values, 4)
  expect_equal(as.numeric(out1[["v2"]]), c(1, 2, 4, 3))
})

#
# test_that("Missing codes for string values ", {
#   expect_warning(extract_Miss_SPSS(string_miss, "x", label_df = string_miss_labs, labeledStrings = FALSE),
#                  "Some or all missing codes for x cannot be coerced to numeric and are therefore changed to NA.")
#   expect_equal(suppressWarnings(extract_Miss_SPSS(string_miss, "x", label_df = string_miss_labs, labeledStrings = FALSE))[, "value"], c(2, 0, NA))
#   expect_equal(extract_Miss_SPSS(string_miss, "x", label_df = string_miss_labs, labeledStrings = TRUE)[, "value"], c(2, 0, "a"))
# })
