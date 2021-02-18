###### test import from R data frame with explicit meta information
df_raw <- data.frame(a = 1:2, b = 2:3)
varLabels_raw <- data.frame(varName = c("a", "b"), varLabel = c("variable a", "variable b"), stringsAsFactors = FALSE)
valLabels_raw <- data.frame(varName = c("a", "a", "b", "b"), value = c(1, 2, 2, 3), valLabel = c("one", "two", "very", "few"), missings = rep("valid", 4), stringsAsFactors = FALSE)

test_that("Checks for import_raw", {
  iris$Species <- as.factor(iris$Species)
  expect_error(import_raw(df = iris), "At least one of the variables in df is a factor. All meta information on value level has to be stored in valLabels.")
  varLabels_raw_fac <- data.frame(varName = c("a", "b"), varLabel = c("variable a", "variable b"), stringsAsFactors = TRUE)
  expect_error(import_raw(df = df_raw, varLabels = varLabels_raw_fac), "One of the variables in varLabels is a factor.")
  valLabels_raw <- data.frame(varName = c("a", "a", "b", "b"), value = c(1, 2, 2, 3), valLabel = c("one", "two", "very", "few"), missings = rep("valid", 4))
  expect_error(import_raw(df = df_raw, varLabels = varLabels_raw_fac, valLabels = valLabels_raw), "One of the variables in varLabels is a factor.")

  expect_error(import_raw(df = df_raw, mtcars), "varLabels needs to contain the variables 'varName' and 'varLabel'.")
  expect_error(import_raw(df = df_raw, varLabels_raw, mtcars), "valLabels needs to contain the variables 'varName', 'value', 'valLabel' and 'missings'.")

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
  expect_equal(out$labels$labeled, rep("yes", 4))

  df <- data.frame(ID = 1:4, sex = c(0, 0, 1, 1), forename = c("Tim", "Bill", "Ann", "Chris"), stringsAsFactors = FALSE)
  varLabels <- data.frame(varName = c("ID", "sex", "forename"), varLabel = c("Person Identifier", "Sex as self reported", "forename provided by teacher"), stringsAsFactors = FALSE)
  valLabels <- data.frame(varName = rep("sex", 3), value = c(0, 1, -99), valLabel = c("male", "female", "missing - omission"), missings = c("valid", "valid", "miss"), stringsAsFactors = FALSE)

  out2 <- import_raw(df = df, varLabels = varLabels, valLabels = valLabels)
  expect_equal(out2$labels$value, c(NA, -99, 0, 1, NA))
  expect_equal(out2$labels$valLabel, c(NA, "missing - omission", "male", "female", NA))
  expect_equal(out2$labels$labeled, c("no", "yes", "yes", "yes", "no"))
})

test_that("import_raw with tibbles", {
  out1 <- import_raw(df = df_raw, varLabels = varLabels_raw, valLabels = valLabels_raw)
  df_raw <- tibble::as_tibble(df_raw)
  varLabels_raw <- tibble::as_tibble(varLabels_raw)
  valLabels_raw <- tibble::as_tibble(valLabels_raw)
  out2 <- import_raw(df = df_raw, varLabels = varLabels_raw, valLabels = valLabels_raw)
  expect_equal(out1, out2)
})
