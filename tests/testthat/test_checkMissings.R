
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

### check missings
df4 <- df3 <- df1
# additional missing and valid values
df3 <- changeMissings(df1, varName = "ID1", value = 2, missings = "miss")
df4 <- changeMissings(df1, varName = "V1", value = c(3, -9, -99, -98), missings = c("valid", "miss", "valid", "miss"))
df4 <- changeValLabels(df4, varName = "V1", value = c(3, -9, -99, -98), valLabel = c("missing", "missing by intention",
                                                                                     "missing by design", "not reached"))

test_that("Missing checks input validation", {
  expect_error(checkMissings(df4, missingLabel = 1),
               "'missingLabel' needs to be a character vector of length 1.")
  expect_error(checkMissings(df4, missingLabel = NA_character_),
               "'missingLabel' is NA.")
  expect_error(checkMissings(df4, missingLabel = ""),
               "'missingLabel' is an empty string.")
  expect_error(checkMissingsByValues(df4, missingValues = c(1, NA)),
               "'missingValues' contains NAs.")
  expect_error(checkMissingsByValues(df4, missingValues = numeric()),
               "'missingValues' needs to be a numeric vector of at least length 1.")
})


test_that("Missing checks raise no false alarms", {
  expect_equal(df1, checkMissings(df1))
  expect_equal(df1, checkMissingsByValues(df1))
})

test_that("checkMissins: Missing labels are correctly checked and added", {
  expect_message(checkMissings(df3),
                 "The following variables have values coded as missings but value labels do not include the term 'missing':
ID1")
  all_messages <- capture_messages(checkMissings(df3, addMissingLabel = TRUE))
  expect_equal(all_messages[2],
               "'generic missing' is inserted into column valLabel for 1 rows.\n")
  expect_equal(checkMissings(df3, addMissingLabel = TRUE)$labels[1, "valLabel"], "generic missing")
})

test_that("checkMissings: Missing codes are correctly checked and added", {
  expect_message(checkMissings(df4, addMissingCode = FALSE),
                 "The following variables have value labels including the term 'missing' which are not coded as missing:\nV1")
  all_messages <- capture_messages(out <- checkMissings(df4))
  expect_equal(all_messages[2], "'miss' is inserted into column missings for 2 rows.\n")
  expect_equal(out$labels[2, "missings"], "miss")
  expect_equal(out$labels[3, "missings"], "miss")
})

test_that("checkMissingsByValues: Missing codes are correctly checked and added", {
  all_messages <- capture_messages(out <- checkMissingsByValues(df4, missingValues = -50:-99))
  expect_equal(all_messages[1], "The following variables have values in the 'missingValues' range which are not coded as missing:\nV1\n")
  expect_equal(all_messages[2], "'miss' is inserted into column missings for 1 rows.\n")
  expect_equal(all_messages[3], "The following variables have values coded as missings which are outside of the specified 'missingValues' range:\nV1\n")
  expect_equal(out$labels[2:5, "missings"], c("miss", "miss", "miss", "valid"))

  expect_silent(out2 <- checkMissingsByValues(out, missingValues = -5:-99))
})
