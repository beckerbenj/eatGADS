
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

### check missings
df4 <- df3 <- df1
df3$labels[1, "missings"] <- "miss"
df4$labels[2, "valLabel"] <- "missing"
df4$labels[3, ] <- df4$labels[2, ]
# wrong valid value
df4$labels[3, "value"] <- -99
df4$labels[3, "valLabel"] <- "missing by design"
df4$labels[3, "missings"] <- "valid"
df4$labels[2:3, "labeled"] <- "yes"

test_that("Missing checks raise no false alarms", {
  expect_equal(df1, checkMissings(df1))
  expect_equal(df2, checkMissings(df2, "mis"))
})

test_that("Missing labels are correctly checked and added", {
  expect_message(checkMissings(df3, addMissingLabel = FALSE),
                 "The following variables have values coded as missings but value labels do not include the term 'missing':
ID1")
  expect_message(checkMissings(df3),
                 "The following variables have values coded as missings but value labels do not include the term 'missing':
ID1")
  all_messages <- capture_messages(checkMissings(df3, addMissingLabel = TRUE))
  expect_equal(all_messages[2],
               "'generic missing' is inserted into column valLabel for 1 rows.\n")
  expect_equal(checkMissings(df3, addMissingLabel = TRUE)$labels[1, "valLabel"], "generic missing")
})

test_that("Missing codes are correctly checked and added", {
  expect_message(checkMissings(df4, addMissingCode = FALSE),
                 "The following variables have value labels including the term 'missing' which are not coded as missing:\nV1")
  all_messages <- capture_messages(checkMissings(df4))
  expect_equal(all_messages[2],
               "'miss' is inserted into column missings for 2 rows.\n")
  expect_equal(checkMissings(df4)$labels[2, "missings"], "miss")
  expect_equal(checkMissings(df4)$labels[3, "missings"], "miss")
})

