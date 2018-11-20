
context("Data Cleaning")

# load test data (df1, df2, pkList, fkList)
# load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_data.rda")
load(file = "helper_data.rda")


### check missings
df4 <- df3 <- df1
df3$labels[1, "missings"] <- "miss"
df4$labels[2, "valLabel"] <- "missing"


test_that("Missing checks raise no false alarms", {
  expect_equal(df1, checkMissings(df1))
  expect_equal(df2, checkMissings(df2, "mis"))
})

test_that("Missing labels are correctly checked and added", {
  expect_message(checkMissings(df3, addMissingLabel = FALSE),
                 "The following variables have values coded as missing but value label does not include the term 'missing':\nID1")
  all_messages <- capture_messages(checkMissings(df3))
  expect_equal(all_messages[2],
               "'generic missing' is inserted into column valLabel for 1 rows.\n")
  expect_equal(checkMissings(df3)$labels[1, "valLabel"], "generic missing")
})

test_that("Missing codes are correctly checked and added", {
  expect_message(checkMissings(df4, addMissingCode = FALSE),
                 "The following variables have value labels including the term 'missing' which are not coded as missing:\nV1")
  all_messages <- capture_messages(checkMissings(df4))
  expect_equal(all_messages[2],
               "'miss' is inserted into column missings for 1 rows.\n")
  expect_equal(checkMissings(df4)$labels[2, "missings"], "miss")
})
