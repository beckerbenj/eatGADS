
# load test data (df1, df2, pkList, fkList)
load(file = test_path("helper_data.rda"))

mod_dat <- data.frame(ID1 = c(1, 3, 4), V2 = c(8, 10, 9))
df3 <- suppressMessages(updateMeta(df1, mod_dat))

# data with existing NAs
df1b <- df1
df1b$dat[1, "V1"] <- NA
df3b <- df3
df3b$dat[2:3, "V2"] <- NA

test_that("Errors for merge", {
  expect_error(merge(df1, df3, by = 2))
  expect_error(merge(df1, df3, by = "x2"))
  expect_error(merge(df1, df1, by = "V1"),
               "y does not contain variables distinct from x.")
})

test_that("Merge two GADSdat objects",{
  out <- merge(df1, df3, by = "ID1")
  expect_equal(out$dat[, "V1"], c(3, 5, NA, NA))
  expect_equal(out$dat[, "V2"], c(8, NA, 10, 9))
  expect_equal(nrow(out$labels), 3)
})

test_that("Merge two GADSdat objects while setting default missing value",{
  out <- merge(df1, df3, by = "ID1", missingValue = -99, missingValLabel = "missing")
  expect_equal(out$dat[, "V1"], c(3, 5, -99, -99))
  expect_equal(out$dat[, "V2"], c(8, -99, 10, 9))
  expect_equal(nrow(out$labels), 3)

  # with existing NAs
  out2 <- merge(df1b, df3b, by = "ID1", missingValue = -99, missingValLabel = "missing")
  expect_equal(out2$dat[, "V1"], c(NA, 5, -99, -99))
  expect_equal(out2$dat[, "V2"], c(8, -99, NA, NA))
  expect_equal(nrow(out2$labels), 3)
  expect_equal(out2$labels[2:3, "value"], c(-99, -99))
  expect_equal(out2$labels[2:3, "valLabel"], c("missing", "missing"))
  expect_equal(out2$labels[2:3, "missings"], c("miss", "miss"))

  # with existing NAs
  out3 <- merge(df1b, df3b, by = "ID1", missingValue = -99)
  expect_equal(out3$dat[, "V1"], c(NA, 5, -99, -99))
  expect_equal(out3$dat[, "V2"], c(8, -99, NA, NA))
  expect_equal(nrow(out3$labels), 3)
  expect_equal(out3$labels[2:3, "value"], c(-99, -99))
  expect_equal(out3$labels[2:3, "valLabel"], c(NA_character_, NA))
  expect_equal(out3$labels[2:3, "missings"], c("miss", "miss"))
})

test_that("Merge two GADSdat objects while setting default missing value and the default substitution value exists in the data",{
  df1c <- df1b
  df1c$dat[2, "V1"] <- -99.9989
  out <- merge(df1c, df3b, by = "ID1", missingValue = -99, missingValLabel = "missing")
  expect_equal(out$dat[, "V1"], c(NA, -99.9989, -99, -99))
  expect_equal(out$dat[, "V2"], c(8, -99, NA, NA))
  expect_equal(nrow(out$labels), 3)
  expect_equal(out$labels[2:3, "missings"], c("miss", "miss"))
})

test_that("Inner join two GADSdat objects", {
  out <- merge(df1, df3, by = "ID1", all = FALSE)
  expect_equal(dim(out$dat), c(1, 3))
})
