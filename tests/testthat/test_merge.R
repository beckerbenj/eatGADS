
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")


test_that("Merge two GADSdat objects",{
  df3 <- df1
  mod_dat <- df3$dat
  mod_dat[, "v3"] <- c(8, 7)
  df3 <- suppressMessages(updateMeta(df3, mod_dat))
  expect_error(merge(df1, df3, by = 2))
  expect_error(merge(df1, df3, by = "x2"))
  expect_error(merge(df1, df1, by = "V1"))
  out <- merge(df1, df3, by = "V1")
  expect_equal(out$dat[, 3], c(8, 7))
  expect_equal(nrow(out$labels), 3)
})

test_that("Inner join two GADSdat objects", {
  df3 <- df1
  mod_dat <- df3$dat
  mod_dat[, "v3"] <- c(8, 7)
  df3 <- suppressMessages(updateMeta(df3, mod_dat))
  df3$dat[3, ] <- c(1, 2, 5)

  out <- merge(df1, df3, by = "V1", all = F)
  expect_equal(dim(out$dat), c(2, 3))

  out2 <- merge(df1, df3, by = "V1", all = T)
  expect_equal(dim(out2$dat), c(3, 3))
})
