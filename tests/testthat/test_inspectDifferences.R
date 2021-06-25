# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

test_that("Errors",{
  df1_5 <- df1_4 <- df1_2 <- df1_3 <- df1
  expect_error(inspectDifferences(c("1", "2"), df1, df1_2, id = "ID1"),
               "'varName' must be a character of length 1.")
  expect_error(inspectDifferences("V1", df1, df1_2, id = 1),
               "'id' must be a character of length 1.")
  expect_error(inspectDifferences("V2", df1, df1_2, id = "ID1"),
               "'varName' is not a variable in 'GADSdat1'.")
  df1_2$dat <- df1_2$dat[c(1, 1, 2), ]
  expect_error(inspectDifferences("V1", df1, df1_2, id = "ID1"),
               "'GADSdat1' and 'GADSdat2' have different row numbers.")
  df1_3$dat$ID1 <- c(2, 1)
  expect_error(inspectDifferences("V1", df1, df1_3, id = "ID1"),
               "'id' column is not equal for 'GADSdat1' and 'GADSdat2'.")
  df1_4$dat[1, 1] <- NA
  expect_error(inspectDifferences("V1", df1, df1_4, id = "ID1"),
               "Missing values in 'id' column of 'GADSdat2'.")
  expect_error(inspectDifferences("V1", df1_4, df1, id = "ID1"),
               "Missing values in 'id' column of 'GADSdat1'.")
  df1_5$dat[, 2] <- as.character(df1_5$dat[, 1])
  expect_error(inspectDifferences("V1", df1, df1_5, id = "ID1"),
               "'varName' column is numeric in 'GADSdat1' but not in 'GADSdat2'.")
  expect_error(inspectDifferences("V1", df1_5, df1, id = "ID1"),
               "'varName' column is numeric in 'GADSdat2' but not in 'GADSdat1'.")
})

test_that("Compare two different GADSdat objects",{
  df1_2 <- df1
  df1_2$dat[1, 2] <- 9
  out <- inspectDifferences("V1", df1, df1_2, id = "ID1")
  expect_equal(names(out), c("cross_table", "some_unequals_GADSdat1", "some_unequals_GADSdat2", "unequal_IDs"))
  expect_equal(out$unequal_IDs, 1)
  expect_equal(out$some_unequals_GADSdat1, df1$dat[1, ])
  expect_equal(out$some_unequals_GADSdat2, df1_2$dat[1, ])
  expect_equal(out$cross_table, table(df1$dat$V1, df1_2$dat$V1, dnn = c("GADSdat1", "GADSdat2")))
})

test_that("Compare two identical GADSdat objects",{
  out <- inspectDifferences("V1", df1, df1, id = "ID1")
  expect_equal(out, "all.equal")
})

test_that("Differences with NA",{
  df1_3 <- df1_2 <- df1
  df1_2$dat[1, 2] <- NA
  out <- inspectDifferences("V1", df1, df1_2, id = "ID1")
  expect_equal(out$unequal_IDs, 1)
  expect_equal(out$some_unequals_GADSdat1, df1$dat[1, ])
  expect_equal(out$some_unequals_GADSdat2, df1_2$dat[1, ])
  expect_equal(out$cross_table, table(df1$dat$V1, df1_2$dat$V1, useNA = "if", dnn = c("GADSdat1", "GADSdat2")))

  df1_3$dat[1:2, 1] <- NA
  out <- inspectDifferences("ID1", df1_3, df1, id = "V1")
  expect_equal(out$unequal_IDs, c(3, 5))
  expect_equal(out$some_unequals_GADSdat1, df1_3$dat[1:2, ])
  expect_equal(out$some_unequals_GADSdat2, df1$dat[1:2, ])
  expect_equal(out$cross_table, table(df1_3$dat$ID1, df1$dat$ID1, useNA = "if", dnn = c("GADSdat1", "GADSdat2")))
})
