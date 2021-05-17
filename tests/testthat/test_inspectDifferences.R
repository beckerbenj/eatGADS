# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

test_that("Errors",{
  df1_2 <- df1_3 <- df1
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
