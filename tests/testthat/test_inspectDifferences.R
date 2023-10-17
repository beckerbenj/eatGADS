# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

test_that("Errors",{
  df1_5 <- df1_4 <- df1_2 <- df1_3 <- df1
  expect_error(inspectDifferences(df1, varName = c("1", "2"), id = "ID1"),
               "'varName' needs to be a character vector of length 1.")
  expect_error(inspectDifferences(df1, varName = "V1", id = 1),
               "'id' needs to be a character vector of length 1.")
  expect_error(inspectDifferences(df1, varName = "V2", id = "ID1"),
               "The following 'varName' are not variables in the GADSdat: V2")
  df1_2$dat <- df1_2$dat[c(1, 1, 2), ]
  expect_error(inspectDifferences(df1, varName = "V1", other_GADSdat = df1_2, id = "ID1"),
               "'GADSdat' and 'other_GADSdat' have different row numbers.")
  df1_3$dat$ID1 <- c(2, 1)
  expect_error(inspectDifferences(df1, varName = "V1", other_GADSdat = df1_3, id = "ID1"),
               "'id' column is not equal for 'GADSdat' and 'other_GADSdat'.")
  df1_4$dat[1, 1] <- NA
  expect_error(inspectDifferences(df1, varName = "V1", other_GADSdat = df1_4, id = "ID1"),
               "Missing values in 'id' column of 'other_GADSdat'.")
  expect_error(inspectDifferences(df1_4, varName = "V1", id = "ID1"),
               "Missing values in 'id' column of 'GADSdat'.")
  df1_5$dat[, 2] <- as.character(df1_5$dat[, 1])
  expect_error(inspectDifferences(df1, varName = "V1", other_GADSdat = df1_5, id = "ID1"),
               "'varName' column is numeric in 'GADSdat' but 'other_varName' is not numeric in 'other_GADSdat'.")
  expect_error(inspectDifferences(df1_5, varName = "V1", other_GADSdat = df1, id = "ID1"),
               "'other_varName' column is numeric in 'other_GADSdat' but 'varName' is not numeric in 'GADSdat'.")
  expect_error(inspectDifferences(df1_5, varName = "V1", other_GADSdat = df1, other_varName = "ID1", id = "ID1"),
               "'other_varName' column is numeric in 'other_GADSdat' but 'varName' is not numeric in 'GADSdat'.")
})

test_that("Compare two different GADSdat objects but the same variable",{
  df1_2 <- df1
  df1_2$dat[1, 2] <- 9
  out <- inspectDifferences(df1, varName = "V1", other_GADSdat = df1_2, id = "ID1")
  expect_equal(names(out), c("cross_table", "unequal_IDs"))
  expect_equal(out$unequal_IDs, 1)
  expect_equal(out$cross_table, table(df1$dat$V1, df1_2$dat$V1, dnn = c("GADSdat", "other_GADSdat")))
})

test_that("Compare two different GADSdat objects and different variables",{
  df1_2 <- df1
  df1_2$dat[1, 2] <- 9
  out <- inspectDifferences(df1, varName = "ID1", other_GADSdat = df1_2, other_varName = "V1", id = "ID1")
  expect_equal(out$unequal_IDs, 1:2)
  expect_equal(out$cross_table, table(df1$dat$ID1, df1_2$dat$V1, dnn = c("ID1", "V1")))
})


test_that("Compare two identical GADSdat objects",{
  out <- inspectDifferences(df1, varName = "V1", id = "ID1")
  expect_equal(out, "all.equal")
})


## could potentially be implemented somewhen (see also equalGADS() for similar functionality)
#test_that("Compare two identical GADSdat objects with different ordering",{
#  pisa2 <- pisa
#  pisa2$dat <- pisa2$dat[nrow(pisa2$dat):1, ]
#  out <- inspectDifferences(pisa, varName = "sameteach", other_GADSdat = pisa2, id = "idstud")
#  expect_equal(out, "all.equal")
#})


test_that("Differences with NA",{
  df1_3 <- df1_2 <- df1
  df1_2$dat[1, 2] <- NA
  out <- inspectDifferences(df1, varName = "V1", other_GADSdat = df1_2, id = "ID1")
  expect_equal(out$unequal_IDs, 1)
  expect_equal(out$cross_table,
               table(df1$dat$V1, df1_2$dat$V1, useNA = "if", dnn = c("GADSdat", "other_GADSdat")))

  df1_3$dat[1:2, 1] <- NA
  out <- inspectDifferences(df1_3, varName = "ID1", other_GADSdat = df1, id = "V1")
  expect_equal(out$unequal_IDs, c(3, 5))
  expect_equal(out$cross_table,
               table(df1_3$dat$ID1, df1$dat$ID1, useNA = "if", dnn = c("GADSdat", "other_GADSdat")))
})
