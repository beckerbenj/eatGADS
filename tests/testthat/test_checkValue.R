
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

df3 <- df2
df3$dat[1, 1:2] <- 8

test_that("Input validation", {
  expect_error(checkValue(df1, value = 3:4),
               "'value' needs to be of length 1.")
  expect_error(checkValue(df1, vars = 1, value = 3),
               "'vars' needs to be a character of at least length 1.", )
  expect_error(checkValue(df1, vars = "lala", value = 3),
               "The following 'vars' are not variables in the GADSdat: lala")
})

test_that("Value checks raise no false alarms", {
  expect_equal(checkValue(df1, value = 4), integer(0))
  expect_equal(checkValue(df2, value = -1), integer(0))
})

test_that("Value occurences reported", {
  expect_equal(checkValue(df1, value = 1),
                 c(ID1 = 1L))
  expect_equal(checkValue(df3, value = 8),
                 c(ID1 = 1L, V2 = 2L))
})

test_that("Value checks for variable subset", {
  expect_equal(checkValue(df1, vars = "V1", value = 1),
               integer())
  expect_equal(checkValue(df3, vars = "V2", value = 8),
               c(V2 = 2))
})

test_that("Value checks for NA", {
  df5 <- df1
  df5$dat[1:2, "V1"] <- NA
  expect_equal(checkValue(df5, value = NA),
               c(V1 = 2L))
  expect_equal(checkValue(df1, value = NA),
               integer())
})
