
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

df3 <- df2
df3$dat[1, 1:2] <- 8

test_that("Input validation", {
  expect_error(checkValue(df1, value = 3:4),
               "'value' needs to be of length 1.")
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
