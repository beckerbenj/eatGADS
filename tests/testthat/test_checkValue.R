
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

df3 <- df2
df3$dat[1, 1] <- 8

test_that("Value checks raise no false alarms", {
  expect_silent(checkValue(df1, value = 4))
  expect_silent(checkValue(df2, value = -1))
})

test_that("Value occurences reported", {
  expect_message(checkValue(df1, value = 1),
                 "The following variables have occurences of 'value' in the GADSdat:
ID1")
  expect_message(checkValue(df3, value = 8),
                 "The following variables have occurences of 'value' in the GADSdat:
ID1, V2")
})
