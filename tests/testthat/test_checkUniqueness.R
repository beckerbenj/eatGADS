
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

### check missings
df1b <- df1
df1b$dat <- rbind(df1$dat, df1$dat)

df5 <- df4 <- df3 <- df1b
df3$dat[, "V1"] <- c(4, 1, 3, 1)

test_that("Errors", {
  expect_error(checkUniqueness(df1, varName = "V1", idVar = "ID1"),
               "'idVar' is unique per row in 'GADSdat' and checking for uniqueness is obsolete.")
})

test_that("No flagging", {
  out <- checkUniqueness(df1b, varName = "V1", idVar = "ID1")
  expect_true(out)
})

test_that("Correct flagging and output", {
  out <- checkUniqueness(df3, varName = "V1", idVar = "ID1")
  comp <- df3$dat[c(1, 3), ]
  row.names(comp) <- NULL
  expect_equal(out, comp)
})
