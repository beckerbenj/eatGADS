
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

test_that("Errors", {
  expect_error(composeVar(df1, sourceVars = "ID1", newVar = "newVar", primarySourceVar = "ID1"),
               "'sourceVars' must be a character vector of length 2.")
})
