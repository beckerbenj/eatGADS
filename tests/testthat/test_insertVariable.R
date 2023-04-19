

test_that("insert variable in different order", {
  expect_error(insertVariable(dfSAV, var = "VAR3", after = "VAR1"),
               "'insertVariable()' has been deprecated, please use 'relocateVariable()' instead.", fixed = TRUE)
})

