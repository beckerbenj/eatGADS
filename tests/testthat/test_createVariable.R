
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

test_that("Errors", {
  expect_error(createVariable(dfSAV, varName = "VAR2"),
               "'VAR2' is already an existing variable in the 'GADSdat'.")
})

test_that("Create a variable", {
  out <- createVariable(dfSAV, varName = "VAR4")
  expect_equal(namesGADS(out), c("VAR1", "VAR2", "VAR3", "VAR4"))
  expect_equal(out$dat$VAR4, rep(NA, 4))
})

test_that("Create a variable with invalid variable name", {
  expect_message(out <- createVariable(dfSAV, varName = "var.1"),
                 "var.1 has been renamed to var_1")
  expect_equal(namesGADS(out), c("VAR1", "VAR2", "VAR3", "var_1"))
  expect_equal(out$dat$var_1, rep(NA, 4))

  out2 <- createVariable(dfSAV, varName = "var.1", checkVarName = FALSE)
  expect_equal(namesGADS(out2), c("VAR1", "VAR2", "VAR3", "var.1"))
})
