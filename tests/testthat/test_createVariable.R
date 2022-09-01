
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

test_that("Errors", {
  expect_error(createVariable(dfSAV, varName = "VAR2"),
               "'VAR2' is already an existing variable in the 'GADSdat'.")
})

test_that("Createa variable", {
  out <- createVariable(dfSAV, varName = "VAR4")
  expect_equal(namesGADS(out), c("VAR1", "VAR2", "VAR3", "VAR4"))
  expect_equal(out$dat$VAR4, rep(NA, 4))
})

