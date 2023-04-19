

# dfSAV <- import_spss(file = "tests/testthat/helper_spss.sav")
dfSAV <- import_spss(file = "helper_spss.sav")

test_that("emptyTheseVariables for GADSdat", {
  out1 <- emptyTheseVariables(dfSAV, vars = c("VAR1", "VAR2"))
  expect_equal(out1$dat$VAR1, rep(NA_real_, 1))
  expect_equal(out1$dat$VAR2, rep(NA_real_, 1))
  expect_equal(out1$dat$VAR3, dfSAV$dat$VAR3)
  out2 <- emptyTheseVariables(dfSAV, vars = "VAR3")
  expect_equal(out2$dat$VAR1, dfSAV$dat$VAR1)
  expect_equal(out2$dat$VAR2, dfSAV$dat$VAR2)
  expect_equal(out2$dat$VAR3, rep(NA_character_, 1))

  expect_silent(write_spss(out1, filePath = tempfile()))
  expect_silent(write_spss(out2, filePath = tempfile()))
})


