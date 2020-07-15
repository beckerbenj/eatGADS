
# rawDat <- import_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss.sav")
rawDat <- import_spss("helper_spss.sav")

test_that("String as numeric", {
  expect_warning(stringAsNumeric(rawDat, varName = "VAR3"), "Some or all values for VAR3 cannot be coerced to numeric and are therefore changed to NA.")
  suppressWarnings(out <- stringAsNumeric(rawDat, varName = "VAR3"))
  expect_equal(out$dat$VAR3, NA_real_)
  expect_equal(out$labels[3, "format"], "F10")

  rawDat2 <- rawDat
  rawDat2$dat$VAR3 <- "1"
  out2 <- stringAsNumeric(rawDat2, varName = "VAR3")
  expect_equal(out2$dat$VAR3, 1)
  expect_equal(out2$labels[3, "format"], "F10")
})
