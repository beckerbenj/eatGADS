test_that("Inputs are correctly validated", {
  expect_warning(getProgramLimit(c("SPSS", "Stata 17"), "varNames"))
  expect_no_error(suppressWarnings(getProgramLimit(c("SPSS", "Stata 17"), "varNames")))
  expect_error(getProgramLimit("Stata", c("varNames", "ncols")))
})

test_that("Returns a list of 1 numeric and 1 character", {
  output <- getProgramLimit("Stata", "varNames")
  # list level
  expect_true(is.list(output))
  expect_equal(length(output), 2)
  # component level
  expect_true(is.numeric(output$x))
  expect_true(is.character(output$unit))
  expect_equal(length(output$x), 1)
  expect_equal(length(output$unit), 1)
})

test_that("Return most restrictive limit", {
  expect_spss <- list(x = 64, unit = "byte")
  expect_stata <- list(x = 32, unit = "char")

  result_spss <- getProgramLimit("SPSS", "varNames")
  result_stata <- getProgramLimit("Stata", "varNames")
  result_joint <- getProgramLimit(c("SPSS", "Stata"), "varNames")

  expect_equal(result_spss, expect_spss)
  expect_equal(result_stata, expect_stata)
  expect_equal(result_joint, expect_stata)
})

test_that("Return limits of specific version", {
  stata_default <- getProgramLimit("Stata", "ncols")
  stata_be <- getProgramLimit(c("Stata", "Stata 19/BE"), "ncols")
  expect_gt(stata_default$x, stata_be$x)
})
