test_that("Inputs are correctly validated", {
  expect_error(get_program_limit("Stata 17", "varNames"))
  expect_error(get_program_limit("Stata", "varNames", version = "Stata 19/SE"))
  expect_error(get_program_limit("Stata", c("varNames", "ncols")))
})

test_that("Returns a list of 1 numeric and 1 character", {
  output <- get_program_limit("Stata", "varNames")
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

  result_spss <- get_program_limit("SPSS", "varNames")
  result_stata <- get_program_limit("Stata", "varNames")
  result_joint <- get_program_limit(c("SPSS", "Stata"), "varNames")

  expect_equal(result_spss, expect_spss)
  expect_equal(result_stata, expect_stata)
  expect_equal(result_joint, expect_stata)
})

test_that("Complete output of all limits", {
  output <- get_program_limit()
  expect_equal(dim(output$x), c(6, 2))
  expect_equal(dim(output$x), dim(output$unit))
})

test_that("Return limits of specific version", {
  stata_default <- get_program_limit("Stata", "ncols")
  stata_mp <- get_program_limit("Stata", "ncols", version = "Stata 19/MP")
  expect_lt(stata_default$x, stata_mp$x)
})
