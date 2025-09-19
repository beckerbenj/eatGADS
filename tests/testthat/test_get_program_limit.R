test_that("Returns a list of 1 numeric and 1 character", {
  output <- get_program_limit("Stata", "varname")
  # list level
  expect_true(is.list(output))
  expect_equal(length(output), 2)
  # component level
  expect_true(is.numeric(output$x))
  expect_true(is.character(output$unit))
  expect_equal(length(output$x), 1)
  expect_equal(length(output$unit), 1)
})
