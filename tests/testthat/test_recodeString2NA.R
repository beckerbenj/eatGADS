
test_that("Error for Recodestring2NA", {
  expect_error(out <- recodeString2NA(1), "This function is deprecated. Use recode2NA() instead.", fixed = TRUE)
})


