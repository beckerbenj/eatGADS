###### test import from eatTools convertLabel
# convertLabel_df <- readRDS(file = "tests/testthat/helper_convertLabel.RDS")
convertLabel_df <- readRDS(file = "helper_convertLabel.RDS")

test_that("Data frames from eatTools convertLabel imported correctly", {
  out <- import_convertLabel(convertLabel_df)
  expect_equal(extractMeta(out, "year")[["varLabel"]], "year of assessment")
  expect_equal(extractMeta(out, "mig")[["varLabel"]], NA_character_)
  expect_equal(extractMeta(out, "sex")[["valLabel"]], c("female", "male"))

  expect_equal(dim(out$dat), c(5, 22))
})
