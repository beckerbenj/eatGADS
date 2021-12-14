
test_that("checkLEStructure", {
  # checkLEStructure(filePaths = c("tests/testthat/helper_comp.db", "tests/testthat/helper_comp.db"), lePath = "tests/testthat/helper_le.db")
  # out <- checkLEStructure(filePaths = c("tests/testthat/helper_comp.db", "tests/testthat/helper_comp.db"), lePath = "tests/testthat/helper_le_wrong.db")
  expect_silent(checkLEStructure(filePaths = c("helper_comp.db", "helper_comp.db"), lePath = "helper_le.db"))
  le_mes <- capture_messages(out <- checkLEStructure(filePaths = c("helper_comp.db", "helper_comp.db"), lePath = "helper_le_wrong.db"))
  expect_equal(le_mes[[1]], "The following variables have linking errors but are not variables in data base 1: other\n")
  expect_equal(le_mes[[2]], "The following variables have linking errors but are not variables in data base 2: other\n")
  expect_equal(le_mes[[3]], "The linking error data base contains variables other than linking errors and key variables.\n")
  expect_equal(le_mes[[4]], "The following variables are key variables in the Linking Error data base but are not variables in data base 1: test_pk\n")
  expect_equal(le_mes[[5]], "The following variables are key variables in the Linking Error data base but are not variables in data base 2: test_pk\n")
  expect_equal(out$dep_notIn_nam[[1]], "other")
  expect_equal(out$dep_notIn_nam[[2]], "other")
  expect_equal(out$key_notIn_nam[[1]], "test_pk")
  expect_equal(out$key_notIn_nam[[2]], "test_pk")
})


fp1 <- system.file("extdata", "trend_gads_2020.db", package = "eatGADS")
fp2 <- system.file("extdata", "trend_gads_2015.db", package = "eatGADS")
fp3 <- system.file("extdata", "trend_gads_2010.db", package = "eatGADS")
lep <- system.file("extdata", "gads_LEs.db", package = "eatGADS")

test_that("3 mp trend", {
  le_mes <- capture_messages(out <- checkLEStructure(filePaths = c(fp1, fp2, fp3), lePath = lep))
  expect_equal(out$dep_notIn_nam[[3]], "transfBista")
  #expect_equal(out$key_notIn_nam[[3]], "parameter")
})
