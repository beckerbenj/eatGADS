
context("Handle data")

# load data with missings
# testM <- import_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav")
testM <- import_spss("helper_spss_missings.sav")

expected_v1 <- c(1, NA, NA, 2)

test_that("Missing Conversion Single Variable", {
  expect_equal(recodeVar(testM$dat$VAR1, testM$labels), expected_v1)
})

test_that("Missing Conversion Data frame", {
  expected <- data.frame(VAR1 = expected_v1)
  expect_equal(miss2NA(testM)[, 1, drop = F], expected)
})


