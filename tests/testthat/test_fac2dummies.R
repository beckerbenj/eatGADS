
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")


test_that("errors", {
  dfSAV2 <- changeValLabels(dfSAV, varName = "VAR1", value = 2, valLabel = "Two")

  expect_error(fac2dummies(dfSAV2, var = "VAR5"),
               "The following 'vars' are not variables in the GADSdat: VAR5")
  expect_error(fac2dummies(dfSAV2, var = 1),
               "'var' needs to be a character vector of length 1.")
})


test_that("factor 2 dummies", {
  dfSAV2 <- changeValLabels(dfSAV, varName = "VAR1", value = 2, valLabel = "Two")
  out <- fac2dummies(dfSAV2, var = "VAR1")
  expect_equal(dim(out$labels), c(16, 8))
  expect_equal(out$labels$varName[9:12], rep("VAR1_a", 4))
  expect_equal(out$labels$varName[13:16], rep("VAR1_b", 4))
  expect_equal(out$labels$varLabel[9:12], rep("Variable 1: One", 4))
  expect_equal(out$labels$varLabel[13:16], rep("Variable 1: Two", 4))
  expect_equal(out$labels$value[9:12], c(-99, -96, 0, 1))
  expect_equal(out$labels$value[13:16], c(-99, -96, 0, 1))
  expect_equal(out$labels$missings[9:12], c("miss", "miss", NA, NA))
  expect_equal(out$labels$missings[13:16], c("miss", "miss", NA, NA))
})

test_that("factor 2 dummies no varLabel", {
  iris2 <- import_DF(iris)
  out <- fac2dummies(iris2, var = "Species")
  expect_equal(dim(out$labels), c(13, 8))
  expect_equal(out$labels$varName[8:9], rep("Species_a", 2))
  expect_equal(out$labels$varName[10:11], rep("Species_b", 2))
  expect_equal(out$labels$varName[12:13], rep("Species_c", 2))
  expect_equal(out$labels$varLabel[8:9], rep("Species: setosa", 2))
  expect_equal(out$labels$varLabel[10:11], rep("Species: versicolor", 2))
  expect_equal(out$labels$varLabel[12:13], rep("Species: virginica", 2))
})
