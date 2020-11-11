
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

dfUn <- import_DF(data.frame(v1 = 1, v2 = 2))

test_that("Errors", {
  expect_error(removeValLabels(dfSAV, varName = "VAR5", value = 2),
               "'varName' is not a variable name in the GADSdat.")
  expect_error(removeValLabels(dfSAV, varName = c("VAR1", "VAR3"), value = 2),
               "'varName' is not a character vector of length 1.")

})


test_that("removeValLabels", {
  out <- removeValLabels(dfSAV, varName = "VAR1", value = 1)
  expect_equal(nrow(out$labels[out$labels$varName == "VAR1", ]), 2)
  expect_equal(out$labels[out$labels$varName == "VAR1", "value"], c(-99, -96))

  out <- removeValLabels(dfSAV, varName = "VAR1", value = c(-96, -99))
  expect_equal(nrow(out$labels[out$labels$varName == "VAR1", ]), 1)
  expect_equal(out$labels[out$labels$varName == "VAR1", "value"], c(1))
})

test_that("no valid values", {
  expect_warning(out <- removeValLabels(dfSAV, varName = "VAR1", value = 3),
                 "None of 'value' are labeled 'values'. Meta data are unchanged.")
  expect_equal(nrow(out$labels[out$labels$varName == "VAR1", ]), 3)
  expect_equal(out$labels[out$labels$varName == "VAR1", "value"], c(-99, -96, 1))

  expect_silent(out2 <- removeValLabels(dfSAV, varName = "VAR1", value = c(1, 3)))
  expect_equal(nrow(out2$labels[out2$labels$varName == "VAR1", ]), 2)
  expect_equal(out2$labels[out2$labels$varName == "VAR1", "value"], c(-99, -96))
})
