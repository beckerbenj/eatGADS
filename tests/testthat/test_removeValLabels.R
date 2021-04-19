
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

  out2 <- removeValLabels(dfSAV, varName = "VAR2", value = -99)
  expect_equal(nrow(out2$labels[out2$labels$varName == "VAR2", ]), 1)
  expect_equal(out2$labels[out2$labels$varName == "VAR2", "value"], c(-96))


  out <- removeValLabels(dfSAV, varName = "VAR1", value = c(-96, -99))
  expect_equal(nrow(out$labels[out$labels$varName == "VAR1", ]), 1)
  expect_equal(out$labels[out$labels$varName == "VAR1", "value"], c(1))
})

test_that("removeValLabels all labels of a variable one by one", {
  out <- removeValLabels(dfSAV, varName = "VAR3", value = -99)
  out2 <- removeValLabels(out, varName = "VAR3", value = -98)

  expect_equal(extractMeta(out2, "VAR1"), extractMeta(dfSAV, "VAR1"))
  expect_equal(extractMeta(out2, "VAR2"), extractMeta(dfSAV, "VAR2"))
  meta_out <- extractMeta(out2, "VAR3")
  expect_equal(nrow(meta_out), 1)
  expect_equal(meta_out$value, NA_real_)
  expect_equal(meta_out$valLabel, NA_character_)
  expect_equal(meta_out$labeled, "no")
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

test_that("removeValLabels with matching valLabels", {
  expect_error(removeValLabels(dfSAV, varName = "VAR1", value = 1:2, valLabel = "One"),
               "'value' and 'valLabel' need to be of identical length.")

  out <- removeValLabels(dfSAV, varName = "VAR1", value = 1, valLabel = "One")
  expect_equal(nrow(out$labels[out$labels$varName == "VAR1", ]), 2)
  expect_equal(out$labels[out$labels$varName == "VAR1", "value"], c(-99, -96))

  out2 <- removeValLabels(dfSAV, varName = "VAR1", value = 1, valLabel = "ne")
  expect_equal(nrow(out2$labels[out2$labels$varName == "VAR1", ]), 2)
  expect_equal(out2$labels[out2$labels$varName == "VAR1", "value"], c(-99, -96))

  out3 <- removeValLabels(dfSAV, varName = "VAR1", value = c(-96, -99), valLabel = c("Om", "design"))
  expect_equal(nrow(out3$labels[out3$labels$varName == "VAR1", ]), 1)
  expect_equal(out3$labels[out3$labels$varName == "VAR1", "value"], c(1))

  out4 <- removeValLabels(dfSAV, varName = "VAR1", value = c(-96, -99), valLabel = c("Om", "other"))
  expect_equal(nrow(out4$labels[out4$labels$varName == "VAR1", ]), 2)
  expect_equal(out4$labels[out4$labels$varName == "VAR1", "value"], c(-99, 1))
})
