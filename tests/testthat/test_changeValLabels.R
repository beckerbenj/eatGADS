
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

dfUn <- import_DF(data.frame(v1 = 1, v2 = 2))

test_that("changevallabel wrapper", {
  out <- changeValLabels(dfSAV, varName = "VAR1", value = 1, valLabel = "test label")
  expect_equal(out$labels[3, "valLabel"], "test label")
  expect_equal(out$dat, dfSAV$dat)

  out2 <- changeValLabels(dfSAV, varName = "VAR2", value = c(-99, -96), valLabel = c("label 3", "label 2"))
  expect_equal(out2$labels[c(4, 5), "valLabel"], c("label 2", "label 3"))
  expect_equal(out2$dat, dfSAV$dat)

  expect_error(changeValLabels(dfSAV, varName = c("VAR1", "VAR2"), value = 1, valLabel = "test label"))
  expect_error(changeValLabels(dfSAV, varName = c("VAR4"), value = 1, valLabel = "test label"))
})


test_that("changevallabel for adding value labels", {
  out <- changeValLabels(dfSAV, varName = "VAR1", value = 2, valLabel = "test label")
  expect_equal(nrow(out$labels[out$labels$varName == "VAR1", ]), 4)
  expect_equal(out$labels[4, "valLabel"], "test label")
  expect_equal(out$labels[4, "value"], 2)
  expect_equal(out$dat, dfSAV$dat)

  out <- changeValLabels(dfSAV, varName = "VAR1", value = c(1, 2), valLabel = c("test label", "test label2"))
  expect_equal(nrow(out$labels[out$labels$varName == "VAR1", ]), 4)
  expect_equal(out$labels[3, "valLabel"], "test label")
  expect_equal(out$labels[4, "valLabel"], "test label2")
  expect_equal(out$labels[3, "value"], 1)
  expect_equal(out$labels[4, "value"], 2)
  expect_equal(out$dat, dfSAV$dat)
})

test_that("changevallabel for adding value labels to unlabeled variable", {
  out <- changeValLabels(dfUn, varName = "v1", value = 1, valLabel = "test label")
  expect_equal(out$labels[1, "valLabel"], "test label")
  expect_equal(out$labels[1, "value"], 1)
  expect_equal(out$dat, dfUn$dat)

  out <- changeValLabels(dfUn, varName = "v2", value = c(1, 2), valLabel = c("test label", "test label2"))
  expect_equal(nrow(out$labels), 3)
  expect_equal(out$labels[2, "valLabel"], "test label")
  expect_equal(out$labels[3, "valLabel"], "test label2")
  expect_equal(out$labels[1, "valLabel"], NA_character_)
  expect_equal(out$labels[2, "value"], 1)
  expect_equal(out$labels[3, "value"], 2)
  expect_equal(out$dat, dfUn$dat)
})

test_that("add value label to a variable with one existing label", {
  dfT <- data.frame(text1 = c(-96, "g", -99), stringsAsFactors = FALSE)
  gadsT <- import_DF(dfT)
  gadsT <- changeValLabels(gadsT, varName = "text1", value = -96, valLabel = "miss")
  gadsT <- changeMissings(gadsT, varName = "text1", value = -96, missings = "miss")
  out <- changeValLabels(gadsT, varName = "text1", value = -99, valLabel = "miss")
  expect_equal(out$labels$value, c(-96, -99))
})
