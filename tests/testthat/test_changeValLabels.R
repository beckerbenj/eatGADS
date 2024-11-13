
dfSAV <- import_spss(file = test_path("helper_spss_missings.sav"))
dfUn <- import_DF(data.frame(v1 = 1, v2 = 2))

test_that("changeValLabel input validation", {
  expect_error(changeValLabels(dfSAV, varName = c("VAR1"), value = 1:2, valLabel = "test label"),
               "value and valLabel are not of identical length.")
  expect_error(changeValLabels(dfSAV, varName = c("VAR4"), value = 1, valLabel = "test label"),
               "The following 'varName' are not variables in the GADSdat: VAR4")
})

test_that("changevallabel wrapper", {
  out <- changeValLabels(dfSAV, varName = "VAR1", value = 1, valLabel = "test label")
  expect_equal(out$labels[3, "valLabel"], "test label")
  expect_equal(out$dat, dfSAV$dat)

  out2 <- changeValLabels(dfSAV, varName = "VAR2", value = c(-99, -96),
                          valLabel = c("label 3", "label 2"))
  expect_equal(out2$labels[c(4, 5), "value"], c(-99, -96))
  expect_equal(out2$labels[c(4, 5), "valLabel"], c("label 3", "label 2"))
  expect_equal(out2$dat, dfSAV$dat)
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
  expect_equal(out$labels$value, c(-99, -96))
})

test_that("add value label to a variable with multiple existing and multiple new labels", {
  out <- changeValLabels(dfSAV, varName = "VAR1", value = c(-99, -98, -97, -96, -95), valLabel =
                           c("miss1", "miss2", "miss3", "miss4", "miss5"))
  expect_equal(out$labels$value[1:5], -99:-95)
  expect_equal(out$labels$valLabel[1:5], paste0("miss", 1:5))
})

test_that("changeValLabels for multiple variables at once", {
  out <- changeValLabels(dfSAV, varName = c("VAR1", "VAR2"),
                         value = c(-99, -98, -97, -96, -95),
                         valLabel = c("miss1", "miss2", "miss3", "miss4", "miss5"))
  expect_equal(out$labels$value[1:5], -99:-95)
  expect_equal(out$labels$value[7:11], -99:-95)
  expect_equal(out$labels$valLabel[1:5], paste0("miss", 1:5))
  expect_equal(out$labels$valLabel[7:11], paste0("miss", 1:5))

})
