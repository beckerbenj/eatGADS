
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")
dfUn <- import_DF(data.frame(v1 = 1, v2 = 2))

test_that("changemissings wrapper", {
  out <- changeMissings(dfSAV, varName = "VAR1", value = 1, missings = "miss")
  expect_equal(out$labels[3, "missings"], "miss")
  expect_equal(out$dat, dfSAV$dat)

  out2 <- changeMissings(dfSAV, varName = "VAR2", value = c(-99, -96), missings = c("valid", "miss"))
  expect_equal(out2$labels[c(4, 5), "missings"], c("miss", "valid"))
  expect_equal(out2$dat, dfSAV$dat)

  expect_error(changeMissings(dfSAV, varName = c("VAR1", "VAR2"), value = 1, missings = "miss"),
               "'varName' is not a character vector of length 1.")
  expect_error(changeMissings(dfSAV, varName = c("VAR4"), value = 1, missings = "miss"),
               "'varName' is not a variable name in the GADSdat.")
  expect_error(changeMissings(dfSAV, varName = "VAR1", value = 1, missings = "miss2"),
               "All values in 'missings' need to be 'miss' or 'valid'.")
})

test_that("changemissings for adding value labels", {
  out <- changeMissings(dfSAV, varName = "VAR1", value = 2, missings = "miss")
  expect_equal(nrow(out$labels[out$labels$varName == "VAR1", ]), 4)
  expect_equal(out$labels[4, "missings"], "miss")
  expect_equal(out$labels[4, "value"], 2)
  expect_equal(out$dat, dfSAV$dat)

  out <- changeMissings(dfSAV, varName = "VAR1", value = c(1, 2), missings = c("miss", "valid"))
  expect_equal(nrow(out$labels[out$labels$varName == "VAR1", ]), 4)
  expect_equal(out$labels[3, "missings"], "miss")
  expect_equal(out$labels[4, "missings"], "valid")
  expect_equal(out$labels[3, "value"], 1)
  expect_equal(out$labels[4, "value"], 2)
  expect_equal(out$dat, dfSAV$dat)
})


test_that("changemissings for adding value labels to unlabeled variable", {
  out <- changeMissings(dfUn, varName = "v1", value = 1, missings = "miss")
  expect_equal(out$labels[1, "missings"], "miss")
  expect_equal(out$labels[1, "value"], 1)
  expect_equal(out$dat, dfUn$dat)

  out <- changeMissings(dfUn, varName = "v2", value = c(1, 2), missings = c("miss", "valid"))
  expect_equal(nrow(out$labels), 3)
  expect_equal(out$labels[2, "missings"], "miss")
  expect_equal(out$labels[3, "missings"], "valid")
  expect_equal(out$labels[1, "missings"], NA_character_)
  expect_equal(out$labels[2, "value"], 1)
  expect_equal(out$labels[3, "value"], 2)
  expect_equal(out$dat, dfUn$dat)
})
