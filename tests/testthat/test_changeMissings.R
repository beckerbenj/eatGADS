
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")
dfUn <- import_DF(data.frame(v1 = 1, v2 = 2))

test_that("changemissings wrapper", {
  out <- changeMissings(dfSAV, varName = "VAR1", value = 1, missings = "miss")
  expect_equal(out$labels[3, "missings"], "miss")
  expect_equal(out$dat, dfSAV$dat)

  out2 <- changeMissings(dfSAV, varName = "VAR2", value = c(-99, -96), missings = c("valid", "miss"))
  expect_equal(out2$labels[c(4, 5), "value"], c(-99, -96))
  expect_equal(out2$labels[c(4, 5), "missings"], c("valid", "miss"))
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
  #out <- changeMissings(dfSAV, varName = "VAR1", value = c(2, 1), missings = c("miss", "miss"))
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

test_that("Adding value label bug", {
  dat_ori <- data.frame(ID = 1:5,
                        var1 = c(1, 3, 4, 1, -99),
                        var2 = c(3, 2, 4, 1, -99),
                        char1 = c("hello", "hi", "hallo", "hoi", "hi"),
                        fac1 = factor(c("engl", "ger", "ger", "ita", "fr")))
  dat <- import_DF(dat_ori)

  dat <- changeValLabels(dat, varName = "var1", value = c(1, 2), valLabel = c("Value label 1", "Value label 2"))
  dat2 <- changeMissings(dat, varName = "var1", value = c(1, -99), missings = c("valid", "miss"))
  expect_equal(dat2$labels[2, "valLabel"], NA_character_)
})

test_that("sequence bug with labeled variables", {
  dat1_seq <- data.frame(ID = 1:7,
                         var1 = c(0, 1, 1, 1, -99, -98, -97))
  dat1 <- import_DF(dat1_seq)

  dat1_relabel <- changeValLabels(dat1, varName = "var1", value = c(0, 1, -99, -97), valLabel = c("true",
                                                                                                  "false",
                                                                                                  "don't know",
                                                                                                  "no idea"))


  dat1_changed <- changeMissings(dat1_relabel, varName = "var1", value = c(-99:-97), missings = rep("miss", 3))
  expect_equal(dat1_changed$labels[2:4, "missings"], rep("miss", 3))
})
