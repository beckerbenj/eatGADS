# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

dummy_df <- data.frame(d1 = c("eng", "no eng", "eng", "no eng"),
                       d2 = c("french", "french", "no french", "no french"),
                       d3 = c("no ger", "ger", "no ger", "ger"),
                       stringsAsFactors = TRUE)
dummy_g <- import_DF(dummy_df)


test_that("errors", {
  expect_error(dummies2char(dfSAV, dummies = "VAR5", dummyValues = 1, charNames = "la"),
               "The following 'vars' are not variables in the GADSdat: VAR5")
  expect_error(dummies2char(dfSAV, dummies = 1),
               "'dummies' needs to be a character vector.")
  expect_error(dummies2char(dfSAV, dummies = c("VAR1", "VAR2"), dummyValues = 1, charNames = c("la", "lo")),
               "'dummyValues' needs to be the same length as 'dummies'.")
  expect_error(dummies2char(dfSAV, dummies = c("VAR1", "VAR2"), dummyValues = 1:2, charNames = c("la")),
               "'charNames' needs to be the same length as 'dummies'.")
})


dummy_g2 <- changeValLabels(dummy_g, varName = "d1", value = -99, valLabel = "missing")
dummy_g2 <- changeMissings(dummy_g2, varName = "d1", value = -99, missings =  "miss")

test_that("dummies 2 characters", {
  out <- dummies2char(dummy_g2, dummies = namesGADS(dummy_g2), dummyValues = c("english", "french", "german"),
                      charNames = c("c1", "c2", "c3"))
  expect_equal(as.character(out$dat[1, 4:6]), c("english", "french", NA))
  expect_equal(as.character(out$dat[2, 4:6]), c("french", "german", NA))
  expect_equal(as.character(out$dat[3, 4:6]), c("english", NA, NA))
  expect_equal(as.character(out$dat[4, 4:6]), c("german", NA, NA))

  expect_equal(extractMeta(out, "c1")$value, -99)
  expect_equal(extractMeta(out, "c1")$missings, "miss")
  expect_equal(extractMeta(out, "c2")$value, NA_real_)
  expect_equal(extractMeta(out, "c2")$missings, NA_character_)
})

test_that("dummies 2 characters with invalid charNames", {
  expect_message(out <- dummies2char(dummy_g2, dummies = namesGADS(dummy_g2), dummyValues = c("english", "french", "german"),
                      charNames = c("v_1", "v.2", "v_3")),
                 "v.2 has been renamed to v_2")
  expect_equal(namesGADS(out)[5], c("v_2"))

  out2 <- dummies2char(dummy_g2, dummies = namesGADS(dummy_g2), dummyValues = c("english", "french", "german"),
                                     charNames = c("v_1", "v.2", "v_3"), checkVarNames = FALSE)
  expect_equal(namesGADS(out2)[5], c("v.2"))
})
