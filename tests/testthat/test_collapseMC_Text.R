
################# Combine MC and text ---------------------------------------------------
mc <- as.factor(c("Ger", "other", "other", "Aus"))
mt <- data.frame(ID = 1:4, mc = mc, text = c(NA, "Eng", "Aus", "Aus2"), stringsAsFactors = FALSE)
mt_gads <- import_DF(mt)

test_that("Errors collapse mc text",{
  expect_error(collapseMC_Text(mt_gads, mc_var = "some_var", text_var = "text", mc_code4text = "other"), "'mc_var' is not a variable in the GADSdat.")
  expect_error(collapseMC_Text(mt_gads, mc_var = "mc", text_var = "some_var", mc_code4text = "other"), "'text_var' is not a variable in the GADSdat.")
  mtcars_g <- import_DF(mtcars)
  expect_error(collapseMC_Text(mtcars_g, mc_var = "cyl", text_var = "gear", mc_code4text = "other"), "'mc_var' must be a labeled integer.")
})

test_that("Errors mc_value4text collapse mc text",{
  expect_error(collapseMC_Text(mt_gads, mc_var = "mc", text_var = "text", mc_code4text = 3), "'mc_code4text' must be a character of length 1.")
  expect_error(collapseMC_Text(mt_gads, mc_var = "mc", text_var = "text", mc_code4text = c("1", "2")), "'mc_code4text' must be a character of length 1.")
  expect_error(collapseMC_Text(mt_gads, mc_var = "mc", text_var = "text", mc_code4text = "other_"), "'mc_code4text' must be a 'valLabel' entry for 'mc_var'.")
})

test_that("Append variable label",{
  mt_gads2 <- changeVarLabels(mt_gads, varName = "ID", varLabel = "id")
  mt_gads2t <- append_varLabel(mt_gads2, varName = "ID", label_suffix = "(recoded)")
  expect_equal(mt_gads2t$label$varLabel[1], "id (recoded)")

  mt_gads2u <- append_varLabel(mt_gads2, varName = "mc", label_suffix = "(recoded)")
  expect_equal(mt_gads2u$label$varLabel[2], "(recoded)")

  mt_gads2v <- append_varLabel(mt_gads2, varName = "mc", label_suffix = "")
  expect_equal(mt_gads2v$label$varLabel[2], NA_character_)
})


test_that("Combine mc and text",{
  test <- collapseMC_Text(mt_gads, mc_var = "mc", text_var = "text", mc_code4text = "other")
  expect_true("mc_r" %in% names(test$dat))
  expect_equal(test$labels[6, "varLabel"], "(recoded)")
  expect_equal(test$dat$mc_r, c(2, 4, 1, 1))
  test_dat <- extractData(test)
  expect_equal(test_dat$mc_r, c("Ger", "Eng", "Aus", "Aus"))
})


test_that("Combine mc and text into old variables",{
  test <- collapseMC_Text(mt_gads, mc_var = "mc", text_var = "text", mc_code4text = "other", var_suffix = NULL, label_suffix = NULL)
  expect_false("mc_r" %in% names(test$dat))
  expect_equal(test$dat$mc, c(2, 4, 1, 1))
  test_dat <- extractData(test)
  expect_equal(test_dat$mc, c("Ger", "Eng", "Aus", "Aus"))
})

test_that("Combine mc and text into old variables via empty string",{
  test <- collapseMC_Text(mt_gads, mc_var = "mc", text_var = "text", mc_code4text = "other", var_suffix = "", label_suffix = NULL)
  expect_false("mc_r" %in% names(test$dat))
  expect_equal(test$dat$mc, c(2, 4, 1, 1))
  test_dat <- extractData(test)
  expect_equal(test_dat$mc, c("Ger", "Eng", "Aus", "Aus"))
})


test_that("Combine mc and text with Missings on mcs",{
  mt_gads2 <- recodeGADS(mt_gads, varName = "mc", oldValues = c(1, 2, 3), newValues = c(-9, -8, 1), existingMeta = "value")
  mt_gads2 <- changeValLabels(mt_gads2, "mc", value = c(1, -8, -9), c("Aus", "missing other", "missing"))
  mt_gads2 <- checkMissings(mt_gads2)

  test <- collapseMC_Text(mt_gads2, mc_var = "mc", text_var = "text", mc_code4text = "Aus")
  expect_equal(test$dat$mc_r, c(-8, 3, 1, 2))
  test_dat <- extractData(test)
  expect_equal(test_dat$mc_r, c(NA, "Eng", "Aus", "Aus2"))
})


test_that("Combinations of mc_code4text and missing in text variable",{
  mt_gads2 <- recodeGADS(mt_gads, varName = "mc", oldValues = c(1, 2, 3), newValues = c(-9, 3, 1), existingMeta = "value")
  mt_gads2 <- changeValLabels(mt_gads2, "mc", value = c(1, 3, -9), c("Aus", "other", "missing"))
  mt_gads2 <- checkMissings(mt_gads2)

  test <- collapseMC_Text(mt_gads2, mc_var = "mc", text_var = "text", mc_code4text = "other")
  expect_equal(test$dat$mc_r, c(3, 1, 1, 4))
  test_dat <- extractData(test)
  expect_equal(test_dat$mc_r, c("other", "Aus", "Aus", "Aus2"))
})


# testMC <- import_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_recodeMC.sav")
suppressWarnings(testMC <- import_spss("helper_spss_recodeMC.sav"))

test_that("Combination of mc_code4text and labeled missing in text variable",{
  test <- collapseMC_Text(testMC, mc_var = "mc", text_var = "text", mc_code4text = "other")
  expect_equal(test$dat$mc_r, c(-9, 1, 2, -9, 4, 3, -9))
  test_dat <- extractData(test)
  expect_equal(test_dat$mc_r, c(NA, "Ger", "Eng", NA, "Aus", "other", NA))
})
