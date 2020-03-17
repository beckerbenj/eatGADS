
context("Value recoding via Excel")


################# Create lookup table ---------------------------------------------------
# rawDat <- import_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss.sav")
rawDat <- import_spss("helper_spss.sav")

test_that("Create lookup for 1 character variables",{
  test <- createLookup(rawDat, recodeVars = c("VAR1"))
  expect_equal(test$variable, c("VAR1"))
  expect_equal(dim(test), c(1, 3))
})

test_that("Create lookup for 2 numeric variables",{
  test <- createLookup(rawDat, recodeVars = c("VAR1", "VAR2"), addCols = c("Rater_1", "Rater_2"))
  expect_equal(test$variable, c("VAR1", "VAR2"))
  expect_equal(names(test), c("variable", "value", "Rater_1", "Rater_2"))
})

test_that("Create lookup for 2 mixed variables",{
  test <- suppressWarnings(createLookup(rawDat, recodeVars = c("VAR1", "VAR3")))
  expect_equal(test$variable, c("VAR1", "VAR3"))
  expect_equal(test$value, c(1, "a"))
  expect_equal(test$value_new, c(NA, NA))
  expect_equal(dim(test), c(2, 3))
})

# testM <- import_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav")
testM <- import_spss("helper_spss_missings.sav")

lu1 <- createLookup(testM, recodeVars = c("VAR1", "VAR2"), addCols = c("r1", "r2"))

test_that("Test unique values functionality for Create lookups",{
  expect_equal(lu1$value, c(1, -99, -96, 2, 1))
  #expect_equal(test$value, c(1, 3))
  #expect_equal(test$newValue, c(NA, NA))
  #expect_equal(dim(test), c(2, 3))
})

lu2 <- createLookup(testM, recodeVars = c("VAR1"), sort_by = "value")
lu3 <- createLookup(testM, recodeVars = c("VAR1", "VAR2"), sort_by = "value")
test_that("Ordering by values",{
  expect_equal(lu2$value, c(-99, -96, 1, 2))

  expect_equal(lu3$value, c(-99, -96, 1, 1, 2))

  test2 <- createLookup(testM, recodeVars = c("VAR1", "VAR2"), sort_by = c("variable", "value"))
  expect_equal(test2$value, c(-99, -96, 1, 2, 1))
})


################# Recode based on lookup table ---------------------------------------------------
test_that("Test unique values functionality for Create lookups",{
  lu1$r1 <- c(1, 2, NA, 4, NA)
  lu1$r2 <- c(NA, -2, 3, 4, 5)

  lu_r <- collapseColumns(lu1, recodeVars = c("r1", "r2"), prioritize = "r2")
  expect_equal(lu_r$value_new, c(1, -2, 3, 4, 5))
})

test_that("Tests for formatting of lookup",{
  lu_false <- lu1[, 2:4]
  expect_error(ng <- applyLookup(testM, lu_false))
})

test_that("Applying recode for 1 variable",{
  lu2$value_new <- c(-9, -6, 10, 11)
  ng <- applyLookup(testM, lu2, suffix = "_r")
  expect_equal(ng$dat$VAR1_r, c(10, -9, -6, 11))
})

test_that("Applying recode for 1 variable while overwriting",{
  lu2$value_new <- c(-9, -6, 10, 11)
  ng <- applyLookup(testM, lu2)
  expect_equal(ng$dat$VAR1, c(10, -9, -6, 11))
  expect_equal(dim(ng$dat), c(4, 3))
})

test_that("Applying recode for more variables",{
  lu3$value_new <- c(-9, -6, 10, -10, 11)
  ng <- applyLookup(testM, lu3, suffix = "_r")
  expect_equal(ng$dat$VAR1_r, c(10, -9, -6, 11))
  expect_equal(ng$dat$VAR2_r, rep(-10, 4))
})

test_that("Applying recode for more variables and lookup as tibble",{
  lu3$value_new <- c(-9, -6, 10, -10, 11)
  lu_tbl <- tibble::as_tibble(lu3)
  ng <- applyLookup(testM, lu_tbl, suffix = "_r")
  expect_equal(ng$dat$VAR1_r, c(10, -9, -6, 11))
  expect_equal(ng$dat$VAR2_r, rep(-10, 4))
})

test_that("Workflow multiple columns, collapse, apply",{
  lu1$r1 <- c(1, 2, NA, 4, NA)
  lu1$r2 <- c(NA, -2, 3, 4, 5)

  lu_r <- collapseColumns(lu1, recodeVars = c("r1", "r2"), prioritize = "r2")
  testM2 <- applyLookup(testM, lu_r, suffix = "_r")
  expect_equal(testM2$dat$VAR2_r, rep(5, 4))
  expect_equal(testM2$dat$VAR1_r, c(1, -2, 3, 4))
})



################# Combine MC and text ---------------------------------------------------
mc <- as.factor(c("Ger", "other", "other", "Aus"))
mt <- data.frame(ID = 1:4, mc = mc, text = c(NA, "Eng", "Aus", "Aus2"), stringsAsFactors = FALSE)
mt_gads <- import_DF(mt)

test_that("Errors variable names",{
  expect_error(collapseMC_Text(mt_gads, mc_var = "some_var", text_var = "text", mc_code4text = "other"), "mc_var is not a variable in the GADSdat.")
  expect_error(collapseMC_Text(mt_gads, mc_var = "mc", text_var = "some_var", mc_code4text = "other"), "text_var is not a variable in the GADSdat.")
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


test_that("Combine mc and text with Missings on mcs",{
  mt_gads2 <- recodeGADS(mt_gads, varName = "mc", oldValues = c(1, 2, 3), newValues = c(-9, -8, 1), newValueLabels = c("1" = "Aus" , "-8" = "missing other", "-9" = "missing"))
  mt_gads2 <- checkMissings(mt_gads2)

  test <- collapseMC_Text(mt_gads2, mc_var = "mc", text_var = "text", mc_code4text = "other")
  expect_equal(test$dat$mc_r, c(-8, 1, 1, 2))
  test_dat <- extractData(test)
  expect_equal(test_dat$mc_r, c(NA, "Aus", "Aus", "Aus2"))
})


test_that("Combinations of mc_code4text and missing in text variable",{
  mt_gads2 <- recodeGADS(mt_gads, varName = "mc", oldValues = c(1, 2, 3), newValues = c(-9, 3, 1), newValueLabels = c("1" = "Aus" , "3" = "other", "-9" = "missing"))
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



################# Combine multi MC and text ---------------------------------------------------
mt2 <- data.frame(ID = 1:4, mc1 = c(1, 0, 0, 0), mc2 = c(0, 0, 0, 0), mc3 = c(0, 1, 1, 0), text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c(NA, "Franz", NA, NA),stringsAsFactors = FALSE)
mt2_gads <- import_DF(mt2)
mt3_gads <- changeVarLabels(mt2_gads, varName = c("mc1", "mc2", "mc3"), varLabel = c("Lang: Eng", "Aus spoken", "other"))
df <- data.frame(v1 = c("j", "i", NA, NA),
                 v2 = c(NA, "i", NA, "k"),
                 v3 = c("j", NA, NA, "j"), stringsAsFactors = FALSE)

test_that("Basic errors in matchValues_varLabels", {
  expect_error(matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc4"), values = c("Ger", "Esp")), "The following vars are not a variable in the GADSdat:\nmc4")
  expect_error(matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2"), values = c("Ger", "Esp"), label_by_hand = c("other" = "mc3")), "All variable names in label_by_hand must be variables in mc_vars.")
})

test_that("Match values and variable names by variable labels", {
  out <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "Eng"), label_by_hand = c("other" = "mc3"))
  expect_equal(unname(out), c("mc1", "mc2", "mc3"))
  expect_equal(names(out), c("Eng", "Aus", "other"))

  # empty matches
  expect_error(matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Ger", "Esp", "Eng")), "The following mc_vars have not been assigned a value: mc2, mc3")
  out2 <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Ger", "Esp", "Eng"), label_by_hand = c("other" = "mc3", "Aus" = "mc2"))
  expect_equal(unname(out2), c("mc1", "mc2", "mc3"))
  expect_equal(names(out2), c("Eng", "Aus", "other"))

  expect_error(matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Ger", "Esp")), "The following mc_vars have not been assigned a value: mc1, mc2, mc3")

  #expect_equal(out$v2, c(NA, NA, NA, "k"))
})


test_that("Remove values from some variables", {
  out <- remove_values(df, vars = c("v1", "v2"), values = c("j", "i"))
  expect_equal(out$v1, c(NA_character_, NA, NA, NA))
  expect_equal(out$v2, c(NA, NA, NA, "k"))
  expect_equal(out$v3, c("j", NA, NA, "j"))
})


test_that("Left fill for text variables", {
  out <- left_fill(df)
  expect_equal(out$v1, c("j", "i", NA, "k"))
  expect_equal(out$v2, c("j", "i", NA, "j"))
  expect_equal(out$v3, c(NA_character_, NA, NA, NA))
})

test_that("Check for duplicate values in combine multi mc and text", {
  mc_vars <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "other"))
  mt3_gads_err <- mt3_gads
  mt3_gads_err$dat[3, "text2"] <- "Aus"
  expect_error(collapseMultiMC_Text(mt3_gads_err, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3"), "Duplicate values in row 3.")
})

test_that("Combine multi mc and text", {
  mc_vars <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "other"))
  test <- collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3")

  expect_equal(test$dat$text1_r, c(NA, "Franz", NA, "Aus2"))
  expect_equal(test$dat$text2_r, c(NA_character_, NA, NA, NA))
  expect_equal(test$dat$text1, c(NA, "Eng", "Aus", "Aus2"))
  expect_equal(test$dat$mc1_r, c(1, 1, 0, 0))
  expect_equal(test$dat$mc2_r, c(0, 0, 1, 0))
  expect_equal(test$labels[test$labels$varName == "text1_r", "varLabel"], "(recoded)")
  expect_equal(test$labels[test$labels$varName == "mc1_r", "varLabel"], "Lang: Eng (recoded)")

  test2 <- collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3", var_suffix = "", label_suffix = "")
  expect_equal(test2$dat$text1, c(NA, "Franz", NA, "Aus2"))
  expect_equal(test2$dat$text2, c(NA_character_, NA, NA, NA))
  expect_equal(test2$labels[test2$labels$varName == "mc1", "varLabel"], "Lang: Eng")
})


################# mulitple Characters to factors with identical labels ---------------------------------------------------
mt4 <- data.frame(text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c("Ger", "Franz", "Eng", NA), stringsAsFactors = FALSE)
mt4_gads <- import_DF(mt4)

test_that("Combine multi mc and text", {
  out <- multiChar2fac(mt4_gads, vars = namesGADS(mt4_gads))
  expect_equal(out$dat$text1_r, c(NA, 3, 1, 2))
  expect_equal(out$dat$text2_r, c(5, 4, 3, NA))
  expect_equal(out$labels[out$labels$varName == "text1_r", "value"], out$labels[out$labels$varName == "text2_r", "value"])
  expect_equal(out$labels[out$labels$varName == "text1_r", "valLabel"], c("Aus", "Aus2", "Eng", "Franz", "Ger"))

  out2 <- multiChar2fac(mt4_gads, vars = namesGADS(mt4_gads), var_suffix = "")
  expect_equal(out2$dat$text1, c(NA, 3, 1, 2))
  expect_equal(out2$dat$text2, c(5, 4, 3, NA))
  expect_equal(out2$labels[out$labels$varName == "text1", "varLabel"][1], "(recoded)")
})



################# count character variables and remove overflowing while coding NA ---------------------------------------------------
test_that("Too many strings to missing", {
  df2 <- left_fill(df)
  df2$v3 <- c("a", NA, NA, "b")
  out <- max_num_strings2NA(df2, vars = names(df), max_num = 2, na_value = NA)
  expect_equal(as.character(out[1, ]), c(NA_character_, NA, NA))
  expect_equal(as.character(out[2, ]), c("i", "i", NA))
  expect_equal(as.character(out[4, ]), c(NA_character_, NA, NA))
})


test_that("Too many strings to missing and NA coding", {
  out <- remove_2NA_char(mt4_gads, vars = namesGADS(mt4_gads), max_num = 1, na_value = -99)

  expect_equal(out$dat$text1, c(-99, -99, -99, "Aus2"))
  expect_equal(dim(out$dat), c(4, 1))
})


#mc_vars <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "Eng"), label_by_hand = c("other" = "mc3"))
#out_gads <- collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3")
#out_gads2 <- multiChar2fac(out_gads, vars = c("text1_r", "text2_r"))
#remove_2NA_char(out_gads2, vars = c("text1", "text2"), max_num = 1, na_value = -99)

