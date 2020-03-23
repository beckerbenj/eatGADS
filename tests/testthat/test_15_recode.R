
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

test_that("Check Lookup, errors and warnings",{
  expect_error(check_lookup(lu2, testM), "All values have no recode value assigned (missings in value_new).", fixed = TRUE)

  lu2_2 <- lu2_1 <- lu2
  lu2_1[1, 1] <- "v10"
  expect_error(check_lookup(lu2_1, testM), "Some of the variables are not variables in the GADSdat.")
  lu2_2[1, 2] <- NA
  expect_error(check_lookup(lu2_2, testM), "In some rows there are missings in column value.")
})

test_that("Behaviour if new variable containts only missings",{
  lu3_1 <- lu3
  lu3_1$value_new <- c(-9, -6, 1, NA, 2)
  mess <- capture_warnings(applyLookup(testM, lu3_1))
  expect_equal(mess[[2]], "In the new variable VAR2 all values are missing, therefore the variable is dropped. If this behaviour is not desired, contact the package author.")

  suppressWarnings(out <- applyLookup(testM, lu3_1))
  expect_equal(namesGADS(out), c("VAR1", "VAR3"))

  suppressWarnings(out <- applyLookup(testM, lu3_1, suffix = c("_r")))
  expect_equal(namesGADS(out), c("VAR1", "VAR2", "VAR3", "VAR1_r"))
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



