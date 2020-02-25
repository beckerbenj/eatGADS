
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
  ng <- applyLookup(testM, lu2)
  expect_equal(ng$dat$VAR1_r, c(10, -9, -6, 11))
})

test_that("Applying recode for more variables",{
  lu3$value_new <- c(-9, -6, 10, -10, 11)
  ng <- applyLookup(testM, lu3)
  expect_equal(ng$dat$VAR1_r, c(10, -9, -6, 11))
  expect_equal(ng$dat$VAR2_r, rep(-10, 4))
})

test_that("Workflow multiple columns, collapse, apply",{
  lu1$r1 <- c(1, 2, NA, 4, NA)
  lu1$r2 <- c(NA, -2, 3, 4, 5)

  lu_r <- collapseColumns(lu1, recodeVars = c("r1", "r2"), prioritize = "r2")
  testM2 <- applyLookup(testM, lu_r)
  expect_equal(testM2$dat$VAR2_r, rep(5, 4))
  expect_equal(testM2$dat$VAR1_r, c(1, -2, 3, 4))
})



################# Combine MC and text ---------------------------------------------------
mc <- as.factor(c("Ger", "other", "other", "Aus"))
mt <- data.frame(ID = 1:4, mc = mc, text = c(NA, "Eng", "Aus", "Aus2"), stringsAsFactors = FALSE)
mt_gads <- import_DF(mt)

test_that("Combine mc and text",{
  test <- collapseMC_Text(mt_gads, mc_var = "mc", text_var = "text", mc_code4text = "other")
  expect_equal(test$dat$mc_r, c(3, 2, 1, 1))
  test_dat <- extractData(test)
  expect_equal(test_dat$mc_r, c("Ger", "Eng", "Aus", "Aus"))
})




