
################# Match values and variables labels ---------------------------------------------------
mt2 <- data.frame(num1 = 1:4, num2 = c(14, 50, 13, NA), fac1 = factor(NA, "Eng", "Aus", "Aus2"), text2 = c(NA, "Franz", NA, "Ger"),stringsAsFactors = FALSE)
mt2_gads <- import_DF(mt2)

# dfSAV <- import_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")


test_that("Create minmax data.frame", {
  out <- createNumCheck(mt2_gads)
  expect_equal(out$variable, c("num1", "num2"))

  out <- createNumCheck(dfSAV)
  expect_equal(out$variable, c("VAR3"))
  expect_equal(out$varLabel, c("Variable 3"))
})

test_that("check numCheck", {
  nc <- createNumCheck(mt2_gads)
  nc1 <- nc2 <- nc3 <- nc4 <- nc

  nc1$variable <- c("ID", 14)
  expect_error(check_numCheck(mt2_gads, nc1), "Not all variables in numCheck are variables in the GADSdat.")
  nc2$min <- c("ID", 14)
  expect_error(check_numCheck(mt2_gads, nc2), "Column 'min' containts non numeric values.")
  nc3$value_new <- c("ID", 14)
  expect_error(check_numCheck(mt2_gads, nc3), "Column 'value_new' containts non numeric values.")


})

test_that("Apply minmax data.frame", {
  nc <- createNumCheck(mt2_gads)
  nc$min <- c(2, 14)
  nc$max <- c(NA, 50)
  nc$value_new <- c(NA, -99)

  out <- applyNumCheck(mt2_gads, nc)
  expect_equal(out$dat$num1, c(NA, 2, 3, 4))
  expect_equal(out$dat$num2, c(14, 50, -99, NA))
})
