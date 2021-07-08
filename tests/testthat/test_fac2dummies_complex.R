
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

df_fac <- data.frame(id = 1:6, fac = c("Opt a", "Opt c, Opt b", "Opt c", "Opt b", "Opt a, Opt b", "Opt a, Opt b, Opt c"),
                     stringsAsFactors = TRUE)
g_fac <- import_DF(df_fac)
g_fac <- recodeGADS(g_fac, varName = "fac", oldValues = c(1, 2, 3, 4, 5, 6), newValues = c(1, 12, 123, 2, 3, 23))

test_that("errors", {
  dfSAV2 <- changeValLabels(dfSAV, varName = "VAR1", value = 2, valLabel = "Two")

  expect_error(fac2dummies_complex(dfSAV2, var = "VAR5"),
               "The following 'vars' are not variables in the GADSdat: VAR5")
  expect_error(fac2dummies_complex(dfSAV2, var = 1),
               "'var' needs to be a character vector of length 1.")

  new_dat <- dfSAV$dat
  new_dat[, "VAR1_a"] <- NA
  dfSAV_test <- suppressMessages(updateMeta(dfSAV, new_dat))
  expect_error(fac2dummies_complex(dfSAV_test, var = "VAR1"),
               "The following variables are already in the 'GADSdat' and conflict with dummy variables you are trying to create: VAR1_a")
})


test_that("factor 2 dummies", {
  expect_message(out <- fac2dummies_complex(g_fac, "fac"),
                 "The following dummy variables have been created: fac_a, fac_b, fac_c")
  expect_equal(dim(out$labels), c(13, 8))
  expect_equal(out$labels$varName[8:9], rep("fac_a", 2))
  expect_equal(out$labels$varName[10:11], rep("fac_b", 2))
  expect_equal(out$labels$varLabel[8:9], rep("fac: Opt a", 2))
  expect_equal(out$labels$value[8:9], c(0, 1))
  expect_equal(out$dat$fac_a, c(1, 0, 0, 0, 1, 1))
  expect_equal(out$dat$fac_b, c(0, 1, 0, 1, 1, 1))
  expect_equal(out$dat$fac_c, c(0, 1, 1, 0, 0, 1))
  expect_equal(out$labels$format[8:11], rep("F2.0", 4))
  expect_equal(out$labels$missings[8:13], rep("valid", 6))
})


