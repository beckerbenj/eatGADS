
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")
dfSAV_unimp <- changeVarNames(dfSAV, "VAR2", "idstud")
dfSAV_unimp$dat$idstud <- 1:4
dfSAV_unimp$dat$VAR1[2] <- 3

dfSAV_imp_dat <- import_DF(data.frame(idstud = c(rep(1, 3), rep(2, 3),rep(3, 3), rep(4, 3)),
                                      imp = rep(1:3, 4), VAR1 = c(1, 1, 1,
                                                        2, 2, 2,
                                                        1, 3, 3,
                                                        1, 2, 1)))

test_that("Errors",{
  dfSAV_unimp2 <- dfSAV_unimp
  expect_error(subImputations(dfSAV_unimp, dfSAV_imp_dat, varName = letters[1:2], id = "idstud", imp = "imp"),
               "'varName' must be a character of length 1.")
  expect_error(subImputations(dfSAV_unimp, dfSAV_imp_dat, varName = "VAR2", id = 2, imp = "imp"),
               "'id' must be a character of length 1.")
  expect_error(subImputations(dfSAV_unimp, dfSAV_imp_dat, varName = "VAR2", id = "idstud", imp = 3:4),
               "'imp' must be a character of length 1.")
  expect_error(subImputations(dfSAV_unimp, dfSAV_imp_dat, varName = "a", id = "idstud", imp = "imp"),
               "'varName' is not a variable in 'GADSdat'.")
  expect_error(subImputations(dfSAV_unimp, dfSAV_imp_dat, varName = "VAR3", id = "idstud", imp = "imp"),
               "'varName_imp' is not a variable in 'GADSdat_imp'.")
  expect_error(subImputations(dfSAV_unimp, dfSAV_imp_dat, varName = "VAR1", id = "id2", imp = "imp"),
               "'id' is not a variable in 'GADSdat'.")
  expect_error(subImputations(dfSAV_unimp, dfSAV_imp_dat, varName = "VAR1", id = "VAR3", imp = "imp"),
               "'id' is not a variable in 'GADSdat_imp'.")
  expect_error(subImputations(dfSAV_unimp, dfSAV_imp_dat, varName = "VAR1", id = "idstud", imp = "VAR3"),
               "'imp' is not a variable in 'GADSdat_imp'.")

  dfSAV_unimp2$dat <- dfSAV_unimp2$dat[c(1, 1, 3, 3), ]
  expect_error(subImputations(dfSAV_unimp2, dfSAV_imp_dat, varName = "VAR1", id = "idstud", imp = "imp"),
               "These 'id' values in 'GADSdat_imp' are not in 'GADSdat': 2, 4")

})

test_that("Substitute imputations when there are valid unimputed values",{
  expect_message(out <- subImputations(dfSAV_unimp, dfSAV_imp_dat, varName = "VAR1", id = "idstud", imp = "imp"),
                 "Values for 2 'id's have been substituted in VAR1.")
  expect_equal(out$labels, dfSAV_imp_dat$labels)
  expect_equal(out$dat[c(1:3,7:9), ], dfSAV_imp_dat$dat[c(1:3,7:9), ])
  expect_equal(out$dat[10:12, 1:2], dfSAV_imp_dat$dat[10:12, 1:2])
  expect_equal(out$dat[10:12, "VAR1"], rep(2, 3))
  expect_equal(out$dat[4:6, "VAR1"], rep(3, 3))
})

test_that("Substitute imputations with different names",{
  dfSAV_imp_dat2 <- changeVarNames(dfSAV_imp_dat, "VAR1", "VAR1_imp")
  expect_message(out <- subImputations(dfSAV_unimp, dfSAV_imp_dat2, varName = "VAR1", varName_imp = "VAR1_imp", id = "idstud", imp = "imp"),
                 "Values for 2 'id's have been substituted in VAR1_imp.")
  expect_equal(out$labels, dfSAV_imp_dat2$labels)
  expect_equal(out$dat[c(1:3,7:9), ], dfSAV_imp_dat2$dat[c(1:3,7:9), ])
  expect_equal(out$dat[10:12, 1:2], dfSAV_imp_dat$dat[10:12, 1:2])
  expect_equal(out$dat[10:12, "VAR1_imp"], rep(2, 3))
  expect_equal(out$dat[4:6, "VAR1_imp"], rep(3, 3))
})

test_that("Substitute imputations when there are missings",{
  dfSAV_imp_dat3 <- dfSAV_imp_dat
  dfSAV_imp_dat3$dat[c(1:3, 10:12), "VAR1"] <- NA
  expect_message(out <- subImputations(dfSAV_unimp, dfSAV_imp_dat3, varName = "VAR1", id = "idstud", imp = "imp"),
                 "Values for 3 'id's have been substituted in VAR1.")
  expect_equal(out$labels, dfSAV_imp_dat$labels)
  expect_equal(out$dat[c(1:3,7:9), ], dfSAV_imp_dat$dat[c(1:3,7:9), ])
  expect_equal(out$dat[10:12, 1:2], dfSAV_imp_dat$dat[10:12, 1:2])
  expect_equal(out$dat[10:12, "VAR1"], rep(2, 3))
  expect_equal(out$dat[4:6, "VAR1"], rep(3, 3))
})
