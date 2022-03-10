
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


test_that("Fill imputations when there are missings",{
  dfSAV_imp_dat3 <- dfSAV_imp_dat
  dfSAV_imp_dat3$dat[c(1:3, 7:12), "VAR1"] <- NA
  out <- fillImputations(dfSAV_unimp, dfSAV_imp_dat3, varName = "VAR1", id = "idstud", imp = "imp")

  expect_equal(out$labels, dfSAV_imp_dat$labels)
  expect_equal(out$dat[c(1:3), ], dfSAV_imp_dat$dat[c(1:3), ])
  expect_equal(out$dat[7:9, "VAR1"], rep(NA_real_, 3))
  expect_equal(out$dat[10:12, 1:2], dfSAV_imp_dat$dat[10:12, 1:2])
  expect_equal(out$dat[10:12, "VAR1"], rep(2, 3))
  expect_equal(out$dat[4:6, "VAR1"], rep(2, 3))
})
