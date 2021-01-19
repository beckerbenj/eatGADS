
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

test_that("errors", {
  expect_error(compareGADS(df1, df1, varNames = "other"),
               "The following 'vars' are not variables in the GADSdat: other")
})

test_that("Compare GADS", {
  df1_b <- df1
  df1_b$dat[, 2] <- c(9, 9)
  out <- compareGADS(df1, df1_b, varNames = namesGADS(df1))

  expect_equal(out[[1]], "all equal")
  expect_equal(out[[2]], data.frame(value = c("3", "5"), frequency = c(1, 1),
                                    valLabel = c(NA_character_, NA), missings = c(NA_character_, NA),
                                    stringsAsFactors = FALSE))

  dfSAV_b <- dfSAV
  dfSAV_b$dat[, 1] <- c(9, 9, 9, 9)
  dfSAV_b$dat[, 3] <- c(9, 9, 9, 9)
  out2 <- compareGADS(dfSAV, dfSAV_b, varNames = namesGADS(dfSAV))

  expect_equal(out2[["VAR1"]], data.frame(value = c("-99", "-96", "1", "2"), frequency = rep(1, 4),
                                     valLabel = c("By design", "Omission", "One", NA),
                                     missings = c("miss", "miss", "valid", NA),
                                     stringsAsFactors = FALSE))
  expect_equal(out2[["VAR2"]], "all equal")
  expect_equal(out2[[3]], data.frame(value = c("-98", "1"), frequency = c(1, 3),
                                     valLabel = NA_character_, missings = c("miss", NA),
                                     stringsAsFactors = FALSE))
})
