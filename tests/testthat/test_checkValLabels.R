
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

df3 <- df2
df3$dat[1, 1:2] <- 8

test_that("Input validation", {
  expect_error(checkValLabels(df1, vars = 3:4),
               "The following 'vars' are not variables in the GADSdat: 3, 4")
  expect_error(checkValLabels(df1, valueRange = 3:5),
               "'valueRange' needs to be a numeric vector of length 2.")
  expect_error(checkValLabels(df1, valueRange = letters[3:4]),
               "'valueRange' needs to be a numeric vector of length 2.")
})

test_that("Standard case", {
  out <- checkValLabels(dfSAV)
  expect_equal(names(out), c("labels with no values", "not labeled values"))
  expect_equal(names(out[[1]]), paste0("VAR", 1:3))
  expect_equal(out[[1]][[1]], numeric(0))
  expect_equal(out[[1]][[2]], c(-96, -99))
  expect_equal(out[[1]][[3]], c(-99))
  expect_equal(out[[2]][[1]], c(2))
  expect_equal(out[[2]][[3]], c(1))
})

test_that("With NAs", {
  suppressMessages(dfSAV2 <- recode2NA(dfSAV, value = 1))
  out <- checkValLabels(dfSAV2)
  expect_equal(names(out), c("labels with no values", "not labeled values"))
  expect_equal(names(out[[1]]), paste0("VAR", 1:3))
  expect_equal(out[[1]][[1]], c(1))
  expect_equal(out[[1]][[2]], c(-96, -99))
  expect_equal(out[[1]][[3]], c(-99))
  expect_equal(out[[2]][[1]], c(2))
  expect_equal(out[[2]][[3]], numeric(0))
})


test_that("with specific value range", {
  out <- checkValLabels(dfSAV, valueRange = c(-100, 0))
  expect_equal(names(out), c("labels with no values", "not labeled values"))
  expect_equal(names(out[[1]]), paste0("VAR", 1:3))
  expect_equal(out[[1]][[1]], numeric(0))
  expect_equal(out[[1]][[2]], c(-96, -99))
  expect_equal(out[[1]][[3]], c(-99))
  expect_equal(out[[2]][[1]], numeric())
  expect_equal(out[[2]][[3]], numeric())

  out2 <- checkValLabels(dfSAV, valueRange = c(0, -100))
  expect_equal(out2, out)
})
