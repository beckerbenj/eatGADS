
dfSAV <- import_spss(file = test_path("helper_spss_missings.sav"))
load(file = test_path("helper_data.rda"))

df3 <- df2
df3$dat[1, 1:2] <- 8

iris2 <- iris
iris2[, "charVar"] <- "test"
suppressMessages(iris_g <- import_DF(iris2))

test_that("Input validation", {
  expect_error(checkEmptyValLabels(df1, vars = 3:4),
               "The following 'vars' are not variables in the GADSdat: 3, 4")
  expect_error(checkEmptyValLabels(df1, valueRange = 3:5),
               "'valueRange' needs to be a numeric vector of length 2.")
  expect_error(checkEmptyValLabels(df1, valueRange = letters[3:4]),
               "'valueRange' needs to be a numeric vector of length 2.")
  expect_error(checkEmptyValLabels(df1, output = "List"))
})

test_that("checkEmptyValLabels", {
  out <- checkEmptyValLabels(dfSAV)
  expect_equal(names(out), paste0("VAR", 1:3))
  expect_equal(out[[1]], NULL)
  expect_equal(names(out[[2]]), c("value", "valLabel", "missings"))
  expect_equal(out[[2]]$value, c(-99, -96))
  expect_equal(out[[2]]$valLabel, c(NA, "missing"))
  expect_equal(out[[2]]$missings, c("miss", "valid"))
  expect_equal(out[[3]]$value, c(-99))
})

test_that("checkEmptyValLabels data.frame", {
  out <- checkEmptyValLabels(dfSAV, output = "data.frame")
  expect_equal(names(out), c("variable", "value", "valLabel", "missings"))
  expect_equal(nrow(out), 3)
  expect_equal(out$value, c(-99, -96, -99))
  expect_equal(out$valLabel, c(NA, "missing", "missing"))
  expect_equal(out$missings, c("miss", "valid", "miss"))
  expect_equal(out$variable, c("VAR2", "VAR2", "VAR3"))
})


test_that("With NAs", {
  suppressMessages(suppressWarnings(dfSAV2 <- recode2NA(dfSAV, value = 1)))
  out <- checkEmptyValLabels(dfSAV2)
  expect_equal(out[[2]], checkEmptyValLabels(dfSAV)[[2]])
  expect_equal(out[[3]], checkEmptyValLabels(dfSAV)[[3]])
  expect_equal(out[[1]]$value, 1)
})


test_that("with specific value range", {
  out <- checkEmptyValLabels(dfSAV, valueRange = c(-97, -100))
  expect_equal(out[[1]], NULL)
  expect_equal(nrow(out[[2]]), 1)
  expect_equal(nrow(out[[3]]), 1)
})
