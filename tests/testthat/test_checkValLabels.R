
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

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
  expect_error(checkMissingValLabels(df1, vars = 3:4),
               "The following 'vars' are not variables in the GADSdat: 3, 4")
  expect_error(checkMissingValLabels(df1, valueRange = 3:5),
               "'valueRange' needs to be a numeric vector of length 2.")
  expect_error(checkMissingValLabels(df1, valueRange = letters[3:4]),
               "'valueRange' needs to be a numeric vector of length 2.")
})

test_that("give_GADSdat_classes", {
  out <- give_GADSdat_classes(dfSAV)
  expect_equal(out, c(VAR1 = "integer", VAR2 = "integer", VAR3 = "integer"))

  suppressMessages(dfSAV_mini <- extractVars(dfSAV, vars = "VAR1"))
  out2 <- give_GADSdat_classes(dfSAV_mini)
  expect_equal(out2, c(VAR1 = "integer"))

  out3 <- give_GADSdat_classes(iris_g)
  expect_equal(out3, c(Sepal_Length = "double", Sepal_Width = "double",
                      Petal_Length = "double", Petal_Width = "double",
                      Species = "integer", charVar = "character"))

  out4 <- give_GADSdat_classes(pisa, vars = c("g8g9", "age"))
  expect_equal(out4, c(g8g9 = "integer", age = "double"))
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

test_that("checkMissingValLabels", {
  out <- checkMissingValLabels(dfSAV)
  expect_equal(names(out), paste0("VAR", 1:3))
  expect_equal(names(out[[1]]), c("varLabel", "missing_labels"))
  expect_equal(names(out[[2]]), c("varLabel", "missing_labels"))
  expect_equal(out[[1]]$varLabel, "Variable 1")
  expect_equal(out[[3]]$varLabel, "Variable 3")
  expect_equal(out[[1]]$missing_labels, 2)
  expect_equal(out[[2]]$missing_labels, 1)

  dfSAV2 <- removeValLabels(dfSAV, "VAR1", value = c(-99, -96, 1))
  out2 <- checkMissingValLabels(dfSAV2)
  expect_equal(out2[[1]]$missing_labels, c(-99, -96, 1, 2))

  dfSAV3 <- changeValLabels(dfSAV, "VAR1", value = c(2), valLabel = "test")
  out3 <- checkMissingValLabels(dfSAV3)
  expect_equal(out3[[1]], NULL)

  out4 <- checkMissingValLabels(iris_g)
  expect_equal(out4, list(Species = NULL))

  out5 <- checkMissingValLabels(pisa, vars = c("g8g9", "age"))
  expect_equal(out5, list(g8g9 = NULL))
})

test_that("checkMissingValLabels data.frame", {
  out <- checkMissingValLabels(dfSAV, output = "data.frame")
  expect_equal(names(out), c("variable", "varLabel", "number_of_missing_labels", "values_with_missing_labels"))
  expect_equal(out$variable, paste0("VAR", 1:3))
  expect_equal(out$number_of_missing_labels, rep(1, 3))
  expect_equal(out$values_with_missing_labels, c("2", "1", "1"))

  dfSAV2 <- removeValLabels(dfSAV, "VAR1", value = c(-99, -96, 1))
  out2 <- checkMissingValLabels(dfSAV2, output = "data.frame")
  expect_equal(out2$number_of_missing_labels, c(4, 1, 1))
  expect_equal(out2$values_with_missing_labels, c("-99, -96, 1, 2", "1", "1"))

  dfSAV3 <- changeValLabels(dfSAV, "VAR1", value = c(2), valLabel = "test")
  out3 <- checkMissingValLabels(dfSAV3, output = "data.frame")
  expect_equal(out3$variable, c("VAR2", "VAR3"))

  dfLong <- data.frame(v1 = factor(letters[1:12]))
  gLong <- import_DF(dfLong)
  gLong <- removeValLabels(gLong, "v1", value = 1:12)
  out4 <- checkMissingValLabels(gLong, output = "data.frame")
  expect_equal(out4$values_with_missing_labels, paste0(paste(1:10, collapse = ", "), ", ..."))
})


test_that("With NAs", {
  suppressMessages(dfSAV2 <- recode2NA(dfSAV, value = 1))
  out <- checkEmptyValLabels(dfSAV2)
  expect_equal(out[[2]], checkEmptyValLabels(dfSAV)[[2]])
  expect_equal(out[[3]], checkEmptyValLabels(dfSAV)[[3]])
  expect_equal(out[[1]]$value, 1)

  out <- checkMissingValLabels(dfSAV2)
  expect_equal(out[[1]], checkMissingValLabels(dfSAV)[[1]])
  expect_equal(out[[2]], NULL)
  expect_equal(out[[3]], NULL)
})


test_that("with specific value range", {
  out <- checkEmptyValLabels(dfSAV, valueRange = c(-97, -100))
  expect_equal(out[[1]], NULL)
  expect_equal(nrow(out[[2]]), 1)
  expect_equal(nrow(out[[3]]), 1)

  out2 <- checkMissingValLabels(dfSAV, valueRange = c(3, -100))
  expect_equal(out2, checkMissingValLabels(dfSAV))

  out3 <- checkMissingValLabels(dfSAV, valueRange = c(1, -100))
  expect_equal(out3[[2]], out2[[2]])
  expect_equal(out3[[1]], NULL)
})
