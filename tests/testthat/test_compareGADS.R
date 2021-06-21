
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

test_that("errors", {
  expect_error(compareGADS(df1, df1, varNames = "other"),
               "The following 'vars' are not variables in the GADSdat: other")
})

test_that("standard functionality", {
  df1_b <- df1
  df1_b$dat[, 2] <- c(9, 9)
  out <- compareGADS(df1, df1_b, varNames = namesGADS(df1))

  expect_equal(out[[1]], "all equal")
  expect_equal(out[[2]], data.frame(value = c("3", "5"), frequency = c(1, 1),
                                    valLabel = c(NA, NA), missings = c(NA, NA),
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

test_that("with character variable", {
  iris_char <- iris
  iris_char$Species <- as.character(iris_char$Species)
  suppressMessages(iris2 <- iris1 <- import_DF(iris_char))
  iris2$dat[c(1, 51, 52), 5] <- c(9, "test", "x")
  expect_silent(out <- compareGADS(iris1, iris2, varNames = namesGADS(iris1)))

  expect_equal(out[[1]], "all equal")
  expect_equal(out[[4]], "all equal")
  expect_equal(out[[5]], data.frame(value = c("setosa", "versicolor"), frequency = c(1, 2),
                                    valLabel = c(NA, NA), missings = c(NA, NA),
                                    stringsAsFactors = FALSE))
})

test_that("recodes in NA", {
  df1_c <- df1_b <- df1
  df1_b$dat[, 2] <- c(NA, NA)
  df1_c$dat[, 2] <- c(9, 9)
  out <- compareGADS(df1_b, df1_c, varNames = namesGADS(df1))

  expect_equal(out[[1]], "all equal")
  expect_equal(out[[2]], data.frame(value = c(NA_character_), frequency = c(2),
                                    valLabel = c(NA_character_), missings = c(NA_character_),
                                    stringsAsFactors = FALSE))

  df1_c <- df1_b <- df1
  df1_c$dat[, 2] <- c(NA, NA)
  df1_b$dat[, 2] <- c(9, 9)
  out2 <- compareGADS(df1_b, df1_c, varNames = namesGADS(df1))

  expect_equal(out2[[1]], "all equal")
  expect_equal(out2[[2]], data.frame(value = c("9"), frequency = c(2),
                                    valLabel = c(NA), missings = c(NA),
                                    stringsAsFactors = FALSE))

})

test_that("recodes in NA with labeled NAs", {
  ## with multiple NA labels but unique NA label for changed value
  dfSAV2 <- dfSAV
  dfSAV2$labels[c(1, 4:5), "value"] <- NA
  dfSAV3 <- dfSAV2
  dfSAV2$dat[1:2, 1] <- NA
  dfSAV2$dat[4, 1] <- 1
  expect_silent(out3 <- compareGADS(dfSAV2, dfSAV3, varNames = namesGADS(dfSAV3)))
  expect_equal(out3[[2]], "all equal")
  expect_equal(out3[["VAR1"]]$value, c("1", NA))
  expect_equal(out3[["VAR1"]]$frequency, c(1, 2))
  expect_equal(out3[["VAR1"]]$missings, c("valid", "miss"))

  ## with multiple NA labels and duplicated NA labels for changed value
  dfSAV4 <- dfSAV
  dfSAV4$dat[1:2, 1] <- NA
  dfSAV4$labels[c(1:2, 4:5), "value"] <- NA
  dfSAV5 <- dfSAV4
  dfSAV5$dat[1:2, 1] <- -99
  expect_error(out4 <- compareGADS(dfSAV4, dfSAV5, varNames = namesGADS(dfSAV5)),
               "Meta information on value level is not unique for variable: VAR1 and value: NA")

  ## with multiple NA labels and duplicated NA labels for changed value
  dfSAV4 <- dfSAV
  dfSAV4$dat[1:2, 1] <- -99
  dfSAV4$labels[c(1:2, 4:5), "value"] <- NA
  dfSAV5 <- dfSAV4
  dfSAV5$dat[1:2, 1] <- NA
  expect_silent(out5 <- compareGADS(dfSAV4, dfSAV5, varNames = namesGADS(dfSAV5)))
  expect_equal(out5[[2]], "all equal")
  expect_equal(out5[["VAR1"]], data.frame(value = "-99", frequency = 2, valLabel = NA, missings = NA,
                                          stringsAsFactors = FALSE))
})



test_that("Compare GADS data.frame and aggregate output", {
  dfSAV2 <- dfSAV
  # create duplicate changed values across variables
  dfSAV2$dat[1, 2:3] <- -96
  dfSAV2$labels[4, "missings"] <- "miss"
  dfSAV2$labels[c(4, 6), "value"] <- -96

  dfSAV_b <- dfSAV2
  dfSAV_b$dat[, 2] <- c(9, 9, 9, 9)
  dfSAV_b$dat[, 3] <- c(9, 9, 9, 9)

  out2 <- compareGADS(dfSAV2, dfSAV_b, varNames = namesGADS(dfSAV), output = "data.frame")
  expect_equal(out2, data.frame(variable = c(rep("VAR2", 2), rep("VAR3", 3)),
                                value = c("-96", "1", "-98", "-96", "1"), frequency = c(1, 3, 1, 1, 2),
                                          valLabel = c("missing", NA, NA, "missing", NA),
                                          missings = c("miss", NA, "miss", "miss", NA),
                                          stringsAsFactors = FALSE))
  out3 <- compareGADS(dfSAV2, dfSAV_b, varNames = namesGADS(dfSAV), output = "aggregated")
  expect_equal(out3, data.frame(value = c("-96", "1", "-98"),
                                valLabel = c("missing", NA, NA),
                                missings = c("miss", NA, "miss"),
                                stringsAsFactors = FALSE))
})

test_that("data.frame and aggregate output with all equals", {
  dfSAV2 <- dfSAV

  out2 <- compareGADS(dfSAV2, dfSAV, varNames = namesGADS(dfSAV), output = "data.frame")
  expect_equal(out2, "all equal")
  out3 <- compareGADS(dfSAV2, dfSAV, varNames = namesGADS(dfSAV), output = "aggregated")
  expect_equal(out3, "all equal")
})
