

# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")


df7 <- df6 <- df5 <- df4 <- df3 <- df1

test_that("Check length of variable labels", {
  df6 <- changeVarNames(df6, oldNames = c("ID1", "V1"),
                        c(paste(rep("a", 64), collapse = ""),
                        paste(rep("a", 65), collapse = "")))

  out <- check4SPSS(df6)
  expect_equal(out$varNames_special, character())
  expect_equal(out$varNames_length, paste(rep("a", 65), collapse = ""))
})


test_that("Check length of variable labels", {
  df3$labels[1, "varLabel"] <- paste(rep("a", 256), collapse = "")
  df3$labels[2, "varLabel"] <- paste(rep("a", 257), collapse = "")

  out <- check4SPSS(df3)
  expect_equal(out$varLabels, c("V1"))
  expect_equal(out$valLabels, character())
  expect_equal(out$missings, character())
})


test_that("Check length of value labels", {
  df4 <- changeValLabels(df4, "ID1", value = 1, valLabel = paste(rep("a", 121), collapse = ""))
  df4 <- changeValLabels(df4, "V1", value = 99, valLabel = paste(rep("3", 125), collapse = ""))

  out <- check4SPSS(df4)
  expect_equal(out$valLabels, list(ID1 = 1, V1 = 99))
  expect_equal(out$varLabels, character())
  expect_equal(out$missings, character())
})

test_that("Check length of variable labels with unicode", {
  df5$labels[1, "varLabel"] <- paste(paste0("ö", 1:66), collapse = "")
  df5$labels[2, "varLabel"] <- paste(paste0("a", 1:88), collapse = "")

  out <- check4SPSS(df5)
  expect_equal(out$varLabels, character())

  df5$labels[1, "varLabel"] <- paste(paste0("ö", 1:67), collapse = "")
  df5$labels[2, "varLabel"] <- paste(paste0("a", 1:89), collapse = "")

  out <- check4SPSS(df5)
  expect_equal(out$varLabels, c("ID1", "V1"))
})

test_that("Check many missing codes", {
  df7 <- changeValLabels(df7, "ID1", value = 1:5, valLabel = 1:5)
  df7 <- changeValLabels(df7, "V1", value = 1:5, valLabel = 1:5)
  df7 <- changeMissings(df7, "ID1", value = 1:5, c("miss", "miss", "valid", "miss", "miss"))
  df7 <- changeMissings(df7, "V1", value = 1:5, c("miss", "miss", "valid", "miss", "miss"))
  df7$dat$V1 <- as.character(df7$dat$V1)

  out <- check4SPSS(df7)
  expect_equal(out$valLabels, character())
  expect_equal(out$varLabels, character())
  expect_equal(out$missings, "V1")
})
