

# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")


df5 <- df4 <- df3 <- df1
df3$labels[1, "varLabel"] <- paste(rep("a", 256), collapse = "")
df3$labels[2, "varLabel"] <- paste(rep("a", 257), collapse = "")

test_that("Check length of variable labels", {
  out <- check4SPSS(df3)
  expect_equal(out$varLabels, c("V1"))
  expect_equal(out$valLabels, character())
 # expect_equal(out$missings, "Functionality not yet implemented.")
})


df4 <- changeValLabels(df4, "ID1", value = 1, valLabel = paste(rep("a", 121), collapse = ""))
df4 <- changeValLabels(df4, "V1", value = 1, valLabel = paste(rep("3", 125), collapse = ""))

test_that("Check length of value labels", {
  out <- check4SPSS(df4)
  expect_equal(out$valLabels, c("ID1", "V1"))
  expect_equal(out$varLabels, character())
 # expect_equal(out$missings, "Functionality not yet implemented.")
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

