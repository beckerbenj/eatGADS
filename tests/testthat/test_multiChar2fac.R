
################# multiple Characters to factors with identical labels ---------------------------------------------------
mt4 <- data.frame(text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c("Ger", "Franz", "Eng", NA), stringsAsFactors = FALSE)
mt4_gads <- import_DF(mt4)

mt5 <- data.frame(text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c("Ger", "Franz", "Eng", NA),
                  other_text = factor(c("apple", "pear", NA, NA)), stringsAsFactors = FALSE)
mt5_gads <- import_DF(mt5)


mt4_gads_2 <- changeVarLabels(mt4_gads, varName = "text1", varLabel = "text var 1")
mt4_gads_2$dat[1, "text1"] <- -99
mt4_gads_2$labels[1, c("value")] <- c(-99)
mt4_gads_2$labels[1, c("valLabel")] <- c("missing")
mt4_gads_2$labels[1, c("labeled")] <- c("yes")
mt4_gads_2 <- checkMissings(mt4_gads_2, missingLabel = "missing")

test_that("Multiple text variables to factors", {
  out <- multiChar2fac(mt4_gads, vars = namesGADS(mt4_gads))
  expect_equal(out$dat$text1_r, c(NA, 3, 1, 2))
  expect_equal(out$dat$text2_r, c(5, 4, 3, NA))
  expect_equal(out$labels[out$labels$varName == "text1_r", "value"], out$labels[out$labels$varName == "text2_r", "value"])
  expect_equal(out$labels[out$labels$varName == "text1_r", "valLabel"], c("Aus", "Aus2", "Eng", "Franz", "Ger"))

  out2 <- multiChar2fac(mt4_gads, vars = namesGADS(mt4_gads), var_suffix = "")
  expect_equal(out2$dat$text1, c(NA, 3, 1, 2))
  expect_equal(out2$dat$text2, c(5, 4, 3, NA))
  expect_equal(out2$labels[out$labels$varName == "text1", "varLabel"][1], "(recoded)")
})

test_that("Multiple text variables to factors, keeping var and missing codes", {
  out <- multiChar2fac(mt4_gads_2, vars = namesGADS(mt4_gads))
  expect_equal(unique(out$labels[out$labels$varName == "text1_r", "varLabel"]), "text var 1 (recoded)")
  expect_equal(unique(out$labels[out$labels$varName == "text1_r", "value"])[1], -99)
  expect_equal(out$dat$text1_r, c(-99, 3, 1, 2))
})

test_that("Multiple text variables to factors, change spss.format", {
  mt4_gads_2$labels[1, "format"] <- "A50"
  out <- multiChar2fac(mt4_gads_2, vars = namesGADS(mt4_gads))

  expect_silent(check_var_type(out))
})

test_that("Multiple text variables to factors with other factor in data set", {
  out <- multiChar2fac(mt5_gads, vars = namesGADS(mt4_gads))
  expect_equal(out$dat$text1_r, c(NA, 3, 1, 2))
  expect_equal(out$dat$text2_r, c(5, 4, 3, NA))
  expect_equal(out$labels[out$labels$varName == "text1_r", "value"], out$labels[out$labels$varName == "text2_r", "value"])
  expect_equal(out$labels[out$labels$varName == "text1_r", "valLabel"], c("Aus", "Aus2", "Eng", "Franz", "Ger"))
})

test_that("Partially labeled variable", {
  mt5_gads <- changeValLabels(mt4_gads, varName = "text1", value = -99, valLabel = "Austria")
  mt5_gads <- changeMissings(mt5_gads, varName = "text1", value = -99, missings =  "valid")
  mt5_gads$dat[4, 1] <- -99
  out <- multiChar2fac(mt5_gads, vars = namesGADS(mt5_gads))

  expect_equal(dim(out$dat), c(4, 4))
  expect_equal(out$dat[[1]], c(NA, "Eng", "Aus", -99))
  expect_equal(out$dat[[3]], c(NA, 2, 1, -99))
  meta <- extractMeta(out, "text1_r")
  expect_equal(meta$value, c(-99, 1:4))
  expect_equal(meta$valLabel, c("Austria", "Aus", "Eng", "Franz", "Ger"))
})
