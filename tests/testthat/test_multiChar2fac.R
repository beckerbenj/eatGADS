
################# multiple Characters to factors with identical labels ---------------------------------------------------
mt4 <- data.frame(text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c("Ger", "Franz", "Eng", NA), stringsAsFactors = FALSE)
mt4_gads <- import_DF(mt4)

mt5 <- data.frame(text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c("Ger", "Franz", "Eng", NA),
                  other_text = factor(c("apple", "pear", NA, NA)), stringsAsFactors = FALSE)
mt5_gads <- import_DF(mt5)


mt4_gads_2 <- changeVarLabels(mt4_gads, varName = "text1", varLabel = "text var 1")
mt4_gads_2$dat[1, "text1"] <- -99
mt4_gads_2$labels[1:2, c("value")] <- c(-99)
mt4_gads_2$labels[1:2, c("valLabel")] <- c("missing")
mt4_gads_2$labels[1:2, c("labeled")] <- c("yes")
mt4_gads_2$labels[1:2, c("missings")] <- c("miss")

test_that("errors", {
  mt4_gads_3 <- changeMissings(mt4_gads_2, "text2", value = -99, missings = "valid")
  expect_error(multiChar2fac(mt4_gads_3, vars = namesGADS(mt4_gads_2)),
               "Meta data on value level ('value', 'valLabel', 'missings') of variables 'text1' and 'text2' must be identical.",
               fixed = TRUE)
})


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

test_that("Single text variable to factors with 1 as missing", {
  mt4_gads_3 <- recodeGADS(mt4_gads_2, varName = "text1", oldValues = -99, newValues = 1)
  out <- multiChar2fac(mt4_gads_3, vars = "text1")
  expect_equal(out$dat$text1_r, c(1, 4, 2, 3))
  expect_equal(out$labels[out$labels$varName == "text1_r", "valLabel"], c("missing", "Aus", "Aus2", "Eng"))
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
  mt5_gads <- mt4_gads_2
  for(nam in namesGADS(mt5_gads)) {
    mt5_gads <- recodeGADS(mt5_gads, varName = nam, oldValues = -99, newValues = 1)
    mt5_gads <- changeValLabels(mt5_gads, varName = nam, value = 1, valLabel = "Austria")
    mt5_gads <- changeMissings(mt5_gads, varName = nam, value = 1, missings =  "valid")
  }
  out <- multiChar2fac(mt5_gads, vars = namesGADS(mt5_gads))

  expect_equal(dim(out$dat), c(4, 4))
  expect_equal(out$dat[[1]], c(1, "Eng", "Aus", "Aus2"))
  expect_equal(out$dat[[3]], c(1, 4, 2, 3))
  meta <- extractMeta(out, "text1_r")
  expect_equal(meta$value, c(1:6))
  expect_equal(meta$valLabel, c("Austria", "Aus", "Aus2", "Eng", "Franz", "Ger"))
})

test_that("with convertCases", {
  expect_error(multiChar2fac(mt4_gads, vars = namesGADS(mt4_gads), convertCases = 1:2),
               "'convertCases' must be a character of length 1.")
  expect_error(multiChar2fac(mt4_gads, vars = namesGADS(mt4_gads), convertCases = "middle"),
               "'convertCases' must one of c('lower', 'upper', 'upperFirst').", fixed = TRUE)
  out <- multiChar2fac(mt4_gads, vars = namesGADS(mt4_gads), convertCases = "lower")
  expect_equal(out$dat$text1_r, c(NA, 3, 1, 2))
  expect_equal(out$dat$text2_r, c(5, 4, 3, NA))
  expect_equal(out$labels[out$labels$varName == "text1_r", "value"], out$labels[out$labels$varName == "text2_r", "value"])
  expect_equal(out$labels[out$labels$varName == "text1_r", "valLabel"], c("aus", "aus2", "eng", "franz", "ger"))
})
