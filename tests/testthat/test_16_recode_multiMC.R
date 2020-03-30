
context("Recode multi mc based on text variables")


################# Match values and variables labels ---------------------------------------------------
mt2 <- data.frame(ID = 1:4, mc1 = c(1, 0, 0, 0), mc2 = c(0, 0, 0, 0), mc3 = c(0, 1, 1, 0), text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c(NA, "Franz", NA, "Ger"),stringsAsFactors = FALSE)
mt2_gads <- import_DF(mt2)
mt3_gads <- changeVarLabels(mt2_gads, varName = c("mc1", "mc2", "mc3"), varLabel = c("Lang: Eng", "Aus spoken", "other"))
df <- data.frame(v1 = c("j", "i", NA, NA),
                 v2 = c(NA, "i", NA, "k"),
                 v3 = c("j", NA, NA, "j"), stringsAsFactors = FALSE)

test_that("Basic errors in matchValues_varLabels", {
  expect_error(matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc4"), values = c("Ger", "Esp")), "The following vars are not a variable in the GADSdat:\nmc4")
  expect_error(matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2"), values = c("Ger", "Esp"), label_by_hand = c("other" = "mc3")), "All variable names in label_by_hand must be variables in mc_vars.")
})

test_that("Match values and variable names by variable labels", {
  out <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "Eng"), label_by_hand = c("other" = "mc3"))
  expect_equal(unname(out), c("mc1", "mc2", "mc3"))
  expect_equal(names(out), c("Eng", "Aus", "other"))

  # empty matches
  expect_error(matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Ger", "Esp", "Eng")), "The following mc_vars have not been assigned a value: mc2, mc3")
  out2 <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Ger", "Esp", "Eng"), label_by_hand = c("other" = "mc3", "Aus" = "mc2"))
  expect_equal(unname(out2), c("mc1", "mc2", "mc3"))
  expect_equal(names(out2), c("Eng", "Aus", "other"))

  expect_error(matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Ger", "Esp")), "The following mc_vars have not been assigned a value: mc1, mc2, mc3")

  #expect_equal(out$v2, c(NA, NA, NA, "k"))
})


################# Apply lookup: Expanding one text variable to multiple text variables  ---------------------------------------------------
lookup <- data.frame(variable = c("v1", "v1", "v2", "v2"),
                     value = c("a, b", "b, f", "a", "k, h"),
                     new_value1 = c("a", "b", "a", "k"),
                     new_value2 = c("b", "f", NA, "h"), stringsAsFactors = FALSE)
l <- data.frame(v1 = c("b, f", "b, f", "a, b"),
                     v2 = c("a", NA, "k, h"), stringsAsFactors = FALSE)
l_gads <- import_DF(l)


test_that("Errors for apply lookup with expanding into multiple variables", {
  lookup4 <- lookup2 <- lookup3 <- lookup
  names(lookup2)[1] <- "v"
  expect_error(applyLookup_expandVar(l_gads, lookup2), "LookUp table has to be formatted correctly.")

  lookup3$value[1] <- NA
  expect_error(applyLookup_expandVar(l_gads, lookup3), "In some rows there are missings in column value.")

  lookup4$new_value1[1] <- NA
  expect_warning(applyLookup_expandVar(l_gads, lookup4),)
})

test_that("Apply lookup with expanding into multiple variables", {
  out <- applyLookup_expandVar(l_gads, lookup)
  expect_equal(out$dat$v1_1, c("b", "b", "a"))
  expect_equal(out$dat$v1_2, c("f", "f", "b"))
  expect_equal(out$dat$v2_2, c(NA, NA, "h"))

  expect_equal(namesGADS(out), c("v1", "v2", "v1_1", "v1_2", "v2_1", "v2_2"))
})



################# Combine multi MC and text ---------------------------------------------------
test_that("Remove values from some variables", {
  out <- remove_values(df, vars = c("v1", "v2"), values = c("j", "i"))
  expect_equal(out$v1, c(NA_character_, NA, NA, NA))
  expect_equal(out$v2, c(NA, NA, NA, "k"))
  expect_equal(out$v3, c("j", NA, NA, "j"))
})


test_that("Left fill for text variables", {
  out <- left_fill(df)
  expect_equal(out$v1, c("j", "i", NA, "k"))
  expect_equal(out$v2, c("j", "i", NA, "j"))
  expect_equal(out$v3, c(NA_character_, NA, NA, NA))
})

test_that("Errors in combine multi mc and text", {
  mc_vars <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "other"))
  mt3_gads_err <- mt3_gads
  mt3_gads_err$dat[3, "text2"] <- "Aus"
  expect_error(collapseMultiMC_Text(mt3_gads_err, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3"), "Duplicate values in row 3.")

  expect_error(collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = c("mc3", "mc1")), "mc_var_4text needs to be a character of lenth one.")

  expect_error(collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars[1:2], text_vars = c("text1", "text2"), mc_var_4text = c("mc3")), "mc_var_4text is not part of mc_vars.")
})


test_that("Combine multi mc and text", {
  mc_vars <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "other"))
  test <- collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3")

  expect_equal(test$dat$text1_r, c(NA, "Franz", NA, "Aus2"))
  expect_equal(test$dat$text2_r, c(NA_character_, NA, NA, "Ger"))
  expect_equal(test$dat$text1, c(NA, "Eng", "Aus", "Aus2"))
  expect_equal(test$dat$mc1_r, c(1, 1, 0, 0))
  expect_equal(test$dat$mc2_r, c(0, 0, 1, 0))
  expect_equal(test$labels[test$labels$varName == "text1_r", "varLabel"], "(recoded)")
  expect_equal(test$labels[test$labels$varName == "mc1_r", "varLabel"], "Lang: Eng (recoded)")

  test2 <- collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3", var_suffix = "", label_suffix = "")
  expect_equal(test2$dat$text1, c(NA, "Franz", NA, "Aus2"))
  expect_equal(test2$dat$text2, c(NA_character_, NA, NA, "Ger"))
  expect_equal(test2$labels[test2$labels$varName == "mc1", "varLabel"], "Lang: Eng")

  mt3_gads_1 <- mt3_gads
  mt3_gads_1$dat$text2[4] <- NA
  expect_warning(test <- collapseMultiMC_Text(mt3_gads_1, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3"), "In the new variable text2_r all values are missing, therefore the variable is dropped. If this behaviour is not desired, contact the package author.")
  expect_false("text2_r" %in% namesGADS(test))
})


################# mulitple Characters to factors with identical labels ---------------------------------------------------
mt4 <- data.frame(text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c("Ger", "Franz", "Eng", NA), stringsAsFactors = FALSE)
mt4_gads <- import_DF(mt4)

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


################# count character variables and remove overflowing while coding NA ---------------------------------------------------
test_that("Too many strings to missing", {
  df2 <- left_fill(df)
  df2$v3 <- c("a", NA, NA, "b")
  out <- max_num_strings2NA(df2, vars = names(df), max_num = 2, na_value = NA)
  expect_equal(as.character(out[1, ]), c(NA_character_, NA, NA))
  expect_equal(as.character(out[2, ]), c("i", "i", NA))
  expect_equal(as.character(out[4, ]), c(NA_character_, NA, NA))
})


test_that("Too many strings to missing and NA coding", {
  out <- remove_2NA_char(mt4_gads, vars = namesGADS(mt4_gads), max_num = 1, na_value = -99)
  expect_equal(out$dat$text1, c(-99, -99, -99, "Aus2"))
  expect_equal(dim(out$dat), c(4, 1))

  out <- remove_2NA_char(mt3_gads, vars = c("text1", "text2"), max_num = 1, na_value = -99)
  expect_equal(out$dat$text1, c(NA, -99, "Aus", -99))
  expect_equal(dim(out$dat), c(4, 5))
})






#mc_vars <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "Eng"), label_by_hand = c("other" = "mc3"))
#out_gads <- collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3")
#out_gads2 <- multiChar2fac(out_gads, vars = c("text1_r", "text2_r"))
#remove_2NA_char(out_gads2, vars = c("text1", "text2"), max_num = 1, na_value = -99)

