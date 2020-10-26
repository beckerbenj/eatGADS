

mt2 <- data.frame(ID = 1:5, mc1 = c(1, 0, 0, 0, 0),
                  mc2 = c(0, 0, 0, 0, 0), mc3 = c(1, 1, 1, 0, 0),
                  text1 = c(NA, "Eng", "Aus", "Aus2", -99), text2 = c(NA, "Franz", NA, "Ger", -99),
                  stringsAsFactors = FALSE)
mt2_gads <- import_DF(mt2)
mt3_gads <- changeVarLabels(mt2_gads, varName = c("mc1", "mc2", "mc3"), varLabel = c("Lang: Eng", "Aus spoken", "other"))
mt3_gads <- changeMissings(mt3_gads, varName = "text1", value = -99, missings = "miss")
mt3_gads <- changeMissings(mt3_gads, varName = "text2", value = -99, missings = "miss")
df <- data.frame(v1 = c("j", "i", NA, NA),
                 v2 = c(NA, "i", NA, "k"),
                 v3 = c("j", NA, NA, "j"), stringsAsFactors = FALSE)

lookup <- data.frame(variable = c("v1", "v1", "v2", "v2"),
                     value = c("a, b", "b, f", "a", "k, h"),
                     new_value1 = c("a", "b", "a", "k"),
                     new_value2 = c("b", "f", NA, "h"), stringsAsFactors = FALSE)
l <- data.frame(v1 = c("b, f", "b, f", "a, b"),
                v2 = c("a", NA, "k, h"), stringsAsFactors = FALSE)
l_gads <- import_DF(l)



################# Combine multi MC and text ---------------------------------------------------
test_that("Errors wrong codes MC variables",{
  expect_error(check_01_mc_in_gadsdat(l_gads, "v1"),
               "MC variables must be coded 0 (no) and 1 (yes):\nv1 contains values: a, b, b, f", fixed = TRUE)
  expect_error(check_01_mc_in_gadsdat(l_gads, c("v1", "v2")),
               "MC variables must be coded 0 (no) and 1 (yes):\nv1 contains values: a, b, b, f\nv2 contains values: a, k, h", fixed = TRUE)

  mc_gads <- import_DF(data.frame(ID = 1:3, mc = c(0, 2, 2)))
  expect_error(check_01_mc_in_gadsdat(mc_gads, "mc"),
               "MC variables must be coded 0 (no) and 1 (yes):\nmc contains values: 0, 2", fixed = TRUE)
})


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
  expect_error(collapseMultiMC_Text(mt3_gads_err, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3"),
               "Duplicate values in row 3.")

  expect_error(collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"),
                                    mc_var_4text = c("mc3", "mc1")),
               "mc_var_4text needs to be a character of lenth one.")

  expect_error(collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars[1:2], text_vars = c("text1", "text2"),
                                    mc_var_4text = c("mc3")),
               "mc_var_4text is not part of mc_vars.")
})


test_that("Combine multi mc and text", {
  mc_vars <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "other"))
  test <- collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3")

  expect_equal(test$dat$text1_r, c(-99, "Franz", -98, "Aus2", -99))
  expect_equal(test$dat$text2_r, c(-99, -99, -98, "Ger", -99))
  expect_equal(test$dat$text1, c(NA, "Eng", "Aus", "Aus2", -99))
  expect_equal(test$dat$mc1_r, c(1, 1, 0, 0, 0))
  expect_equal(test$dat$mc2_r, c(0, 0, 1, 0, 0))
  expect_equal(test$dat$mc3_r, c(1, 1, 0, 1, 0)) ### should be recoded by function according to left over fields!
  expect_equal(test$labels[test$labels$varName == "text1_r", "varLabel"], c("(recoded)", "(recoded)"))
  expect_equal(test$labels[test$labels$varName == "text1_r", "value"], c(-99, -98))
  expect_equal(test$labels[test$labels$varName == "text1_r", "missings"], c("miss", "miss"))
  expect_equal(test$labels[test$labels$varName == "text1_r", "valLabel"], c("Missing: By intention", "Missing: Invalid response"))
  expect_equal(test$labels[test$labels$varName == "text2_r", "value"], c(-99, -98))
  expect_equal(test$labels[test$labels$varName == "text2_r", "valLabel"], c("Missing: By intention", "Missing: Invalid response"))
  expect_equal(test$labels[test$labels$varName == "mc1_r", "varLabel"], "Lang: Eng (recoded)")

  test2 <- collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3", var_suffix = "", label_suffix = "")
  expect_equal(test2$dat$text1, c(-99, "Franz", -98, "Aus2", -99))
  expect_equal(test2$dat$text2, c(-99, -99, -98, "Ger", -99))
  expect_equal(test2$labels[test2$labels$varName == "mc1", "varLabel"], "Lang: Eng")

  mt3_gads_1 <- mt3_gads
  mt3_gads_1$dat$text2[4] <- NA
  expect_warning(test <- collapseMultiMC_Text(mt3_gads_1, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3"),
                 "In the new variable text2_r all values are missing, therefore the variable is dropped. If this behaviour is not desired, contact the package author.")
  expect_false("text2_r" %in% namesGADS(test))
})

test_that("Combine multi mc and text with empty text variables", {
  mc_vars <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "other"))
  mtE_gads <- mt3_gads
  mt3_gads$dat <- mt3_gads$dat[-5, ]
  mtE_gads$dat[1, c("text1", "text2")] <- c("", "")
  mtE_gads$dat[4, c("text1", "text2")] <- c("Aus", NA)
  mtE_gads$dat[5, 1:4] <- c(5, 0, 0, -99)
  mtE_gads$dat[5, c("text1", "text2")] <- c("Aus", NA)
  mtE_gads <- changeMissings(mtE_gads, varName = "mc3", value = -99, "miss")

  test <- collapseMultiMC_Text(mtE_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3")

  expect_equal(as.character(test$dat[1, c("text1", "text2")]), c("", ""))
  expect_equal(as.character(test$dat[1, c("text1_r", "text2_r")]), c("", ""))
  # if other 1 and text variable originally empty, keep 1 in other
  expect_equal(as.numeric(test$dat[1, c("mc3", "mc3_r")]), c(1, 1))

  # if other 1 and text variable with values which move into mcs, set other to 0 (or stay 0); specific missings codes
  expect_equal(as.numeric(test$dat[3, c("mc3", "mc3_r")]), c(1, 0))
  expect_equal(as.numeric(test$dat[3, c("text1_r", "text2_r")]), c(-98, -98))
  expect_equal(as.numeric(test$dat[4, c("mc3", "mc3_r")]), c(0, 0))
  expect_equal(as.numeric(test$dat[4, c("text1_r", "text2_r")]), c(-98, -98))
  expect_equal(as.numeric(test$dat[5, c("mc3", "mc3_r")]), c(-99, 0))
  expect_equal(as.numeric(test$dat[5, c("text1_r", "text2_r")]), c(-98, -98))
})

test_that("Combine multi mc and text with keeping mc other as is", {
  mc_vars <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "other"))
  mtO_gads <- mt3_gads
  mtO_gads$dat <- mtO_gads$dat[1:4, ]
  mtO_gads$dat[c(1, 2), c("mc3")] <- c(0, -94)
  mtO_gads$dat[2, c("text1", "text2")] <- c(NA, NA)
  mtO_gads <- changeMissings(mtO_gads, varName = "mc3", value = -94, missings = "miss")

  test <- collapseMultiMC_Text(mtO_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3")
  expect_equal(as.numeric(test$dat[, c("mc3_r")]), c(0, -94, 0, 1))
})




