
mt2 <- data.frame(ID = 1:4, mc1 = c(1, 0, 0, 0), mc2 = c(0, 0, 0, 0), mc3 = c(1, 1, 1, 0), text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c(NA, "Franz", NA, "Ger"),stringsAsFactors = FALSE)
mt2_gads <- import_DF(mt2)
mt3_gads <- changeVarLabels(mt2_gads, varName = c("mc1", "mc2", "mc3"), varLabel = c("Lang: Eng", "Aus spoken", "other"))
df <- data.frame(v1 = c("j", "i", NA, NA),
                 v2 = c(NA, "i", NA, "k"),
                 v3 = c("j", NA, NA, "j"), stringsAsFactors = FALSE)

mt4 <- data.frame(text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c("Ger", "Franz", "Eng", NA), stringsAsFactors = FALSE)
mt4_gads <- import_DF(mt4)

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
  out <- remove2NAchar(mt4_gads, vars = namesGADS(mt4_gads), max_num = 1, na_value = -99)
  expect_equal(out$dat$text1, c(-99, -99, -99, "Aus2"))
  expect_equal(dim(out$dat), c(4, 1))

  out <- remove2NAchar(mt3_gads, vars = c("text1", "text2"), max_num = 1, na_value = -99)
  expect_equal(out$dat$text1, c(NA, -99, "Aus", -99))
  expect_equal(dim(out$dat), c(4, 5))
})

test_that("remove2NAchar and numeric variables", {
  out <- remove2NAchar(mt3_gads, vars = c("mc1", "mc2"), max_num = 1, na_value = -99)
  expect_equal(out$dat$mc1, c(-99, -99, -99, -99))
})

test_that("remove2NAchar max_num which exceeds number of strings", {
  out <- remove2NAchar(mt4_gads, vars = namesGADS(mt4_gads), max_num = 3, na_value = -99)
  expect_equal(out$dat, mt4_gads$dat)
})



#mc_vars <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "Eng"), label_by_hand = c("other" = "mc3"))
#out_gads <- collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3")
#out_gads2 <- multiChar2fac(out_gads, vars = c("text1_r", "text2_r"))
#remove_2NA_char(out_gads2, vars = c("text1", "text2"), max_num = 1, na_value = -99)

