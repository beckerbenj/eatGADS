
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
  out <- max_num_strings2NA(df2, max_num = 2, na_value = NA)
  expect_equal(as.character(out[1, ]), c(NA_character_, NA, NA))
  expect_equal(as.character(out[2, ]), c("i", "i", NA))
  expect_equal(as.character(out[4, ]), c(NA_character_, NA, NA))
})

test_that("Input errors", {
  expect_error(remove2NAchar(mt4_gads, vars = namesGADS(mt4_gads), max_num = 1:2, na_value = -99, na_label = "missing"),
               "'max_num' needs to be a single numeric value greater than 0.")
  expect_error(remove2NAchar(mt4_gads, vars = namesGADS(mt4_gads), max_num = "2", na_value = -99, na_label = "missing"),
               "'max_num' needs to be a single numeric value greater than 0.")
  expect_error(remove2NAchar(mt4_gads, vars = namesGADS(mt4_gads), max_num = 1, na_value = "-99", na_label = "missing"),
               "'na_value' needs to be a single numeric value.")
  expect_error(remove2NAchar(mt4_gads, vars = namesGADS(mt4_gads), max_num = 2, na_value = -99, na_label = c("m", 2)),
               "'na_label' needs to be a single character value.")
})

test_that("Too many strings to missing and NA coding", {
  mess_out <- capture_messages(out <- remove2NAchar(mt4_gads, vars = namesGADS(mt4_gads), max_num = 1, na_value = -99, na_label = "missing"))
  expect_equal(length(mess_out), 2)
  expect_equal(out$dat$text1, c(-99, -99, -99, "Aus2"))
  expect_equal(dim(out$dat), c(4, 1))
  expect_equal(out$labels$value, -99)
  expect_equal(out$labels$missings, "miss")
  expect_equal(out$labels$labeled, "yes")

  out <- remove2NAchar(mt3_gads, vars = c("text1", "text2"), max_num = 1, na_value = -99, na_label = "missing")
  expect_equal(out$dat$text1, c(NA, -99, "Aus", -99))
  expect_equal(dim(out$dat), c(4, 5))
  expect_equal(out$labels[5, "value"], -99)
  expect_equal(out$labels[5, "missings"], "miss")
})

test_that("remove2NAchar and numeric variables", {
  out <- remove2NAchar(mt3_gads, vars = c("mc1", "mc2"), max_num = 1, na_value = -99, na_label = "missing")
  expect_equal(out$dat$mc1, c(-99, -99, -99, -99))
})

test_that("remove2NAchar max_num which exceeds number of strings", {
  out <- remove2NAchar(mt4_gads, vars = namesGADS(mt4_gads), max_num = 3, na_value = -99, na_label = "missing")
  expect_equal(out$dat, mt4_gads$dat)
})


test_that("Text variables with (partial) missing codes", {
  mt4_gads2 <- mt4_gads
  for(new_text_var in namesGADS(mt4_gads2)) {
    mt4_gads2 <- changeValLabels(mt4_gads2, varName = new_text_var, value = -96, valLabel = "miss")
    mt4_gads2 <- changeMissings(mt4_gads2, varName = new_text_var, value = -96, missings = "miss")
    mt4_gads2 <- changeValLabels(mt4_gads2, varName = new_text_var, value = -99, valLabel = "miss")
    mt4_gads2 <- changeMissings(mt4_gads2, varName = new_text_var, value = -99, missings = "miss")
  }
  mt4_gads2$dat$text1 <- c(-99, "Eng", "Aus", -96)
  mt4_gads2$dat$text2 <- c(-99, -99, "Eng", -96)
  out <- remove2NAchar(mt4_gads2, vars = namesGADS(mt4_gads2), max_num = 1, na_value = -95, na_label = "missing")
  expect_equal(out$dat$text1, c(-99, "Eng", -95, -96))
  expect_equal(out$labels$varName, c("text1", "text1", "text1"))
  expect_equal(out$labels$value, c(-96, -99, -95))
  expect_equal(out$labels$missings, c("miss", "miss", "miss"))
})



