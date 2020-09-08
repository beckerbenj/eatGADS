
################# Recode string to NA ---------------------------------------------------
mc <- as.factor(c("Ger", "other", NA, "Aus"))
mt <- data.frame(ID = 1:4, mc = mc, text = c(NA, "", "Aus", "Aus2"), stringsAsFactors = FALSE)
mt_gads <- import_DF(mt)

txt <- data.frame(ID = 1:4, var1 = c("", "Eng", "Aus", "Aus2"),
                   var2 = c("", "French", "Ger", "Ita"),
                   stringsAsFactors = FALSE)
txt_gads <- import_DF(txt)

test_that("Recodestring2NA", {
  mess <- capture_messages(out <- recodeString2NA(txt_gads))
  expect_equal(out$dat$var1, c(NA, "Eng", "Aus", "Aus2"))
  expect_equal(out$dat$var2, c(NA, "French", "Ger", "Ita"))
  expect_equal(out$labels, txt_gads$labels)
  expect_equal(mess[[1]], "Recodes in variable ID: 0\n")
  expect_equal(mess[[2]], "Recodes in variable var1: 1\n")
})

test_that("Recodestring2NA mixed data and missings in string", {
  mess2 <- capture_messages(out <- recodeString2NA(mt_gads))
  expect_equal(out$dat$text, c(NA, NA, "Aus", "Aus2"))
  expect_equal(mess2[[3]], "Recodes in variable text: 1\n")
})


test_that("Errors for Recodestring2NA", {
  expect_error(out <- recodeString2NA(txt_gads, string = c("", "la")), "'string' needs to be a character vector of exactly length 1.")
  expect_error(out <- recodeString2NA(mt_gads, string = c("1")), "'string' is labeled in at least one of the recodeVars.")
})



