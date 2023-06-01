# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

mc <- as.factor(c("Ger", "other", NA, "Aus"))
mt <- data.frame(ID = 1:4, mc = mc, text = c(NA, "", "Aus", "Aus2"), stringsAsFactors = FALSE)
mt_gads <- import_DF(mt)

txt <- data.frame(ID = 1:4, var1 = c("", "Eng", "Aus", "Aus2"),
                  var2 = c("", "French", "Ger", "Ita"),
                  stringsAsFactors = FALSE)
txt_gads <- import_DF(txt)


test_that("Errors", {
  expect_error(recodeNA2missing(dfSAV, value = c()),
               "'value' needs to be a numeric vector of length 1.")
  expect_error(recodeNA2missing(dfSAV, recodeVar = 1, value = 1),
               "'recodeVars' needs to be character vector of at least length 1.")
  expect_error(recodeNA2missing(dfSAV, recodeVar = "test1", value = 1),
               "The following 'recodeVars' are not variables in the GADSdat: test1")
})

test_that("RecodeNA2missing", {
  out <- recodeNA2missing(mt_gads, valLabel = "miss test", value = -90)
  expect_equal(out$dat$text, c(-90, "", "Aus", "Aus2"))
  expect_equal(out$dat$mc, c(2, 3, -90, 1))
  expect_equal(out$labels$value, c(NA, -90, 1, 2, 3, -90))
  expect_equal(out$labels$valLabel, c(NA, "miss test", "Aus", "Ger", "other", "miss test"))
  #expect_equal(mess[[1]], "Recodes in variable ID: 0\n")
})

test_that("Recode2NA mixed data and missings in string", {
  mess2 <- capture_messages(out <- recode2NA(mt_gads))
  expect_equal(out$dat$text, c(NA, NA, "Aus", "Aus2"))
  expect_equal(mess2[[3]], "Recodes in variable text: 1\n")

  mess3 <- capture_messages(out2 <- recode2NA(mt_gads, value = c("", "Aus")))
  expect_equal(out2$dat$text, c(NA, NA, NA, "Aus2"))
  expect_equal(mess3[[3]], "Recodes in variable text: 2\n")
})

test_that("Recode2NA numerics", {
  dfSAV2 <- dfSAV
  dfSAV2$dat[2:3, "VAR1"] <- NA
  dfSAV2$dat[1, "VAR3"] <- NA
  expect_warning(out <- recodeNA2missing(dfSAV2, recodeVars =  "VAR1", value = 1),
                 "'value' is already labeled for the following variable in 'recodeVars': VAR1")
  expect_equal(out$dat$VAR1, c(1, 1, 1, 2))

  warns <- capture_warnings(out2 <- recodeNA2missing(dfSAV2, value = -99, valLabel = "miss test"))
  expect_equal(warns[1], "'value' is already labeled for the following variable in 'recodeVars': VAR1")
  expect_equal(warns[2], "'value' is already labeled for the following variable in 'recodeVars': VAR3")
  expect_equal(out2$dat$VAR1, c(1, -99, -99, 2))
  expect_equal(out2$dat$VAR2, dfSAV2$dat$VAR2)
  expect_equal(out2$dat$VAR3, c(-99, 1, 1, -98))
})

