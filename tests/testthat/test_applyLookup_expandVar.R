
################# Apply lookup: Expanding one text variable to multiple text variables  ---------------------------------------------------
l <- data.frame(v1 = c("b, f", "b, f", "a, b"),
                v2 = c("a", NA, "k, h"), stringsAsFactors = FALSE)
l_gads <- import_DF(l)

lookup <-  createLookup(l_gads, recodeVars = namesGADS(l_gads), addCols = c("new_value1", "new_value2"))
lookup[, "new_value1"] <- c("b", "a", "a", NA, "k")
lookup[, "new_value2"] <- c("f", "b", NA, NA, "h")



test_that("Errors for apply lookup with expanding into multiple variables", {
  lookup4 <- lookup2 <- lookup3 <- lookup
  names(lookup2)[1] <- "v"
  expect_error(applyLookup_expandVar(l_gads, lookup2),
               "'lookup' table has to be formatted correctly.")

  lookup4$new_value1[1] <- NA
  expect_warning(applyLookup_expandVar(l_gads, lookup4),
                 "Not all values have a recode value assigned (missings in value_new).", fixed = TRUE)
})

test_that("Warnings for missings in first recode column (apply lookup with expanding into multiple variables)", {
  lookup3 <- lookup
  lookup3[1, 3] <- NA
  expect_warning(applyLookup_expandVar(l_gads, lookup3),
                 "Not all values have a recode value assigned (missings in value_new).", fixed = TRUE)

})

test_that("Apply lookup with expanding into multiple variables", {
  expect_warning(out <- applyLookup_expandVar(l_gads, lookup),
                 "Not all values have a recode value assigned (missings in value_new).", fixed = TRUE)
  expect_equal(out$dat$v1_1, c("b", "b", "a"))
  expect_equal(out$dat$v1_2, c("f", "f", "b"))
  expect_equal(out$dat$v2_2, c(NA, NA, "h"))
  expect_equal(namesGADS(out), c("v1", "v2", "v1_1", "v1_2", "v2_1", "v2_2"))

  lookup6 <- lookup
  lookup6[4, "new_value1"] <- -99
  expect_silent(suppressMessages(out <- applyLookup_expandVar(l_gads, lookup6)))

})


test_that("Warnings for empty strings", {
  lookup5 <- lookup
  lookup5[4, "new_value1"] <- -94
  l_gads2 <- l_gads
  l_gads2$dat[2, "v2"] <- ""

  warns <- capture_warnings(out <- applyLookup_expandVar(l_gads2, lookup5))
  expect_equal(warns[3],
               "Empty strings are values in the data but not in the look up table. Using recodeString2NA() is recommended.",
               fixed = TRUE)

})

test_that("Warnings for mismatch data and lookup table", {
  lookup6 <- lookup
  lookup6[1:2, "value"] <- c("some string1", "some string2")

  warns <- capture_warnings(out <- applyLookup_expandVar(l_gads, lookup6))
  expect_equal(warns[2],
               "For variable v1 the following values are in the lookup table but not in the data: some string1, some string2")
  expect_equal(warns[3],
               "For variable v1 the following values are in the data but not in the lookup table: b, f, a, b")

})
