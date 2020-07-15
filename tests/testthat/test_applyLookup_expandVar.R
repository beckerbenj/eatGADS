
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

  lookup3$value[1:2] <- NA
  expect_error(applyLookup_expandVar(l_gads, lookup3), "In more than 1 row value is missing.")

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

