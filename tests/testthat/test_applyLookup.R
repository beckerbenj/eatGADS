################# Recode based on lookup table ---------------------------------------------------

# testM <- import_spss("tests/testthat/helper_spss_missings.sav")
testM <- import_spss("helper_spss_missings.sav")

lu1 <- createLookup(testM, recodeVars = c("VAR1", "VAR2"), addCols = c("r1", "r2"))
lu2 <- createLookup(testM, recodeVars = c("VAR1"), sort_by = "value")
lu3 <- createLookup(testM, recodeVars = c("VAR1", "VAR2"), sort_by = "value")

test_that("Check Lookup, errors and warnings",{
  expect_error(check_lookup(lu2, testM), "No values have a recode value assigned (missings in value_new).", fixed = TRUE)

  lu2_3 <- lu2_2 <- lu2_1 <- lu2
  lu2_1[1, 1] <- "v10"
  expect_error(check_lookup(lu2_1, testM), "Some of the variables are not variables in the GADSdat.")
  lu2_2[1, 2] <- NA
  lu2_2[1, 3] <- 1
  expect_silent(suppressWarnings(check_lookup(lu2_2, testM)))
  lu2_2[2, 2] <- NA
  expect_error(check_lookup(lu2_2, testM), "In more than 1 row value is missing.")

  lu2_3[1, 3] <- -9
  w <- capture_warnings(applyLookup(testM, lu2_3, suffix = "_r"))
  expect_equal(w[1], "Not all values have a recode value assigned (missings in value_new).")
})

test_that("Warnings for incomplete lookup/lookup with additional values",{
  lu3_1 <- lu3
  lu3_1$value_new <- c(-9, -6, 1, NA, 2)
  mess <- capture_warnings(applyLookup(testM, lu3_1[1:4, ]))
  expect_equal(mess[[2]], "For variable VAR1 the following values are in the data but not in the lookup table: 2")
})

test_that("Behaviour if new variable containts only missings",{
  lu3_1 <- lu3
  lu3_1$value_new <- c(-9, -6, 1, NA, 2)
  mess <- capture_warnings(applyLookup(testM, lu3_1))
  expect_equal(mess[[2]], "In the new variable VAR2 all values are missing, therefore the variable is dropped. If this behaviour is not desired, contact the package author.")

  suppressWarnings(out <- applyLookup(testM, lu3_1))
  expect_equal(namesGADS(out), c("VAR1", "VAR3"))

  suppressWarnings(out <- applyLookup(testM, lu3_1, suffix = c("_r")))
  expect_equal(namesGADS(out), c("VAR1", "VAR2", "VAR3", "VAR1_r"))
})


test_that("Tests for formatting of lookup",{
  lu_false <- lu1[, 2:4]
  expect_error(ng <- applyLookup(testM, lu_false))
})

test_that("Applying recode for 1 variable",{
  lu2$value_new <- c(-9, -6, 10, 11)
  ng <- applyLookup(testM, lu2, suffix = "_r")
  expect_equal(ng$dat$VAR1_r, c(10, -9, -6, 11))
})

test_that("Applying recode for 1 variable with one NA old value",{
  lu2$value_new <- c(-94, -6, 10, 11)
  lu2$value[1] <- NA
  testM$dat$VAR1[2] <- NA
  ng <- applyLookup(testM, lu2, suffix = "_r")
  expect_equal(ng$dat$VAR1_r, c(10, -94, -6, 11))

  ng2 <- applyLookup(testM, lu2, suffix = "")
  expect_equal(ng2$dat$VAR1, c(10, -94, -6, 11))
})

test_that("Applying recode for 1 variable with one empty string old value",{
  string_df <- data.frame(VAR1 = c("a", "b", ""), stringsAsFactors = FALSE)
  string_gads <- import_DF(string_df)
  lu_string <- createLookup(string_gads, recodeVars = "VAR1")

  lu_string$value_new <- c("q", "h", "y")

  ng <- applyLookup(string_gads, lu_string, suffix = "_r")
  expect_equal(ng$dat$VAR1_r, c("q", "h", "y"))

  ng2 <- applyLookup(string_gads, lu_string, suffix = "")
  expect_equal(ng2$dat$VAR1, c("q", "h", "y"))
})

test_that("Applying partial recode for 1 variable",{
  lu2$value_new <- c(-9, -6, 10, 11)
  lu2_part <- lu2[3:4, ]
  suppressWarnings(ng <- applyLookup(testM, lu2_part, suffix = "_r"))
  expect_equal(ng$dat$VAR1_r, c(10, -99, -96, 11))
  expect_equal(ng$dat$VAR1, c(1, -99, -96, 2))
})

test_that("Applying recode for 1 variable while overwriting",{
  lu2$value_new <- c(-9, -6, 10, 11)
  ng <- applyLookup(testM, lu2)
  expect_equal(ng$dat$VAR1, c(10, -9, -6, 11))
  expect_equal(dim(ng$dat), c(4, 3))
})

test_that("Applying recode for character to numeric/character variables",{
  df <- data.frame(id = 1:3, v1 = c("1", "5", "3"), stringsAsFactors = FALSE)
  gads <- import_DF(df)
  lu_nc2 <- lu_nc1 <- createLookup(gads, "v1")

  # originally problems, if variable could be converted to numeric before recoding
  lu_nc1$value_new <- c("one", "five", "three")
  ng1 <- applyLookup(gads, lu_nc1)

  lu_nc2$value_new <- c(1, 5, 3)
  ng2 <- applyLookup(gads, lu_nc2)
  expect_equal(ng2$dat$v1, c("1", "5", "3"))
})

test_that("Applying recode for with a tibbel lookup table",{
  lu2$value_new <- c(-9, -6, 10, 11)
  lu2 <- tibble::as_tibble(lu2)
  ng <- applyLookup(testM, lu2)
  expect_equal(ng$dat$VAR1, c(10, -9, -6, 11))
  expect_equal(dim(ng$dat), c(4, 3))
})

test_that("Applying recode for more variables",{
  lu3$value_new <- c(-9, -6, 10, -10, 11)
  ng <- applyLookup(testM, lu3, suffix = "_r")
  expect_equal(ng$dat$VAR1_r, c(10, -9, -6, 11))
  expect_equal(ng$dat$VAR2_r, rep(-10, 4))
})

test_that("Applying recode for more variables and lookup as tibble",{
  lu3$value_new <- c(-9, -6, 10, -10, 11)
  lu_tbl <- tibble::as_tibble(lu3)
  ng <- applyLookup(testM, lu_tbl, suffix = "_r")
  expect_equal(ng$dat$VAR1_r, c(10, -9, -6, 11))
  expect_equal(ng$dat$VAR2_r, rep(-10, 4))
})

test_that("Workflow multiple columns, collapse, apply",{
  lu1$r1 <- c(1, 2, NA, 4, NA)
  lu1$r2 <- c(NA, -2, 3, 4, 5)

  lu_r <- collapseColumns(lu1, recodeVars = c("r1", "r2"), prioritize = "r2")
  testM2 <- applyLookup(testM, lu_r, suffix = "_r")
  expect_equal(testM2$dat$VAR2_r, rep(5, 4))
  expect_equal(testM2$dat$VAR1_r, c(1, -2, 3, 4))
})


test_that("Specific warning for empty strings (necessary due to readxl)",{
  df <- data.frame(v1 = c(1, 1, 2), v2 = c("lala", "", ""), stringsAsFactors = FALSE)
  gads <-import_DF(df)
  l <- createLookup(gads, recodeVars = "v2")
  l[2, 2] <- NA
  l$value_new <- c("Germany", "missing")
  warns <- capture_warnings(applyLookup(gads, l, suffix = "_r"))

  expect_equal(warns[3], "Empty strings are values in the data but not in the look up table. Using recodeString2NA() is recommended.")
})
