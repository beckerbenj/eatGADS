testdf <- data.frame(idstud = 1:5,
                     somevar = letters[1:5],
                     selfeff = c(1, 3, 1, 99, 99))
good_gads <- import_DF(testdf)
good_gads <- changeVarLabels(good_gads, "somevar", "Teacher notes")
good_gads <- changeVarLabels(good_gads, "selfeff", "Self Efficacy")
good_gads <- changeMissings(good_gads, "selfeff", 99, "miss")
good_gads <- changeValLabels(good_gads, "selfeff",
                             c(1, 3, 99),
                             c("amazing", "mediocre", "missing by intention"))

test_that("Output has correct format, regardless of result", {
  result_good <- check4Stata(good_gads)

  bad_gads <- good_gads
  bad_gads$labels$value[bad_gads$labels$value == 99] <- 99.9
  result_bad <- check4Stata(bad_gads)

  expect_type(result_good, "list")
  expect_type(result_bad, "list")
  expect_length(result_good, 12)
  expect_length(result_bad, 12)
  listnames <- c("verdict",
                 "r1_dots", "r1_specchars",
                 "r2_longNames",
                 "r3_labeledFractionals",
                 "r4_largeIntegers",
                 "r5_varLabels", "r5_valLabels",
                 "r6_labeledStrings",
                 "r7_longStrings",
                 "r8_surplusRows", "r8_surplusCols")
  expect_named(result_good, listnames)
  expect_named(result_bad, listnames)
})

test_that("Correctly identify 'good' datasets", {
  expect_silent(result_good <- check4Stata(good_gads))
  expect_true(result_good$verdict)
})

test_that("Correctly identify 'bad' datasets using other checks", {
  # long variable names
  bad_gads <- good_gads
  names(bad_gads[[1]])[[2]] <- bad_gads$labels$varName[[2]] <-
    paste0(rep("a", getProgramLimit("Stata", "varNames")$value + 1), collapse = "")
  expect_silent(result_longname <- check4Stata(bad_gads))

  # labeled fractionals
  bad_gads <- good_gads
  bad_gads$labels$value[bad_gads$labels$value == 99] <- 99.9
  expect_silent(result_labfract <- check4Stata(bad_gads))

  # integer overflow
  bad_gads <- good_gads
  bad_gads$labels$value[bad_gads$labels$value == 99] <- 9999999999
  expect_silent(result_intover <- check4Stata(bad_gads))

  # long labels
  bad_gads <- good_gads
  bad_gads$labels$valLabel[[3]] <- paste0(rep("a", getProgramLimit("Stata", "valLabels")$value + 1),
                                          collapse = "")
  bad_gads$labels$varLabel[[1]] <- paste0(rep("a", getProgramLimit("Stata", "varLabels")$value + 1),
                                          collapse = "")
  expect_silent(result_longlab <- check4Stata(bad_gads))

  expect_false(result_longname$verdict)
  expect_false(result_labfract$verdict)
  expect_false(result_intover$verdict)
  expect_false(result_longlab$verdict)
})

test_that("Correctly identify variable names with dots and special characters", {
  bad_gads3 <- bad_gads2 <- bad_gads1 <- good_gads

  names(bad_gads1[[1]])[[1]] <- bad_gads1$labels$varName[[1]] <- "id.stud"
  expect_silent(result_dots <- check4Stata(bad_gads1))
  expect_false(result_dots$verdict)
  expect_equal(result_dots$r1_dots, "id.stud")

  names(bad_gads2[[1]])[[1]] <- bad_gads2$labels$varName[[1]] <- "idstüd"
  expect_silent(result_specchar <- check4Stata(bad_gads2))
  expect_false(result_specchar$verdict)
  expect_equal(result_specchar$r1_specchars, "idstüd")

  names(bad_gads3[[1]])[[1]] <- bad_gads3$labels$varName[[1]] <- "id.stüd"
  expect_silent(result_dotandspec <- check4Stata(bad_gads3))
  expect_false(result_specchar$verdict)
  expect_equal(result_dotandspec$r1_dots, "id.stüd")
  expect_equal(result_dotandspec$r1_specchars, "id.stüd")
})

test_that("Correctly identify labeled strings (not really needed rn)", {
  bad_gads <- good_gads
  bad_gads$labels$value[[3]] <- "c"
  bad_gads$labels$valLabel[[3]] <- "missing by design"
  bad_gads$labels$missings[[3]] <- "miss"
  expect_error(result_labstring <- check4Stata(bad_gads))
})

test_that("Correctly identify long strings", {
  okay_gads <- good_gads
  longstring <- paste0(rep("a", getProgramLimit("Stata", "stringvars")$value),
                       collapse = "")
  okay_gads$dat$somevar[[2]] <- longstring
  expect_silent(result_longstring <- check4Stata(okay_gads))
  expect_true(result_longstring$verdict)

  bad_gads <- okay_gads
  bad_gads$dat$somevar[[2]] <- paste0(longstring, "a")
  expect_silent(result_2longstring <- check4Stata(bad_gads))
  expect_false(result_2longstring$verdict)
  expect_equal(result_2longstring$r7_longStrings,
               data.frame(varName = "somevar",
                          string = paste0(paste0(rep("a", 37), collapse = ""), "...")))
})

test_that("Correctly identify huge datasets", {
  too_many_cols <- getProgramLimit("Stata 19/BE", "ncols")$value + 1
  widedf <- as.data.frame(matrix(1:too_many_cols, ncol = too_many_cols))
  widegads <- import_DF(widedf)
  expect_silent(result_manycols <- check4Stata(widegads, version = "Stata 19/BE"))
  expect_false(result_manycols$verdict)
  expect_equal(result_manycols$r8_surplusCols, 1)

  # Limits for the number of rows is not really testable because they blow up the memory.
  # too_many_rows <- getProgramLimit("Stata", "nrows")$value + 1
  # longdf <- as.data.frame(matrix(1:too_many_rows, nrow = too_many_rows))
  # longgads <- import_DF(data.frame(x = 1))
  # longgads$dat <- longdf
})
