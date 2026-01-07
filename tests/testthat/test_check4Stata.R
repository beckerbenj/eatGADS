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

test_that("Output has correct format", {
  bad_gads <- good_gads
  bad_gads$labels$value[bad_gads$labels$value == 99] <- 99.9
  result_bad <- check4Stata(bad_gads)

  expect_type(result_bad, "list")
  expect_length(result_bad, 11)
  expect_named(result_bad, c("verdict",
                             "dots_in_varNames",
                             "special_chars_in_varNames",
                             "varName_length",
                             "labeled_fractionals",
                             "large_integers",
                             "varLabel_length",
                             "valLabel_length",
                             "long_strings",
                             "too_many_rows",
                             "too_many_cols"))
})

test_that("Correctly identify 'good' datasets", {
  expect_silent(result_good <- check4Stata(good_gads))
  expect_null(result_good)
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

  expect_equal(result_longname$verdict, "hard issue")
  expect_equal(result_labfract$verdict, "hard issue")
  expect_equal(result_intover$verdict, "hard issue")
  expect_equal(result_longlab$verdict, "soft issue")
})

test_that("Correctly identify variable names with dots and special characters", {
  bad_gads3 <- bad_gads2 <- bad_gads1 <- good_gads

  names(bad_gads1[[1]])[[1]] <- bad_gads1$labels$varName[[1]] <- "id.stud"
  expect_silent(result_dots <- check4Stata(bad_gads1))
  expect_equal(result_dots$verdict, "hard issue")
  expect_equal(result_dots$dots_in_varNames, "id.stud")

  names(bad_gads2[[1]])[[1]] <- bad_gads2$labels$varName[[1]] <- "idstüd"
  expect_silent(result_specchar <- check4Stata(bad_gads2))
  expect_equal(result_specchar$verdict, "hard issue")
  expect_equal(result_specchar$special_chars_in_varNames, "idstüd")

  names(bad_gads3[[1]])[[1]] <- bad_gads3$labels$varName[[1]] <- "id.stüd"
  expect_silent(result_dotandspec <- check4Stata(bad_gads3))
  expect_equal(result_dotandspec$verdict, "hard issue")
  expect_equal(result_dotandspec$dots_in_varNames, "id.stüd")
  expect_equal(result_dotandspec$special_chars_in_varNames, "id.stüd")
})

test_that("Correctly identify labeled strings (placeholder for when that check is implemented)", {
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
  expect_null(result_longstring)

  bad_gads <- okay_gads
  bad_gads$dat$somevar[[2]] <- paste0(longstring, "a")
  expect_silent(result_2longstring <- check4Stata(bad_gads))
  expect_equal(result_2longstring$verdict, "soft issue")
  expect_equal(result_2longstring$long_strings,
               data.frame(varName = "somevar",
                          string = paste0(paste0(rep("a", 37), collapse = ""), "...")))
})

test_that("Correctly identify huge datasets", {
  too_many_cols <- getProgramLimit("Stata 19/BE", "ncols")$value + 1
  widedf <- as.data.frame(t(vector(mode = "logical", length = too_many_cols)))
  widegads <- import_DF(widedf)
  expect_silent(result_manycols <- check4Stata(widegads, version = "Stata 19/BE"))
  expect_equal(result_manycols$verdict, "hard issue")
  expect_equal(result_manycols$too_many_cols, 1)

  # Limits for the number of rows is not really testable because they blow up the memory.
  # too_many_rows <- getProgramLimit("Stata", "nrows")$value + 1
  # longdf <- as.data.frame(vector(mode = "logical", length = too_many_rows))
  # longgads <- import_DF(data.frame(x = 1))
  # longgads$dat <- longdf
  # expect_silent(result_manyrows <- check4Stata(longgads, version = "Stata 19/BE"))
  # expect_equal(result_manyrows$verdict, "hard issue")
  # expect_equal(result_manyrows$too_many_rows, 1)
})
