# dates and times
# ------------------------------------------------------------------------
# testT <- haven::read_sav("tests/testthat/helper_spss_times_error.sav", user_na = TRUE)
# testT <- haven::read_sav("tests/testthat/helper_spss_times.sav", user_na = TRUE)
testT <- haven::read_sav("helper_spss_times.sav", user_na = TRUE)

#testT2 <- haven::read_sav("tests/testthat/helper_spss_datetime.sav", user_na = TRUE)
testT2 <- haven::read_sav("helper_spss_datetime.sav", user_na = TRUE)

test_that("Modifiying of variables of class date/time", {
  warns <- capture_warnings(out <- times2character.savDat(testT))
  expect_equal(warns[[1]], "Value labels and missing codes for 'TIMES' variables are not supported by eatGADS. Missing values are converted to NA and labels and missing codes are dropped from meta data for variable VAR1")
  expect_equal(warns[[2]], "Value labels and missing codes for 'TIMES' variables are not supported by eatGADS. Missing values are converted to NA and labels and missing codes are dropped from meta data for variable VAR2")

  expect_equal(attributes(out$VAR1)$na_values, NULL)
  expect_equal(attributes(out$VAR1)$labels, NULL)

  expect_equal(attributes(out$VAR3_1)$format.spss, c("A11"))
})

test_that("Import of variables of class date/time", {
  # out <- import_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_times.sav")
  suppressWarnings(out <- import_spss("helper_spss_times.sav"))
  expect_equal(out$dat$VAR1, c("13:00:00", NA))
  expect_equal(out$dat$VAR2, c("13:00:00", NA))
  expect_equal(out$dat$VAR1_1, c("13:00:00", "-99:00:00"))
  expect_equal(out$dat$VAR3_1, c("1989-08-12", "2009-01-27"))

  expect_equal(out$labels[1, "format"], c("A8"))
})

test_that("Import of variables of class date with labels", {
  # out <- import_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_date_labeled.sav")
  # out <- import_spss("tests/testthat/helper_spss_date_labeled.sav")
  warns <- capture_warnings(out <- import_spss("helper_spss_date_labeled.sav"))
  expect_equal(warns[[1]], "Value labels and missing codes for 'DATE' variables are not supported by eatGADS and current implementation is experimental. Missing values are converted to NA and labels and missing codes are dropped from meta data for variable VAR3_1")
  expect_equal(out$dat$VAR3_1, c(NA, "2009-01-27"))

})

test_that("Errors for import of variables of class date/time", {
  #expect_error(out <- import_spss("helper_spss_times_error.sav"), "Labelled dates are currently not supported by eatGADS.")
  vec <- as.POSIXct(strptime("2011-03-27 01:30:00", "%Y-%m-%d %H:%M:%S"))
  df <- data.frame(vec)
  expect_error(out <- import_DF(df), "POSIXct and POSIXlt are currently not supported by eatGADS.")
})


test_that("Import of variables of class datetime/posx", {
  # out <- import_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_datetime.sav")
  out <- import_spss("helper_spss_datetime.sav")

  expect_equal(out$dat[, 2], c("2012-03-30 13:40:55", "1995-12-05 23:59:59", "1950-03-01 00:01:01"))
  expect_equal(out$labels[2, "format"], c("ATIME20"))
})

#x <-"Q:/FDZ/Alle/01_Studien/StEG/StEG Systemmonitoring/Aufbereitung und Pruefung/Bearbeitung/Daten/Arbeitskopie/StEG 2018/(1) Scientific Use File/fc_suf_test.sav"
#test <-import_spss(x)
