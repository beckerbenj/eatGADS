
###### check_GADSdat
# testM <- import_spss("tests/testthat/helper_spss_missings.sav")
testM <- import_spss("helper_spss_missings.sav")
# load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_data.rda")
load(file = "helper_data.rda")


test_that("Object validater for GADSdat objects",{
  testM6 <- testM5 <- testM4 <- testM3 <- testM2 <- testM
  testM$dat[, "newVar"] <- NA
  expect_error(check_GADSdat(testM), "The following variables are in the data but do not have meta data: newVar")
  testM2$labels[7, "varName"] <- "newVar"
  expect_error(check_GADSdat(testM2), "The following variables have meta data but are not in the actual data: newVar")
  testM3$labels[2, "varLabel"] <- "other label"
  expect_error(check_GADSdat(testM3), "The following variable has inconsistent meta information on variable level: VAR1")
  testM4$dat <- tibble::as_tibble(testM4$dat)
  expect_error(check_GADSdat(testM4), "dat element has to be a data frame and can not be a tibble.")
  testM5$labels[4:5, "value"] <- -99
  expect_error(check_GADSdat(testM5), "The following variable has duplicate values rows in its meta data: VAR2")

  ## but does tolerate NAs (because these are assigned to labeled strings!)
  testM6$labels[4:5, "value"] <- NA
  expect_silent(check_GADSdat(testM6))

  df1_3 <- df1_1 <- df1_2 <- df1
  df1_1$labels[2, c("value")] <- -99
  df1_2$labels[2, c("valLabel")] <- "some"
  expect_error(check_GADSdat(df1_1), "The following variable has value labels but is not marked as labeled: V1")
  expect_error(check_GADSdat(df1_2), "The following variable has value labels but is not marked as labeled: V1")

  df1_3$labels[2, c("value")] <- "-99"
  expect_error(check_GADSdat(df1_3), "Column 'value' in the meta data is not numeric.")
})


