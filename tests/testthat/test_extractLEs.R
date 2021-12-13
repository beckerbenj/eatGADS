
# load(file = "tests/testthat/helper_data.rda")
# testM <- import_spss("tests/testthat/helper_spss_missings.sav")
testM <- import_spss("helper_spss_missings.sav")
load(file = "helper_data.rda")

control_caching <- FALSE


test_that("Warnings and errors",  {
  expect_error(extractLEs(testM),
               "extractLEs() is only meaningful for 'trend_GADSdat' objects.", fixed = TRUE)

  fp1 <- system.file("extdata", "trend_gads_2020.db", package = "eatGADS")
  fp2 <- system.file("extdata", "trend_gads_2015.db", package = "eatGADS")
  fp3 <- system.file("extdata", "trend_gads_2010.db", package = "eatGADS")
  s <- capture_output(gads_3mp <- getTrendsGADS(filePaths = c(fp1, fp2, fp3), years = c(2020, 2015, 2010), fast = FALSE))
  expect_error(extractLEs(gads_3mp),
               "No linking errors found in 'GADSdat'. Make sure to specify 'lePath' in getTrendsGADS().", fixed = TRUE)
})

### with linking errors
test_that("With linking errors", {
  # out <- getTrendGADS(filePath1 = "tests/testthat/helper_comp.db", filePath2 = "tests/testthat/helper_comp2.db", years = c(2012, 2018), lePath = "tests/testthat/helper_le.db", fast = FALSE, vSelect = c("ID", "PV"))
  out <- getTrendGADS(filePath1 = "helper_comp.db", filePath2 = "helper_comp2.db", years = c(2012, 2018), lePath = "helper_le.db", fast = control_caching, vSelect = c("ID", "PV"))
  dat <- extractLEs(out)
  expect_equal(dim(dat), c(2, 2))
  expect_equal(dat$LE_PV, c(0.3, 0.2))

  ## more variables
  # out2 <- getTrendGADS(filePath1 = "tests/testthat/helper_comp.db", filePath2 = "tests/testthat/helper_comp2.db", years = c(2012, 2018), lePath = "tests/testthat/helper_le.db", fast = FALSE, vSelect = c("ID", "level", "PV"))
  out2 <- getTrendGADS(filePath1 = "helper_comp.db", filePath2 = "helper_comp2.db", years = c(2012, 2018), lePath = "helper_le.db", fast = control_caching, vSelect = c("ID", "level", "PV"))
  dat2 <- extractLEs(out2)
  expect_equal(dim(dat2), c(12, 4))
  expect_equal(dat2[dat2$level == 4 & dat2$dim == "A", "LE_level"], 0.2)
  expect_equal(dat2[dat2$level == 5 & dat2$dim == "B", "LE_level"], 0.9)
  expect_equal(dat2[dat2$level == "1a" & dat2$dim == "A", "LE_level"], 0.01)
  expect_equal(dat2[dat2$level == "1b" & dat2$dim == "B", "LE_level"], 0.4)


  ## vSelect is null
  out3 <- getTrendGADS(filePath1 = "helper_comp.db", filePath2 = "helper_comp2.db", years = c(2012, 2018), lePath = "helper_le.db", fast = control_caching)
  dat <- extractDataOld(out3)
  expect_equal(dim(dat2), c(12, 4))
  expect_equal(names(dat2), c("dim", "LE_PV", "level", "LE_level"))


})







