
# load(file = "tests/testthat/helper_data.rda")
# testM <- import_spss("tests/testthat/helper_spss_missings.sav")
testM <- import_spss("helper_spss_missings.sav")
load(file = "helper_data.rda")

control_caching <- FALSE


test_that("Warnings and errors",  {
  expect_error(extractDataOld(testM),
               "extractDataOld() is only implemented for backwards compatability of 'trend_GADSdat' objects. Please use extractData() for 'GADSdat' objects.", fixed = TRUE)

  fp1 <- system.file("extdata", "trend_gads_2020.db", package = "eatGADS")
  fp2 <- system.file("extdata", "trend_gads_2015.db", package = "eatGADS")
  fp3 <- system.file("extdata", "trend_gads_2010.db", package = "eatGADS")
  s <- capture_output(gads_3mp <- getTrendGADS(filePaths = c(fp1, fp2, fp3), years = c(2020, 2015, 2010), fast = FALSE))
  expect_error(extractDataOld(gads_3mp),
               "extractDataOld() is only implemented for backwards compatability of 'trend_GADSdat' with data from two data bases. For 'trend_GADSdat' objects with data from more than two data bases use extractData() instead.", fixed = TRUE)
})


test_that("Extract data trend GADS", {
  # trend_gads <- getTrendGADS(filePath1 = "tests/testthat/helper_dataBase.db", filePath2 = "tests/testthat/helper_dataBase_uniqueVar.db", years = c(2012, 2018), fast = FALSE)
  trend_gads <- suppressWarnings(getTrendGADSOld(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase_uniqueVar.db", years = c(2012, 2018), fast = FALSE))
  out <- extractDataOld(trend_gads)
  expect_equal(dim(out), c(6, 5))
  expect_equal(names(out), c("ID1", "V1", "V2", "V3", "year"))

  expect_equal(out$year, c(rep(2012, 3), c(rep(2018, 3))))

  ## convertVariables if some variables are not in both GADS
  out2 <- extractDataOld(trend_gads, convertVariables = "V3")
  expect_equal(out, out2)
})

### with linking errors
test_that("With linking errors", {
  # out <- getTrendGADS(filePath1 = "tests/testthat/helper_comp.db", filePath2 = "tests/testthat/helper_comp2.db", years = c(2012, 2018), lePath = "tests/testthat/helper_le.db", fast = FALSE, vSelect = c("ID", "level", "PV"))
  out <- getTrendGADSOld(filePath1 = "helper_comp.db", filePath2 = "helper_comp2.db", years = c(2012, 2018), lePath = "helper_le.db", fast = control_caching, vSelect = c("ID", "PV"))
  dat <- extractDataOld(out)
  expect_equal(dim(dat), c(8, 5))
  expect_equal(dat$LE_PV, c(rep(0.3, 4), rep(0.2, 4)))

  ## more variables
  out2 <- getTrendGADSOld(filePath1 = "helper_comp.db", filePath2 = "helper_comp2.db", years = c(2012, 2018), lePath = "helper_le.db", fast = control_caching, vSelect = c("ID", "level", "PV"))
  dat2 <- extractDataOld(out2)
  expect_equal(dim(dat2), c(8, 7))
  expect_equal(dat2[dat2$level == 4 & dat2$dim == "A", "LE_level"], c(0.2, 0.2))
  expect_equal(dat2[dat2$level == 5 & dat2$dim == "B", "LE_level"], c(0.9, 0.9))
  expect_equal(dat2[dat2$level == "1a" & dat2$dim == "A", "LE_level"], c(0.01, 0.01))
  expect_equal(dat2[dat2$level == "1b" & dat2$dim == "B", "LE_level"], c(0.4, 0.4))


  ## vSelect is null
  out3 <- getTrendGADSOld(filePath1 = "helper_comp.db", filePath2 = "helper_comp2.db", years = c(2012, 2018), lePath = "helper_le.db", fast = control_caching)
  dat <- extractDataOld(out3)
  expect_equal(dim(dat2), c(8, 7))
  expect_equal(names(dat2), c("ID", "dim", "PV", "level", "LE_PV", "LE_level", "year"))


})


## Archiv
les <- import_DF(data.frame(ID1 = 1:2, le = c(1.1, 0.9), comp = 1:2))
les2 <- import_DF(data.frame(ID1 = c(1, 2, 1), le = c(1.1, 0.9, 1.3), comp = 1:3))
les3 <- import_DF(data.frame(ID1 = c(1, 2, 1), le = c(1.1, 0.9, 1.3), V2 = c(4, NA, 8)))

#expect_error(merge_LEs(gads_trend = gads_trend, les = les2, le_keys = c("ID1", "comp")))

#out_single <- merge_LEs(gads_trend = gads_trend, les = les, le_keys = "ID1")
#expect_equal(out_single$dat$le, c(rep(1.1, 4), rep(0.9, 2)))
#expect_equal(out_single$labels$data_table[9:10],  rep("LEs", 2))

# expect_error(merge_LEs(gads_trend = gads_trend, les = les3, le_keys = c("ID1"))) ### desired, but difficult to realize

#out_double <- merge_LEs(gads_trend = gads_trend, les = les3, le_keys = c("ID1", "V2"))
#expect_equal(out_single$dat$le, c(rep(1.1, 4), rep(0.9, 2)))
#expect_equal(out_single$labels$data_table[9:10],  rep("LEs", 2))







