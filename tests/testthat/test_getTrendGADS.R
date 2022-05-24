
# load test data
# load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
allList <- mergeLabels(df1 = df1, df2 = df2)

# dfSAV <- import_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

control_caching <- FALSE

fp1 <- system.file("extdata", "trend_gads_2020.db", package = "eatGADS")
fp2 <- system.file("extdata", "trend_gads_2015.db", package = "eatGADS")
fp3 <- system.file("extdata", "trend_gads_2010.db", package = "eatGADS")
fp2b <- system.file("extdata", "trend_gads_2015_pkList.db", package = "eatGADS")
lep <- system.file("extdata", "gads_LEs.db", package = "eatGADS")

### check
test_that("check_keyStrcuture_TrendGADS", {
  expect_silent(check_keyStrcuture_TrendsGADS(filePaths = c(fp1, fp2, fp3)))
  # checkTrendGADS(filePath1 = "tests/testthat/helper_dataBase3.db", filePath2 = "tests/testthat/helper_dataBase2.db")
  expect_error(check_keyStrcuture_TrendsGADS(filePaths = c(fp1, fp2b, fp3)), "Trend data bases must have the same primary key structure.")
})

### check
test_that("check_keyStrcuture_TrendGADS", {
  expect_silent(check_keyStrcuture_TrendGADS(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase2.db"))
  # checkTrendGADS(filePath1 = "tests/testthat/helper_dataBase3.db", filePath2 = "tests/testthat/helper_dataBase2.db")
  expect_error(check_keyStrcuture_TrendGADS(filePath1 = "helper_dataBase3.db", filePath2 = "helper_dataBase2.db"), "Trend data bases must have the same primary key structure.")
})

test_that("check_vSelect", {
  # check_vSelect(namesGADS("tests/testthat/helper_dataBase.db"), vSelect = c("ID1", "test2"))
  out <- check_vSelect(namesGADS("helper_dataBase.db"), vSelect = c("ID1", "test2"))
  expect_equal(out, list(in_gads = "ID1", not_in_gads = "test2"))
})

test_that("errors_vSelect", {
  in1 <- list(list(in_gads = c("ID", "v1"), not_in_gads = c("v2", "v3")),
              list(in_gads = c("ID", "v1"), not_in_gads = c("v2", "v3")))
  expect_error(errors_vSelect(in1, years = c(2011, 2016)),
               "The following selected variables are not in any of the data bases: v2, v3")

  in2 <- list(list(in_gads = c("ID", "v1"), not_in_gads = c("v2", "v3")),
              list(in_gads = c("ID", "v1"), not_in_gads = c()))
  expect_warning(errors_vSelect(in2, years = c(2011, 2016)),
               "The following variables are not in GADS 2011: v2, v3. NAs will be inserted if data is extracted.")

  in3 <- list(list(in_gads = c(), not_in_gads = c("ID", "v1")),
              list(in_gads = c("ID", "v1"), not_in_gads = c()))
  expect_error(errors_vSelect(in3, years = c(2011, 2016)),
               "No variables from data base 2011 selected.")
})


### trend gads without LEs
test_that("Extract trend GADS errors", {
  # out <- getTrendGADS(filePath1 = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase.db", filePath2 = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase2.db", years = c(2012, 2018))
  expect_error(out <- getTrendGADS(filePaths = c(fp1, fp1), years = c(2012, 2018), fast = control_caching),
               "All file arguments have to point to different files.")
  expect_error(out <- getTrendGADS(filePaths = c(fp1, fp2, fp3), years = c(2012, 2018), fast = control_caching),
               "'years' has to be a numeric vector of the same length as 'filePaths'.")
  expect_error(out <- getTrendGADS(filePaths = c(fp1, fp2, fp3), years = c(2012, 2018, "b"), fast = control_caching),
               "'years' has to be a numeric vector of the same length as 'filePaths'.")
  expect_error(getTrendGADS(filePaths = c(fp1, fp2, fp3), years = c(2020, 2015, 2010), fast = 1),
               "'fast' has to be a logical vector of length 1.")
  expect_error(getTrendGADS(filePaths = c(fp1, fp2, fp3), years = c(2020, 2015, 2010), verbose = c(TRUE, TRUE)),
               "'verbose' has to be a logical vector of length 1.")

})


test_that("Extract trend GADS", {
  s <- capture_output(out <- getTrendGADS(filePaths = c(fp1, fp2, fp3), years = c(2020, 2015, 2010), fast = control_caching))
  expect_equal(s[1], " -----  Loading GADS 2020 ----- \n -----  Loading GADS 2015 ----- \n -----  Loading GADS 2010 ----- ")
  expect_equal(unique(out$datList$gads2020$year), 2020)
  expect_equal(unique(out$datList$gads2015$year), 2015)
  expect_equal(unique(out$datList$gads2010$year), 2010)
  expect_equal(dim(out$datList$gads2020), c(60, 10))
  expect_equal(dim(out$datList$gads2010), c(60, 10))
  expect_equal(dim(out$allLabels), c(33, 9))
  expect_equal(out$allLabels$data_table, c(rep("gads2020", 11), rep("gads2015", 11), rep("gads2010", 11)))
  expect_equal(class(out), c("trend_GADSdat", "all_GADSdat", "list"))

  s <- capture_output(out <- getTrendGADS(filePaths = c(fp1, fp2, fp3), years = c(2020, 2015, 2010), fast = control_caching, verbose = FALSE))
  expect_equal(s[1], "")
})

test_that("Correct vSelect errors for getTrendGADS", {
  expect_error(getTrendGADS(filePaths = c(fp1, fp2, fp3), years = c(2020, 2015, 2010), vSelect = c("idstud", "gender", "test"),
                             fast = control_caching),
               "The following selected variables are not in any of the data bases: test")
  expect_error(getTrendGADS(filePaths = c(fp1, fp2, fp3), years = c(2020, 2015, 2010), vSelect = c("idstud", "gender", "test", "test2"),
                             fast = control_caching),
               "The following selected variables are not in any of the data bases: test, test2")
  expect_error(getTrendGADS(filePaths = c("helper_dataBase.db", "helper_dataBase_uniqueVar.db"), years = c(2012, 2018), vSelect = c("V3"),
                            fast = control_caching),
               "No variables from data base 2012 selected.")
})


