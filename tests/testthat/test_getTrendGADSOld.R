
# load test data
# load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
allList <- mergeLabels(df1 = df1, df2 = df2)

# dfSAV <- import_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

control_caching <- FALSE


### trend gads without LEs
test_that("Extract trend GADS", {
  # out <- getTrendGADSOld(filePath1 = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase.db", filePath2 = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase2.db", years = c(2012, 2018))
  expect_error(out <- getTrendGADSOld(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase.db", years = c(2012, 2018), fast = control_caching), "All file arguments have to point to different files.")
  expect_error(out <- getTrendGADSOld(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase2.db", years = 2012, fast = control_caching), "years has to be a numeric vector of length 2.")
  expect_error(out <- getTrendGADSOld(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase2.db", years = c(2012, "b"), fast = control_caching), "years has to be a numeric vector of length 2.")
  out <- getTrendGADSOld(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase2.db", years = c(2012, 2018), fast = control_caching)
  expect_equal(out$datList$gads2012$year, c(rep(2012, 3)))
  expect_equal(out$datList$gads2018$year, c(rep(2018, 3)))
  expect_equal(dim(out$datList$gads2012), c(3, 4))
  expect_equal(dim(out$datList$gads2018), c(3, 4))
  expect_equal(dim(out$allLabels), c(8, 9))
  expect_equal(out$allLabels$data_table, c(rep("gads2012", 4), rep("gads2018", 4)))
  expect_equal(class(out), c("trend_GADSdat", "all_GADSdat", "list"))
})

test_that("Correct vSelect errors for getTrendGADSOld", {
  expect_error(getTrendGADSOld(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase2.db", years = c(2012, 2018), vSelect = c("ID1", "V2", "test"), fast = control_caching),
               "Variables test are in neither of both data bases.")
  expect_error(getTrendGADSOld(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase_uniqueVar.db", years = c(2012, 2018), vSelect = c("V3"), fast = control_caching),
               "No variables from first data base selected.")
})

test_that("Extract trend GADS with unique variables in one GADS", {
  # out <- getTrendGADSOld(filePath1 = "tests/testthat/helper_dataBase.db", filePath2 = "tests/testthat/helper_dataBase_uniqueVar.db", years = c(2012, 2018), fast = FALSE)
  expect_warning(getTrendGADSOld(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase_uniqueVar.db", years = c(2012, 2018),
                              fast = control_caching),
                 "The following variables are not in GADS 2012: V3. NAs will be inserted if data is extracted.")
  out <- suppressWarnings(getTrendGADSOld(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase_uniqueVar.db", years = c(2012, 2018),
                                       fast = control_caching))
  expect_equal(ncol(out$datList$gads2018), 5)
  expect_equal(nrow(out$allLabels), 9)
  expect_equal(out$datList$gads2018$V3, c(8, 8, 9))

  expect_warning(getTrendGADSOld(filePath1 = "helper_dataBase_uniqueVar.db", filePath2 = "helper_dataBase.db", years = c(2012, 2018),
                              fast = control_caching),
                 "The following variables are not in GADS 2018: V3. NAs will be inserted if data is extracted.")
  out2 <- suppressWarnings(getTrendGADSOld(filePath1 = "helper_dataBase_uniqueVar.db", filePath2 = "helper_dataBase.db", years = c(2012, 2018),
                                        fast = control_caching))
  expect_equal(ncol(out2$datList$gads2012), 5)
  expect_equal(nrow(out2$allLabels), 9)
  expect_equal(out2$datList$gads2012$V3, c(8, 8, 9))

  expect_warning(getTrendGADSOld(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase_uniqueVar.db", years = c(2012, 2018),
                              fast = control_caching, vSelect = c("ID1", "V1", "V2", "V3")),
  "The following variables are not in GADS 2012: V3. NAs will be inserted if data is extracted.")
  out3 <- suppressWarnings(getTrendGADSOld(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase_uniqueVar.db", years = c(2012, 2018),
                                        fast = control_caching, vSelect = c("ID1", "V1", "V2", "V3")))
  expect_equal(out, out3)
})


# with linking errors
# ---------------------------------------------------------------------------
# getGADS(filePath = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_le.db")

test_that("make_leSelect", {
  expect_equal(make_leSelect(lePath = "helper_le.db", vSelect = NULL), NULL)
  expect_equal(make_leSelect(lePath = "helper_le.db", vSelect = c("PV")), "LE_PV")
  expect_equal(make_leSelect(lePath = "helper_le.db", vSelect = c("PV", "level")), c("LE_PV", "LE_level"))
  expect_equal(make_leSelect(lePath = "helper_le.db", vSelect = c("lala")), character(0))
})

### trend gads with LEs
test_that("Extract trend GADS with linking errors", {
  # out <- getTrendGADSOld(filePath1 = "tests/testthat/helper_comp.db", filePath2 = "tests/testthat/helper_comp2.db", years = c(2012, 2018), lePath = "tests/testthat/helper_le.db", vSelect = c("ID", "PV"))

  ## no linking errors
  expect_message(getTrendGADSOld(filePath1 = "helper_comp.db", filePath2 = "helper_comp2.db", years = c(2012, 2018), lePath = "helper_le.db", fast = control_caching, vSelect = "ID"),
                 "No linking errors for chosen variables available.")
  out <- suppressMessages(getTrendGADSOld(filePath1 = "helper_comp.db", filePath2 = "helper_comp2.db", years = c(2012, 2018), lePath = "helper_le.db", fast = control_caching, vSelect = "ID"))
  expect_equal(names(out$datList), c("gads2012", "gads2018", "LEs"))
  expect_equal(out$datList$LEs, NULL)
  expect_equal("LEs" %in% out$allLabels$data_table, FALSE)

  ## linking errors
  out2 <- getTrendGADSOld(filePath1 = "helper_comp.db", filePath2 = "helper_comp2.db", years = c(2012, 2018), lePath = "helper_le.db", fast = control_caching, vSelect = c("ID", "PV"))
  expect_equal(dim(out2$datList$LEs), c(2, 2))
  expect_equal(names(out2$datList$LEs), c("dim", "LE_PV"))
  expect_equal("LEs" %in% out2$allLabels$data_table, TRUE)
})






