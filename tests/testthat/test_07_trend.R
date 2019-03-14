context("Trend data bases")

# load test data
load(file = "helper_data.rda")
allList <- mergeLabels(df1 = df1, df2 = df2)


### check
test_that("Check trend GADS", {
  expect_silent(checkTrendGADS(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase2.db"))
  # checkTrendGADS(filePath1 = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase3.db", filePath2 = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase2.db")
  expect_error(checkTrendGADS(filePath1 = "helper_dataBase3.db", filePath2 = "helper_dataBase2.db"), "Trend data bases must have the same primary key structure.")
})



### trend gads without LEs
test_that("Extract trend GADS", {
  # out <- getTrendGADS(filePath1 = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase.db", filePath2 = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase2.db", years = c(2012, 2018))
  expect_error(out <- getTrendGADS(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase.db", years = c(2012, 2018)), "All file arguments have to point to different files.")
  expect_error(out <- getTrendGADS(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase2.db", years = 2012), "years has to be a numeric vector of length 2.")
  out <- getTrendGADS(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase2.db", years = c(2012, 2018))
  expect_equal(out$dat$year, c(rep(2012, 3), rep(2018, 3)))
  expect_equal(dim(out$dat), c(6, 4))
  expect_equal(dim(out$labels), c(8, 9))
  expect_equal(out$labels$data_table, c(rep(2012, 4), rep(2018, 4)))
})

### merging linking errors
test_that("Mering linking errors", {
  gads_trend <- getTrendGADS(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase2.db", years = c(2012, 2018))
  les <- import_DF(data.frame(ID1 = 1:2, le = c(1.1, 0.9), comp = 1:2))
  les2 <- import_DF(data.frame(ID1 = c(1, 2, 1), le = c(1.1, 0.9, 1.3), comp = 1:3))
  les3 <- import_DF(data.frame(ID1 = c(1, 2, 1), le = c(1.1, 0.9, 1.3), V2 = c(4, NA, 8)))

  expect_error(merge_LEs(gads_trend = gads_trend, les = les2, le_keys = c("ID1", "comp")))

  out_single <- merge_LEs(gads_trend = gads_trend, les = les, le_keys = "ID1")
  expect_equal(out_single$dat$le, c(rep(1.1, 4), rep(0.9, 2)))
  expect_equal(out_single$labels$data_table[9:10],  rep("LEs", 2))

  # expect_error(merge_LEs(gads_trend = gads_trend, les = les3, le_keys = c("ID1"))) ### desired, but difficult to realize

  out_double <- merge_LEs(gads_trend = gads_trend, les = les3, le_keys = c("ID1", "V2"))
  expect_equal(out_single$dat$le, c(rep(1.1, 4), rep(0.9, 2)))
  expect_equal(out_single$labels$data_table[9:10],  rep("LEs", 2))
})


### trend gads with LEs
test_that("Extract trend GADS", {
  # out <- getTrendGADS(filePath1 = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase.db", filePath2 = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase2.db", years = c(2012, 2018), lePath = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_le_dataBase.db")
  out <- getTrendGADS(filePath1 = "helper_dataBase.db", filePath2 = "helper_dataBase2.db", years = c(2012, 2018), lePath = "helper_le_dataBase.db")
  expect_equal(out$dat$year, c(2012, 2012, 2018, 2018, 2012, 2018))
  expect_equal(dim(out$dat), c(6, 5))
  expect_equal(dim(out$labels), c(10, 9))
  expect_equal(out$labels$data_table, c(rep(2012, 4), rep(2018, 4), rep("LEs", 2)))
  expect_equal(out$dat$le, c(rep(0.9, 4), rep(1.1, 2)))
})
