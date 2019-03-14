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


### LE data base
les <- data.frame(comp = c(1:2, 1:2),
                  domain = c("reading", "reading", "math", "math"),
                  leScore = c(1.2, 1.3, 0.9, 0.8))
les <- import_DF(les)
test_that("Create linking error data base", {
  expect_message(createLEs(LEs = les, IDvars = c("comp", "domain"), filePath = ":memory:"), "filePath points to work memory")
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



### trend gads with LEs
test_that("Extract trend GADS", {
  out <- getTrendGADS(filePath1 = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase.db", filePath2 = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase2.db", years = c(2012, 2018), lePath = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_le_dataBase.db")
  expect_equal(out$dat$year, c(2012, 2012, 2018, 2018, 2012, 2018))
  expect_equal(dim(out$dat), c(6, 5))
  expect_equal(dim(out$labels), c(10, 9))
  expect_equal(out$labels$data_table, c(rep(2012, 4), rep(2018, 4), rep("LEs", 2)))
  expect_equal(out$dat$le, c(rep(0.9, 4), rep(1.1, 2)))
})
