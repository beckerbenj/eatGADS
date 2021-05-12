
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

test_that("Compare two different GADSdat objects",{
  out <- equalGADS(df1, df2)
  expect_equal(out$names_not_in_1, "V2")
  expect_equal(out$names_not_in_2, "V1")
  expect_equal(out$data_differences, "ID1")
  expect_equal(out$meta_data_differences, character())
  expect_equal(out$data_nrow, "all.equal")

  df1_2 <- df1
  df1_2$dat[1, ] <- c(0, 0)
  out2 <- equalGADS(df1, df1_2)
  expect_equal(out2$data_differences, c("ID1", "V1"))
})

test_that("Compare two identical GADSdat objects",{
  out <- equalGADS(df1, df1)
  expect_equal(out$names_not_in_1, character())
  expect_equal(out$names_not_in_2, character())
  expect_equal(out$data_differences, character())
  expect_equal(out$meta_data_differences, character())
  expect_equal(out$data_nrow, "all.equal")
})

test_that("Compare two different GADSdat objects, large ID numbers",{
  df1_2 <- df1
  df1_2$dat$ID1 <- c(5140010110, 5140010111)
  df1$dat$ID1 <- c(5140010109, 5140010110)
  out <- equalGADS(df1, df1_2)
  expect_equal(out$data_differences, c("ID1"))
})
