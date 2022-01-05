# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")


test_that("Cbind two GADSdat objects",{
  df3 <- df1
  df3 <- changeVarNames(df3, oldNames = namesGADS(df3), newNames = c("var1", "var2"))
  expect_error(cbind(df1, df1), "The following variables are in multiple GADSdats: ID1, V1")

  out <- cbind(df1, df3)
  expect_equal(namesGADS(out), c("ID1", "V1", "var1", "var2"))
  expect_equal(out$dat$V1, out$dat$var2)
  expect_equal(nrow(out$labels), 4)
})
