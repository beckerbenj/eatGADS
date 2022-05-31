
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

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

test_that("Compare while ignoring order differences",{
  dfSAV2 <- dfSAV
  dfSAV2$labels <- dfSAV2$labels[c(2:1, 3:7), ]
  out <- equalGADS(dfSAV, dfSAV2)
  expect_equal(out$meta_data_differences, character())
})

test_that("Compare while ignoring irrelevant format differences",{
  dfSAV2 <- changeSPSSformat(dfSAV, "VAR1", format = "F8")
  out <- equalGADS(dfSAV, dfSAV2)
  expect_equal(out$meta_data_differences, character())
})


test_that("Compare two different GADSdat objects, large ID numbers",{
  df1_2 <- df1
  df1_2$dat$ID1 <- c(5140010110, 5140010111)
  df1$dat$ID1 <- c(5140010109, 5140010110)
  out <- equalGADS(df1, df1_2)
  expect_equal(out$data_differences, c("ID1"))
})

test_that("Compare two different GADSdat objects with varying tolerance",{
  df1_2 <- df1
  df1_2$dat$V1 <- c(3 + 1e-07, 5 + 1e-09)
  out <- equalGADS(df1, df1_2)
  expect_equal(out$data_differences, c("V1"))

  df1_2 <- df1
  df1_2$dat$V1 <- c(3 + 1e-07, 5 + 1e-09)
  out <- equalGADS(df1, df1_2, tolerance = 0.00001)
  expect_equal(out$data_differences, character())
})


test_that("Compare two GADSdat objects with metaExceptions",{
  df1_3 <- df1_2 <- df1
  df1_2$labels[1, "format"] <- "F8"
  df1_2$labels[2, "varLabel"] <- "F8"
  out <- equalGADS(df1, df1_2, metaExceptions = c("format", "varLabel"))
  expect_equal(out$names_not_in_1, character())
  expect_equal(out$names_not_in_2, character())
  expect_equal(out$data_differences, character())
  expect_equal(out$meta_data_differences, character())
  expect_equal(out$data_nrow, "all.equal")

  out2 <- equalGADS(df1, df1_2, metaExceptions = c("display_width"))
  expect_equal(out2$meta_data_differences, c("ID1", "V1"))
})
