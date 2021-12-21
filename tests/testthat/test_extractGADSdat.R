

# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")


### Extracting GADSdat from all_GADSdat (reverse to merge above)
test_that("Checks performed correctly", {
  expect_error(extractGADSdat(df1, "df1"))
  expect_error(extractGADSdat(expected_bigList, "df3"), "name has to be the name of a GADSdat element of all_GADSdat.")
  expect_error(extractGADSdat(expected_bigList, c("df1", "df2")), "name has to be a character vector of length 1.")
})

test_that("Extract GADSdat performed correctly", {
  expect_equal(extractGADSdat(expected_bigList, "df1"), df1)
  expect_equal(extractGADSdat(expected_bigList, "df2"), df2)
})


test_that("Extract GADSdat for trendGADSdat", {
  out <- getTrendGADS(filePaths = c("helper_dataBase.db", "helper_dataBase2.db"), years = c(2012, 2018),
                      fast = FALSE, verbose = FALSE)
  g1 <- extractGADSdat(out, name = "gads2012")
  g2 <- extractGADSdat(out, name = "gads2018")
  g1 <- removeVars(g1, "year")
  g2 <- removeVars(g2, "year")
  expect_equal(g1, g2)
})
