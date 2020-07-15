

# load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_data.rda")
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
