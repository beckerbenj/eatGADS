
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

test_that("GADS DB names", {
  expect_identical(namesGADS(GADS = "helper_dataBase.db"),
                   list(df1 = c("ID1", "V1"), df2 = c("ID1", "V2")))
})

test_that("GADSdat names", {
  expect_identical(namesGADS(df1),
                   c("ID1", "V1"))
})

test_that("all_GADSdat names", {
  expect_identical(namesGADS(expected_bigList),
                   list(df1 = c("ID1", "V1"), df2 = c("ID1", "V2")))
})
