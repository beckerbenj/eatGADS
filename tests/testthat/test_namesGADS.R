

### names
test_that("GADS DB names", {
  expect_identical(namesGADS(GADS = "helper_dataBase.db"),
                   list(df1 = c("ID1", "V1"), df2 = c("ID1", "V2")))
})
