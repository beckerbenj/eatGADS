

# load test data
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
allList <- mergeLabels(df1 = df1, df2 = df2)


### create
test_that("GADS DB creation", {
  expect_message(createGADS(allList = allList, pkList = pkList, fkList = fkList, filePath = ":memory:"),
                 "filePath points to work memory")
  expect_message(createGADS(allList = df1, pkList = list(df1 = "ID1"), filePath = ":memory:"),
                 "filePath points to work memory")
  expect_error(createGADS(allList = df1, pkList = "ID1", filePath = ":memory:"), "All input lists have to be named")
  expect_error(createGADS(allList = list(df1), pkList = list(df1 = "ID1"), filePath = ":memory:"), "no applicable method for 'createGADS' applied to an object of class \"list\"")
})
