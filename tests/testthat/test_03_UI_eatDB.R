
context("UI for eatDB")

# load test data
load(file = "helper_data.rda")
allList <- mergeLabels(df1 = df1, df2 = df2)


### create
test_that("GADS DB creation", {
  expect_message(createGADS(allList = allList, pkList = pkList, fkList = fkList, filePath = ":memory:"),
                 "filePath points to work memory")
})


### names
test_that("GADS DB names", {
  expect_identical(namesGADS(filePath = "helper_dataBase.db"),
                 list(df1 = c("ID1", "V1"), df2 = c("ID1", "V2")))
})

### labels
test_that("GADS DB labels", {
  expect_identical(labelsGADS(filePath = "helper_dataBase.db"),
                   expected_labels)
})


### gads data
# create expected data result
m1 <- merge(df1$dat, df2$dat, all = TRUE)
expected_all <- m1[, c(2, 1, 3)]
expected_ID2 <- unique(m1[, 1, drop = FALSE])
rownames(expected_ID2) <- NULL

test_that("GADS DB get all data", {
  expect_equal(getGADS(filePath = "helper_dataBase.db"),
                   m1)
})

test_that("GADS DB get one variable", {
  expect_equal(getGADS(vSelect = "ID1", filePath = "helper_dataBase.db"),
               expected_ID2)
})
