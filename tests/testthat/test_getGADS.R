

# load test data
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
allList <- mergeLabels(df1 = df1, df2 = df2)

### gads data
# create expected data result
m1 <- plyr::join(df1$dat, df2$dat, match = "all", by = "ID1")
expected_ID2 <- unique(m1[, 1, drop = FALSE])
rownames(expected_ID2) <- NULL

test_that("GADS DB get all data", {
  out1 <- getGADS(filePath = "helper_dataBase.db")
  out2 <- getGADS(vSelect = c("V2", "V1", "ID1"), filePath = "helper_dataBase.db")
  expect_equal(out1$dat, m1)
  expect_equal(out2$dat, m1)
  expect_equal(out1$labels, expected_labels[c(1, 2, 4), -9])
})

test_that("GADS DB get one variable", {
  out <- getGADS(vSelect = "ID1", filePath = "helper_dataBase.db")
  expect_equal(out$dat, expected_ID2)
  expect_equal(out$labels, expected_labels[1, -9])
})

test_that("First list match", {
  all_nam <- namesGADS("helper_dataBase.db")
  x <- "ID1"
  expect_equal(first_list_match(x, all_nam), "df1")
})

test_that("Only first meta data for foreign keys is extracted", {
  # out1 <- getGADS(filePath = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase.db")
  out1 <- getGADS(filePath = "helper_dataBase.db")
  expect_equal(sum(out1$labels$varName == "ID1"), 1)
})
