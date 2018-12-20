
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

test_that("Fast getting GADSdat", {
  filePath <- paste(getwd(), "helper_dataBase.db", sep = "/")
  sink("aux")
  out <- eatGADS:::getGADS_fast(vSelect = "ID1", filePath = filePath, tempPath = "C:/Benjamin_Becker")
  sink()
  expect_equal(out$dat, expected_ID2)
  expect_equal(out$labels, expected_labels[1, -9])
  expect_error(getGADS_fast(vSelect = "ID1", filePath = filePath, tempPath = "C:/"), "User has no writing permission for tempPath.")
})

