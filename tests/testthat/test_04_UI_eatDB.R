
context("UI for eatDB")

# load test data
# load(file = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_data.rda")
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


### names
test_that("GADS DB names", {
  expect_identical(namesGADS(GADS = "helper_dataBase.db"),
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


test_that("Fast getting GADSdat", {
  filePath <- file.path(getwd(), "helper_dataBase.db")
  sink("aux")
  out <- eatGADS:::getGADS_fast(vSelect = "ID1", filePath = filePath)
  sink()
  expect_equal(out$dat, expected_ID2)
  expect_equal(out$labels, expected_labels[1, -9])
  expect_error(getGADS_fast(vSelect = "ID1", filePath = filePath, tempPath = "C:/"), "User has no writing permission for tempPath.")
})

### test via hand, because package has to be unattached for testing
# "Automatic File Deletion of Fast getting GADSdat"
# dat <- getGADS_fast(filePath = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase.db", tempPath = "c:/Benjamin_Becker")
# dat2 <- getGADS_fast(vSelect = "IDSTUD", filePath = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase.db", tempPath = "c:/Benjamin_Becker")
# detach("package:eatGADS", unload=TRUE)
# gc()
