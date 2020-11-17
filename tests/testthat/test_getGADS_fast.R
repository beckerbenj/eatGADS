
# load test data
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
allList <- mergeLabels(df1 = df1, df2 = df2)

### gads data
# create expected data result
m1 <- plyr::join(df1$dat, df2$dat, match = "all", by = "ID1")
expected_ID2 <- unique(m1[, 1, drop = FALSE])
rownames(expected_ID2) <- NULL


test_that("Fast getting GADSdat", {
  #filePath <- file.path(getwd(), "tests/testthat/helper_dataBase.db")
  filePath <- file.path(getwd(), "helper_dataBase.db")
  # sink produces error; if necessary add capture.output
  stuff <- capture_output(out <- eatGADS:::getGADS_fast(vSelect = "ID1", filePath = filePath))
  #sink()
  expect_equal(out$dat, expected_ID2)
  expect_equal(out$labels, expected_labels[1, -9])
  #expect_error(getGADS_fast(vSelect = "ID1", filePath = filePath, tempPath = "C:/"), "User has no writing permission for tempPath.")

  filePath2 <- gsub("/", "\\\\", filePath)
  #stuff <- capture_output(out2 <- eatGADS:::getGADS_fast(vSelect = "ID1", filePath = filePath))
  #expect_equal(out2$dat, expected_ID2)

})

test_that("Fast getting GADSdat invalid path", {
  f <- "c:/some_path00/canbeguaranteedtobenotreal/some_file"
  expect_error(getGADS_fast(filePath = f),
               "c:/some_path00/canbeguaranteedtobenotreal/some_file is not a valid path to a data base")
})


### test via hand, because package has to be unattached for testing
# "Automatic File Deletion of Fast getting GADSdat"
# dat <- getGADS_fast(filePath = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase.db", tempPath = "c:/Benjamin_Becker")
# dat2 <- getGADS_fast(vSelect = "IDSTUD", filePath = "C:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dataBase.db", tempPath = "c:/Benjamin_Becker")
# detach("package:eatGADS", unload=TRUE)
# gc()
