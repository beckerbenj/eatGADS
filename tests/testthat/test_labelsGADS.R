
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

### labels
test_that("GADS DB labels", {
  expect_identical(labelsGADS(filePath = "helper_dataBase.db"),
                   expected_labels)
})
