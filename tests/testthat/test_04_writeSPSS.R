

context("Write SPSS files")

# load test data
df <- eatDB::dbPull(filePath = "helper_database.db")
label_df <- eatDB::dbSingleDF(filePath = "helper_database.db")


label_df_v1 <- label_df[which(label_df == "v1"), ]
label_df_v2 <- label_df[which(label_df == "v2"), ]
label_df_ID2 <- label_df[which(label_df == "ID2"), ]

### check single variable label adding
test_that("Variable labels are added correctly to attributes", {
  expected <- list(label = "var 1",
                   class = c("labelled_spss", "labelled"))
  expect_equal(addLabels_single(label_df_v1), expected)
})

test_that("Value labels are added correctly to attributes", {
  expected <- list(label = "var 2",
                   class = c("labelled_spss", "labelled"),
                   labels = c(One = 1))
  expect_equal(addLabels_single(label_df_v2), expected)
})

test_that("Variable labels are added correctly to attributes", {
  expected <- list(label = "ID var",
                   na_values = -99,
                   class = c("labelled_spss", "labelled"),
                   labels = c("NA" = -99))
  expect_equal(addLabels_single(label_df_ID2), expected)
})

### check all variable label adding (with one variable)
test_that("Variable labels are added correctly to attributes", {
  expected <- list(label = "ID var",
                   na_values = -99,
                   class = c("labelled_spss", "labelled"),
                   labels = c("NA" = -99))
  out <- addLabels(df, label_df)
  expect_equal(attributes(out$ID2),
               expected)
})


### write SPSS
# tbd (but how to only temporarily write spss file?)
