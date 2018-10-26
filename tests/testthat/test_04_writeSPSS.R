

context("Write SPSS files")

# load test data
df <- getGADS(filePath = "helper_database.db")
label_df <- labelsGADS(filePath = "helper_database.db")

label_df_V2 <- label_df[which(label_df == "V2"), ]

expected_ID1 <- list(label = NA_character_,
                 format.spss = "F8.2",
                 display_width = NA_real_,
                 class = NA_character_)
expected_V2 <- list(label = "Variable 2",
                    format.spss = "F10.2",
                    display_width = NA_real_,
                    class = c("haven_labelled_spss", "haven_labelled"),
                    na_values = 99,
                    labels = c(mis = 99))

### check single variable label adding
test_that("Variable labels are added correctly to attributes, for single variable", {
  expect_equal(addLabels_single(label_df_V2), expected_V2)
})

### check all variable label adding (with one variable)
test_that("Variable labels are added correctly to attributes for all variables", {
  out <- addLabels(df, label_df)
  expect_equal(attributes(out$ID1), expected_ID1)
  expect_equal(attributes(out$V1), expected_ID1)
  expect_equal(attributes(out$V2), expected_V2)
})

### also for R variables
iris2 <- import_DF(iris)
expected_Species <- list(label = NA_character_,
                    format.spss = NA_character_,
                    display_width = NA_character_,
                    class = c("haven_labelled_spss", "haven_labelled"),
                    labels = c(setosa = 1, versicolor = 2, virginica = 3))

test_that("Variable labels are added correctly for factor", {
  expect_equal(addLabels_single(iris2$labels[iris2$labels$varName == "Species", ]),
               expected_Species)
})

### write SPSS
# tbd (but how to only temporarily write spss file?)
