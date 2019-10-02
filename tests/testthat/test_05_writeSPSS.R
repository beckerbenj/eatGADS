

context("Write SPSS files")

### load test data
# df <- getGADS(filePath = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_database.db")
df <- getGADS(filePath = "helper_database.db")
# label_df <- labelsGADS(filePath = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_database.db")
label_df <- labelsGADS(filePath = "helper_database.db")

label_df_V2 <- label_df[which(label_df == "V2"), ]

expected_ID1 <- list(format.spss = "F8.2")
expected_V2 <- list(label = "Variable 2",
                    format.spss = "F10.2",
                    class = c("haven_labelled_spss", "haven_labelled"),
                    na_values = 99,
                    labels = c(mis = 99))

### check single variable label adding
test_that("Variable labels are added correctly to attributes, for single variable", {
  expect_equal(addLabels_single(label_df_V2), expected_V2)
})

### check export_tibble: all variable label adding (with one variable)
test_that("Variable labels are added correctly to attributes for all variables", {
  out <- export_tibble(df)
  expect_equal(attributes(out$ID1), expected_ID1)
  expect_equal(attributes(out$V1), expected_ID1)
  expect_equal(attributes(out$V2), expected_V2)
  expect_equal(class(out)[1], "tbl_df")
})

### also for R variables
iris2 <- import_DF(iris)
expected_Species <- list(class = c("haven_labelled_spss", "haven_labelled"),
                    labels = c(setosa = 1, versicolor = 2, virginica = 3))

test_that("Variable labels are added correctly for factor", {
  expect_equal(addLabels_single(iris2$labels[iris2$labels$varName == "Species", ]),
               expected_Species)
})

### To many missings defined for discrete missing values
test_that("Variable labels are added correctly for factor", {
  df_test <- df
  df_test$labels[4, ] <- df_test$labels[3, ]
  df_test$labels[4, "value"] <- 98
  df_test$labels[5, ] <- df_test$labels[3, ]
  df_test$labels[5, "value"] <- 97
  df_test$labels[6, ] <- df_test$labels[3, ]
  df_test$labels[6, "value"] <- 96
  test_labeled <- addLabels_single(df_test$labels[df_test$labels$varName == "V2", ])
  expect_equal(test_labeled$na_range,c(96, 99))
})



### write SPSS
test_that("GADSdat correctly written to sav", {
  write_spss(df, "helper_write_spss.sav")

  df2 <- import_spss("helper_write_spss.sav")
  # df2 <- import_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_write_spss.sav")
  expect_equal(df$dat, df2$dat)
  rownames(df$labels) <- NULL
  expect_equal(df$labels, df2$labels)
})

### Possible Problems when writing with haven
test_that("Check haven behaviour", {
  raw_df <- export_tibble(df)
  test3 <- test1 <- test2 <- raw_df

  attributes(test1$V2) <- list(label = "Variable - 2",
                      format.spss = "F10.2")
  expect_silent(haven::write_sav(test1, "helper_write_spss_test.sav"))

  attributes(test2$V2) <- list(na_values = c(99, 98, 97, 95),
                               class = c("haven_labelled_spss", "haven_labelled"),
                               format.spss = "F10.2")
  expect_error(haven::write_sav(test2, "helper_write_spss_test.sav"))

  attributes(test3$V2) <- list(label = "Schul-ID",
                               display_width = 14,
                               format.spss = "F6.0",
                               class = c("haven_labelled_spss", "haven_labelled"))
  expect_error(haven::write_sav(test3, "helper_write_spss_test.sav"))
})

