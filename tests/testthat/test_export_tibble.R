### load test data
# df <- getGADS(filePath = "tests/testthat/helper_database.db")
df <- getGADS(filePath = "helper_dataBase.db")
# label_df <- labelsGADS(filePath = "tests/testthat/helper_database.db")
label_df <- labelsGADS(filePath = "helper_dataBase.db")

label_df_V2 <- label_df[which(label_df == "V2"), ]

expected_ID1 <- list(format.spss = "F8.2")
expected_V2 <- list(label = "Variable 2",
                    format.spss = "F10.2",
                    na_values = 99,
                    class = c("haven_labelled_spss", "haven_labelled"),
                    labels = c(mis = 99))


###
test_that("Check for variable type and format.spss", {
  expect_silent(check_var_type(df))
  df3 <- df2 <- df
  df2$labels[3, "format"] <- "A12"
  expect_error(check_var_type(df2), "Incompatible R variable type and format.spss for variable V2")

  df3$dat[, "V1"] <- as.character(df3$dat[, "V1"])
  expect_error(check_var_type(df3), "Incompatible R variable type and format.spss for variable V1")
})


### check single variable label adding
test_that("Variable labels are added correctly to attributes, for single variable", {
  expect_equal(addLabels_single(label_df_V2, varClass = "numeric"), expected_V2)
})

test_that("Variable labels are added correctly to attributes, for single character variable", {
  label_df_V2_string <- label_df_V2
  label_df_V2_string$format <- "A20"
  expect_equal(class(addLabels_single(label_df_V2_string, "character")$labels), "character")
})

test_that("Value labels work correctly for character variable with and without SPSS format", {
  PlantGrowth$group <- as.character(PlantGrowth$group)
  suppressMessages(g <- import_DF(PlantGrowth))
  g$labels[2, "value"] <- -99
  g$labels[2, "valLabel"] <- "miss"
  g$labels[2, "missings"] <- "miss"
  g$labels[2, "labeled"] <- "yes"
  g2 <- g

  # with SPSS format
  g$labels[2, "format"] <- "A10"
  out <- export_tibble(g)
  expect_true(is.character(attr(out$groupVar, "labels")))
  expect_silent(write_spss(g, filePath = tempfile(fileext = ".sav")))

  # with SPSS format
  out2 <- export_tibble(g2)
  expect_true(is.character(attr(out2$groupVar, "labels")))
  expect_silent(write_spss(g2, filePath = tempfile(fileext = ".sav")))
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
iris_fac <- iris
iris_fac$Species <- as.factor(iris$Species)
iris2 <- import_DF(iris)
expected_Species <- list(class = c("haven_labelled"),
                         labels = c(setosa = 1, versicolor = 2, virginica = 3))

test_that("Variable labels are added correctly for factor", {
  expect_equal(addLabels_single(iris2$labels[iris2$labels$varName == "Species", ], varClass = "numeric"),
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
  test_labeled <- addLabels_single(df_test$labels[df_test$labels$varName == "V2", ], varClass = "numeric")
  expect_equal(test_labeled$na_range,c(96, 99))
})
