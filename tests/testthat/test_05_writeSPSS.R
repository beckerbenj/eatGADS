

context("Write SPSS files")

### load test data
# df <- getGADS(filePath = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_database.db")
df <- getGADS(filePath = "helper_dataBase.db")
# label_df <- labelsGADS(filePath = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_database.db")
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
  expect_equal(addLabels_single(label_df_V2), expected_V2)
})

test_that("Variable labels are added correctly to attributes, for single character variable", {
  label_df_V2_string <- label_df_V2
  label_df_V2_string$format <- "A20"
  expect_equal(class(addLabels_single(label_df_V2_string)$labels), "character")
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
  # write_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_write_spss.sav")
  sav_path <- file.path(tempdir(), "helper_write_spss.sav")
  write_spss(df, filePath = sav_path)

  #test_df <- export_tibble(df)
  #test_df2 <- haven::read_sav("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/other_code/helper_write_spss_manual.sav", user_na = TRUE)
  #str(test_df)
  #str(test_df2)

  df2 <- import_spss(sav_path)
  # df2 <- import_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_write_spss.sav")
  expect_equal(df$dat, df2$dat)
  rownames(df$labels) <- NULL
  expect_equal(df$labels, df2$labels)
})


test_that("Full workflow with haven", {
  #test_df <- import_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_havenbug.sav")
  suppressWarnings(test_df <- import_spss("helper_spss_havenbug.sav"))
  test_tbl <- export_tibble(test_df)

  expect_silent(write_spss(test_df, filePath = tempfile()))
})


### Possible Problems when writing with haven
test_that("Check haven behaviour", {
  raw_df <- export_tibble(df)
  test3 <- test1 <- test2 <- raw_df

  attributes(test1$V2) <- list(label = "Variable - 2",
                      format.spss = "F10.2")
  expect_silent(haven::write_sav(test1, tempfile()))

  attributes(test2$V2) <- list(na_values = c(99, 98, 97, 95),
                               class = c("haven_labelled_spss", "haven_labelled"),
                               format.spss = "F10.2")
  expect_error(haven::write_sav(test2, tempfile()), "Writing failure: The number of defined missing values exceeds the format limit.")

  attributes(test3$V2) <- list(label = "Schul-ID",
                               display_width = 14,
                               format.spss = "F6.0",
                               class = c("haven_labelled_spss", "haven_labelled"))
  #expect_error(haven::write_sav(test3, "helper_write_spss_test.sav"))

  ## Note to myself: There is still a haven bug regarding wide string variable when reading (missing codes are dropped)
  # and writing (sometimes all missing codes and value labels are dropped; haven 2.2.0, 24.03.2020)
  # but it is difficult to produce a minimal reprex
})


#df <- data.frame(long = paste(rep("a", 1000), collapse = ""), stringsAsFactors = FALSE)
#haven::write_sav(df, path = "tests/testthat/helper_longstring.sav")

test_that("Write strings longer than 255", {
  #g <- import_spss("tests/testthat/helper_longstring.sav")
  g <- import_spss("helper_longstring.sav")
  f <- paste0(tempfile(), ".sav")
  write_spss(g, filePath = f)
  out <- haven::read_spss(f)
  expect_equal(dim(out), c(1, 1))
})





