### load test data
# df <- getGADS(filePath = "tests/testthat/helper_database.db")
df <- getGADS(filePath = "helper_dataBase.db")
# label_df <- labelsGADS(filePath = "tests/testthat/helper_database.db")
label_df <- labelsGADS(filePath = "helper_dataBase.db")
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")


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
  expect_equal(addLabels_single(varName = "V2", label_df = label_df_V2, raw_dat = df$dat$V2),
               expected_V2)
})

test_that("Variable labels are added correctly to attributes, for single character variable", {
  label_df_V2_string <- label_df_V2
  label_df_V2_string$format <- "A20"
  df2  <- df
  df2$dat$V2 <- as.character(df2$dat$V2)
  expect_equal(class(addLabels_single(varName = "V2", label_df = label_df_V2_string, raw_dat = df2$dat$V2)$labels),
               "character")
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
  expect_equal(addLabels_single(varName = "Species", label_df = iris2$labels[iris2$labels$varName == "Species", ],
                                raw_dat = iris2$dat$Species),
               expected_Species)
})

### To many missings defined for discrete missing values
dfSAV2 <- changeMissings(dfSAV, varName = "VAR1", value = c(-90, -80), missings = c("miss", "miss"))
miss_labs <- dfSAV2$labels[dfSAV2$labels$varName == "VAR1", ]

test_that("add_miss_tags", {
  out <- add_miss_tags(varName = "VAR1", attr_list = list(), label_df = miss_labs, raw_dat = dfSAV$dat$VAR1)
  expect_equal(out, list(na_range = c(-99, -80)))
})

test_that("add_miss_tags errors", {
  # conflict in data
  miss_dat2 <- dfSAV$dat$VAR1
  miss_dat2[c(1, 4)] <- c(-85, -86)

  expect_error(add_miss_tags(varName = "VAR1", attr_list = list(),
                                       label_df = miss_labs, raw_dat = miss_dat2),
               "Conversion of missing tags for variable 'VAR1' to SPSS conventions is not possible, as the new missing range (-99 to -80) would include the following values not tagged as missing in the data: -85, -86. Adjust missing tags to export to tibble or write to SPSS.", fixed = TRUE)

  # conflict in value labels
  dfSAV3 <- changeValLabels(dfSAV2, varName = "VAR1", value = c(-85), valLabel = c("some label"))
  miss_labs3 <- dfSAV3$labels[dfSAV3$labels$varName == "VAR1", ]

  expect_error(add_miss_tags(varName = "VAR1", attr_list = list(),
                             label_df = miss_labs3, raw_dat = dfSAV$dat$VAR1),
               "Conversion of missing tags for variable 'VAR1' to SPSS conventions is not possible, as the new missing range (-99 to -80) would include the following labeled values not tagged as missing: -85. Adjust missing tags to export to tibble or write to SPSS.", fixed = TRUE)

  # character variable
  dfSAV4 <- dfSAV2
  dfSAV4$dat$VAR1[c(1, 4)] <- c("a", "b")
  dfSAV4 <- changeSPSSformat(dfSAV4, varName = "VAR1", format = "A10")
  miss_labs4 <- dfSAV2$labels[dfSAV2$labels$varName == "VAR1", ]

  expect_error(add_miss_tags(varName = "VAR1", attr_list = list(),
                             label_df = miss_labs4, raw_dat = dfSAV4$dat$VAR1),
               "Conversion of missing tags for variable 'VAR1' to SPSS conventions is not possible, as too many missings are declared for a character variable (maximum 3). Adjust missing tags to export to tibble or write to SPSS.", fixed = TRUE)


})


