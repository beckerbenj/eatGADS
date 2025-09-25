### Name transformation
rawDat_names <- haven::read_spss(test_path("helper_spss_names.sav"), user_na = TRUE)

# load test data (df1, df2, pkList, fkList)
load(file = test_path("helper_data.rda"))

### check missings
df4 <- df3 <- df1
df3$labels[1, "missings"] <- "miss"
df4$labels[2, "valLabel"] <- "missing"
df4$labels[3, ] <- df4$labels[2, ]
# wrong valid value
df4$labels[3, "value"] <- -99
df4$labels[3, "valLabel"] <- "missing by design"
df4$labels[3, "missings"] <- "valid"
df4$labels[2:3, "labeled"] <- "yes"

df5a <- data.frame(good.name = 1:3,
                   group = letters[1:3],
                   aaaüaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaüaaa = LETTERS[1:3])
df5b <- data.frame(good_name = 1:3,
                   groupVar = letters[1:3],
                   aaaüaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa_tr = LETTERS[1:3])

GADS5a <- import_DF(df5a, checkVarNames = FALSE)
GADS5b <- import_DF(df5b, checkVarNames = FALSE)
rownames(GADS5b$labels) <- rownames(GADS5a$labels) <- NULL

allGADS5b <- allGADS5a <- expected_bigList
allGADS5a$datList$df2 <- allGADS5a$datList$df1 <- df5a
allGADS5a$allLabels <- rbind(GADS5a$labels, GADS5a$labels)
allGADS5b$datList$df2 <- allGADS5b$datList$df1 <- df5b
allGADS5b$allLabels <- rbind(GADS5b$labels, GADS5b$labels)
allGADS5b$datList$df2[2,] <- allGADS5a$datList$df2[2,] <- c(10, "x", "GO")
allGADS5b$allLabels$data_table <- allGADS5a$allLabels$data_table <- c(rep("df1", 3), rep("df2", 3))


test_that("Variable name are transformed correctly for character vectors", {
  expect_error(checkVarNames(c("group", "var.1", "Select", NA)),
               "Column names can not be NA.")
  all_messages <- capture_messages(out <- checkVarNames(c("group", "var.1", "Select", "test", "Test")))

  expect_equal(all_messages[1], "group has been renamed to groupVar\n")
  expect_equal(all_messages[2], "var.1 has been renamed to var_1\n")
  expect_equal(all_messages[3], "Select has been renamed to SelectVar\n")
  expect_equal(all_messages[4], "Test has been renamed to Test_2\n")
  expect_identical(out, c("groupVar", "var_1", "SelectVar", "test", "Test_2"))

  input <- c("group", "var.1", "Select", "test", "Test")
  out2 <- checkVarNames(input, checkDuplicates = FALSE, checkDots = FALSE, checkKeywords = FALSE)
  expect_equal(out2, input)
})

test_that("Variable name are transformed correctly for data.frames", {
  all_messages2 <- capture_messages(out2 <- checkVarNames(rawDat_names))

  expect_equal(all_messages2[1], "group has been renamed to groupVar\n")
  expect_equal(all_messages2[2], "var.1 has been renamed to var_1\n")
  expect_identical(names(out2), c("groupVar", "var_1"))
})

### Check VarNames
dot_df <- import_DF(iris, checkVarNames = FALSE)

test_that("Check varNames", {
  # if no changes
  expect_equal(checkVarNames(df1), df1)
  # if changes
  expect_message(checkVarNames(dot_df))
  changed_df <- suppressMessages(checkVarNames(dot_df))
  imported_df <- suppressMessages(import_DF(iris))
  expect_equal(changed_df, imported_df)
})

test_that("Unduplicate", {
  out <- unduplicate(c("v1", "V1", "v2"))
  expect_equal(out, c("v1", "V1_2", "v2"))
  out <- unduplicate(c("v1", "V1", "v2"))

  df <- data.frame("var1" = 1, "Var1" = 1, "vAr1" = 1)
  out2 <- prepare_labels(df, TRUE, TRUE)
  expect_equal(names(out2$dat), c("var1", "Var1_2", "vAr1_2_2"))
})

test_that("Check varNames all_GADSdat", {
  # if no changes
  expect_equal(checkVarNames(expected_bigList), expected_bigList)
  # if changes
  names(expected_bigList$datList$df1)[1] <- "group"
  names(expected_bigList$datList$df2)[1] <- "group"
  expected_bigList$allLabels[expected_bigList$allLabels$varName == "ID1", "varName"] <- "group"

  expect_message(checkVarNames(expected_bigList))
  #changed_df <- suppressMessages(checkVarNames(expected_bigList))
  #imported_df <- suppressMessages(import_DF(iris))
  #expect_equal(changed_df, imported_df)
})

test_that("Check and truncate long variable names", {
  name32char <- paste0(substr(names(df5b)[3], 0, 29), "_tr")
  df5c <- df5b
  names(df5c)[3] <- name32char
  GADS5c <- GADS5b
  names(GADS5c$dat)[3] <- name32char
  GADS5c$labels$varName[3] <- name32char
  allGADS5c <- allGADS5b
  names(allGADS5c$datList$df2)[3] <- names(allGADS5c$datList$df1)[3] <- name32char
  allGADS5c$allLabels$varName <- gsub(pattern = names(df5b)[3],
                                      replacement = name32char,
                                      x = allGADS5c$allLabels$varName)

  df_SPSS <- checkVarNames(df5a, charLimits = "SPSS")
  df_Stata <- checkVarNames(df5a, charLimits = "Stata")
  df_min <- checkVarNames(df5a, charLimits = c("Stata", "SPSS"))
  expect_equal(df_SPSS, df5b)
  expect_equal(df_Stata, df5c)
  expect_equal(df_min, df5c)

  GADS_SPSS <- checkVarNames(GADS5a, charLimits = "SPSS")
  GADS_Stata <- checkVarNames(GADS5a, charLimits = "Stata")
  GADS_min <- checkVarNames(GADS5a, charLimits = c("Stata", "SPSS"))
  expect_equal(GADS_SPSS, GADS5b)
  expect_equal(GADS_Stata, GADS5c)
  expect_equal(GADS_min, GADS5c)

  allGADS_SPSS <- checkVarNames(allGADS5a, charLimits = "SPSS")
  allGADS_Stata <- checkVarNames(allGADS5a, charLimits = "Stata")
  allGADS_min <- checkVarNames(allGADS5a, charLimits = c("Stata", "SPSS"))
  expect_equal(allGADS_SPSS, allGADS5b)
  expect_equal(allGADS_Stata, allGADS5c)
  expect_equal(allGADS_min, allGADS5c)
})

test_that("Check of byte length work irrespective of whether byte length == character length", {
  df6a <- data.frame(x = 1:5,
                     aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaueaaa = letters[1:5])
  df6b <- data.frame(x = 1:5,
                     aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaüaaa = letters[1:5])
  expect_equal(checkVarNames(df6a, charLimits = "SPSS"),
               checkVarNames(df6b, charLimits = "SPSS"))

  GADS6a <- import_DF(df6a, checkVarNames = FALSE)
  rownames(GADS6a$labels) <- NULL
  GADS6b <- import_DF(df6b, checkVarNames = FALSE)
  rownames(GADS6b$labels) <- NULL
  expect_equal(checkVarNames(GADS6a, charLimits = "SPSS"),
               checkVarNames(GADS6b, charLimits = "SPSS"))
})

test_that("Checks are only performed selectively, if requested", {
  # only keywords
  df_keywords_T <- checkVarNames(df5a, checkKeywords = TRUE, checkDots = FALSE)
  expect_equal(names(df_keywords_T)[2], names(df5b)[2])
  expect_equal(names(df_keywords_T)[c(1, 3)], names(df5a)[c(1, 3)])

  GADS_keywords_T <- checkVarNames(GADS5a, checkKeywords = TRUE, checkDots = FALSE)
  expect_equal(names(GADS_keywords_T$dat)[2], names(GADS5b$dat)[2])
  expect_equal(names(GADS_keywords_T$dat)[c(1, 3)], names(GADS5a$dat)[c(1, 3)])
  expect_equal(GADS_keywords_T$labels$varName[2], GADS5b$labels$varName[2])
  expect_equal(GADS_keywords_T$labels$varName[c(1, 3)], GADS5a$labels$varName[c(1, 3)])

  allGADS_keywords_T <- checkVarNames(allGADS5a, checkKeywords = TRUE, checkDots = FALSE)
  expect_equal(names(allGADS_keywords_T$datList$df1)[2], names(allGADS5b$datList$df1)[2])
  expect_equal(names(allGADS_keywords_T$datList$df2)[2], names(allGADS5b$datList$df2)[2])
  expect_equal(names(allGADS_keywords_T$datList$df1)[c(1, 3)], names(allGADS5a$datList$df1)[c(1, 3)])
  expect_equal(names(allGADS_keywords_T$datList$df2)[c(1, 3)], names(allGADS5a$datList$df2)[c(1, 3)])
  expect_equal(unique(allGADS_keywords_T$allLabels$varName)[2], unique(allGADS5b$allLabels$varName)[2])
  expect_equal(unique(allGADS_keywords_T$allLabels$varName)[c(1, 3)], unique(allGADS5a$allLabels$varName)[c(1, 3)])


  # only dots
  df_dots_T <- checkVarNames(df5a, checkKeywords = FALSE, checkDots = TRUE)
  expect_equal(names(df_dots_T)[1], names(df5b)[1])
  expect_equal(names(df_dots_T)[2:3], names(df5a)[2:3])

  GADS_dots_T <- checkVarNames(GADS5a, checkKeywords = FALSE, checkDots = TRUE)
  expect_equal(names(GADS_dots_T$dat)[1], names(GADS5b$dat)[1])
  expect_equal(names(GADS_dots_T$dat)[2:3], names(GADS5a$dat)[2:3])
  expect_equal(GADS_dots_T$labels$varName[1], GADS5b$labels$varName[1])
  expect_equal(GADS_dots_T$labels$varName[2:3], GADS5a$labels$varName[2:3])

  allGADS_dots_T <- checkVarNames(allGADS5a, checkKeywords = FALSE, checkDots = TRUE)
  expect_equal(names(allGADS_dots_T$datList$df1)[1], names(allGADS5b$datList$df1)[1])
  expect_equal(names(allGADS_dots_T$datList$df2)[1], names(allGADS5b$datList$df2)[1])
  expect_equal(names(allGADS_dots_T$datList$df1)[2:3], names(allGADS5a$datList$df1)[2:3])
  expect_equal(names(allGADS_dots_T$datList$df2)[2:3], names(allGADS5a$datList$df2)[2:3])
  expect_equal(unique(allGADS_dots_T$allLabels$varName)[1], unique(allGADS5b$allLabels$varName)[1])
  expect_equal(unique(allGADS_dots_T$allLabels$varName)[2:3], unique(allGADS5a$allLabels$varName)[2:3])


  # only length
  df_limits_T <- checkVarNames(df5a, checkKeywords = FALSE, checkDots = FALSE, charLimits = "SPSS")
  expect_equal(names(df_limits_T)[3], names(df5b)[3])
  expect_equal(names(df_limits_T)[1:2], names(df5a)[1:2])

  GADS_limits_T <- checkVarNames(GADS5a, checkKeywords = FALSE, checkDots = FALSE, charLimits = "SPSS")
  expect_equal(names(GADS_limits_T$dat)[3], names(GADS5b$dat)[3])
  expect_equal(names(GADS_limits_T$dat)[1:2], names(GADS5a$dat)[1:2])
  expect_equal(GADS_limits_T$labels$varName[3], GADS5b$labels$varName[3])
  expect_equal(GADS_limits_T$labels$varName[1:2], GADS5a$labels$varName[1:2])

  allGADS_limits_T <- checkVarNames(allGADS5a, checkKeywords = FALSE, checkDots = FALSE, charLimits = "SPSS")
  expect_equal(names(allGADS_limits_T$datList$df1)[3], names(allGADS5b$datList$df1)[3])
  expect_equal(names(allGADS_limits_T$datList$df2)[3], names(allGADS5b$datList$df2)[3])
  expect_equal(names(allGADS_limits_T$datList$df1)[1:2], names(allGADS5a$datList$df1)[1:2])
  expect_equal(names(allGADS_limits_T$datList$df2)[1:2], names(allGADS5a$datList$df2)[1:2])
  expect_equal(unique(allGADS_limits_T$allLabels$varName)[3], unique(allGADS5b$allLabels$varName)[3])
  expect_equal(unique(allGADS_limits_T$allLabels$varName)[1:2], unique(allGADS5a$allLabels$varName)[1:2])
})
