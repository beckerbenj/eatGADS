
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")


test_that("Drop missing labels from meta", {
  # if no changes
  expect_equal(drop_missing_labels(df1$labels[df1$labels$varName == "ID1", ]), df1$labels[df1$labels$varName == "ID1", ])
  # if changes
  out <- dfSAV$labels[3, ]
  row.names(out) <- NULL
  expect_equal(drop_missing_labels(dfSAV$labels[dfSAV$labels$varName == "VAR1", ]), out)
  # if no value labels left
  expect_equal(drop_missing_labels(dfSAV$labels[dfSAV$labels$varName == "VAR3", ]),
               data.frame(varName = "VAR3", varLabel = "Variable 3", format = "F8.2", display_width = NA_real_, labeled = "no", value = NA_real_, valLabel = NA_character_, missings = NA_character_, stringsAsFactors = FALSE))
})

test_that("Transfer meta information from one GADSdat to another", {
  dat2 <- import_DF(dfSAV$dat)
  dat3 <- reuseMeta(dat2, varName = "VAR1", dfSAV)
  dat3 <- reuseMeta(dat3, varName = "VAR2", dfSAV)
  dat3 <- reuseMeta(dat3, varName = "VAR3", dfSAV)
  expect_equal(dfSAV, dat3)
  dat4 <- changeVarNames(dat2, oldNames = "VAR1", newNames = "v1")
  dat5 <- reuseMeta(dat4, varName = "v1", dfSAV, other_varName = "VAR1")
  expect_silent(check_GADSdat(dat5))
})

test_that("Transfer meta information from all_GADSdat to GADSdat", {
  all_g <- mergeLabels(df1 = dfSAV, df2 = dfSAV)
  dat2 <- import_DF(dfSAV$dat)
  dat3 <- reuseMeta(dat2, varName = "VAR1", all_g)
  dat3 <- reuseMeta(dat3, varName = "VAR2", all_g)
  dat3 <- reuseMeta(dat3, varName = "VAR3", all_g)
  expect_equal(dfSAV, dat3)
})

test_that("Use reuseMeta for combining value labels, including adapting meta data on variable level", {
  df <- dfSAV$dat[, 2, drop = FALSE]
  new_dfSAV <- updateMeta(dfSAV, df)
  new_dfSAV$labels <- new_dfSAV$labels[1, ]
  new_dfSAV$labels[, "value"] <- 5
  test <- reuseMeta(dfSAV, varName = "VAR1", other_GADSdat = new_dfSAV, other_varName = "VAR2", addValueLabels = TRUE)
  test_labels <- test$labels[test$labels$varName == "VAR1", ]
  expect_equal(test_labels$value, c(-99, -96, 1, 5))
  expect_equal(unique(test_labels$varLabel), "Variable 1")
  test2 <- reuseMeta(dfSAV, varName = "VAR1", other_GADSdat = new_dfSAV, other_varName = "VAR2", addValueLabels = FALSE)
  test2_labels <- test2$labels[test2$labels$varName == "VAR1",]
  expect_equal(test2_labels$value, c(5))
  expect_equal(unique(test2_labels$varLabel), "Variable 2")
})

test_that("Reuse meta with special missing treatment", {
  dat2 <- import_DF(dfSAV$dat)
  expect_error(reuseMeta(dat2, varName = "VAR1", dfSAV, missingLabels = "drp"), "Invalid input for argument missingLabels.")
  dat3 <- reuseMeta(dat2, varName = "VAR1", dfSAV, missingLabels = "drop")
  expect_equal(nrow(dat3$labels), 3)
  expect_equal(dat3$labels[1, "value"], 1)
  dat2 <- import_DF(dfSAV$dat)
  dat3 <- reuseMeta(dfSAV, varName = "VAR1", dat2, missingLabels = "leave")
  expect_equal(nrow(dat3$labels), 6)
  expect_equal(dat3$labels[1, "value"], -99)
  dat3 <- reuseMeta(dfSAV, varName = "VAR1", dat2, missingLabels = "only")
  expect_equal(nrow(dat3$labels), 5)
  expect_equal(dat3$labels[1, "value"], NA_real_)
  dat5 <- reuseMeta(dat2, varName = "VAR1", dfSAV, missingLabels = "only")
  expect_equal(nrow(dat5$labels), 4)
  expect_equal(dat5$labels[1:2, "value"], c(-99, -96))
  expect_equal(dat5$labels[1:2, "valLabel"], c("By design", "Omission"))
  expect_equal(dat5$labels[1:2, "missings"], c("miss", "miss"))
})

test_that("Reuse meta adding value labels to an unlabeled variable", {
  out <- reuseMeta(df1, varName = "V1", other_GADSdat = dfSAV, other_varName = "VAR1", addValueLabels = TRUE)
  expect_equal(out$labels[2, "labeled"], "yes")
  expect_equal(out$labels[2, "valLabel"], "By design")
})

test_that("Bugfix if only missing rows and missingLabels = leave", {
  out <- reuseMeta(dfSAV, varName = "VAR3", other_GADSdat = dfSAV, other_varName = "VAR1", missingLabels = "leave")
  expect_equal(nrow(out$labels), 8)
})
