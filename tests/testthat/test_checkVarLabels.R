dfSAV <- import_spss(file = test_path("helper_spss_missings.sav"))
load(file = test_path("helper_data.rda"))
default_out <- data.frame(varName = character(),
                          varLabel = character(),
                          length = numeric(),
                          unit = character())
gads_long_label <- dfSAV
stata_limit <- getProgramLimit("Stata", "varLabels")$value
spss_limit <- getProgramLimit("SPSS", "varLabels")$value
gads_long_label$labels[1, "varLabel"] <- paste0(rep("a", stata_limit + 1),
                                                collapse = "")


test_that("End early if there are no variable labels to check", {
  expect_message(out <- checkVarLabels(GADSdat = df1, charLimits = "SPSS"))
  expect_equal(out, default_out)
})

test_that("End early if all variable labels are within the limits", {
  out1 <- checkVarLabels(GADSdat = dfSAV, charLimits = "SPSS")
  expect_equal(out1, default_out)

  out2 <- checkVarLabels(GADSdat = gads_long_label, charLimits = "Stata", vars = c("VAR2", "VAR3"))
  expect_equal(out2, default_out)
})

test_that("Correctly identify long variable labels", {
  out1 <- checkVarLabels(GADSdat = gads_long_label, charLimits = "Stata", printLength = 40)
  out2 <- checkVarLabels(GADSdat = gads_long_label, charLimits = "Stata",
                         vars = "VAR1", printLength = 40)
  expected_out <- data.frame(varName = "VAR1",
                             varLabel = paste0(paste0(rep("a", 40),
                                                      collapse = ""),
                                               "..."),
                             length = stata_limit + 1,
                             unit = "char")
  expect_equal(out1, expected_out)
  expect_equal(out2, expected_out)
})

test_that("Use most restrictive limit", {
  out1 <- checkVarLabels(GADSdat = gads_long_label, charLimits = "SPSS", printLength = 40)
  out2 <- checkVarLabels(GADSdat = gads_long_label, charLimits = c("Stata", "SPSS"),
                         printLength = 40)
  expected_out <- data.frame(varName = "VAR1",
                             varLabel = paste0(paste0(rep("a", 40),
                                                      collapse = ""),
                                               "..."),
                             length = stata_limit + 1,
                             unit = "char")
  expect_equal(out1, default_out)
  expect_equal(out2, expected_out)
})

test_that("Return untruncated label on demand", {
  out <- checkVarLabels(GADSdat = gads_long_label, charLimits = "Stata", printLength = NULL)
  expected_out <- data.frame(varName = "VAR1",
                             varLabel = paste0(rep("a", stata_limit + 1),
                                               collapse = ""),
                             length = stata_limit + 1,
                             unit = "char")
  expect_equal(out, expected_out)
})
