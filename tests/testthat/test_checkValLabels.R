dfSAV <- import_spss(file = test_path("helper_spss_missings.sav"))
load(file = test_path("helper_data.rda"))
default_out <- data.frame(varName = character(),
                          value = numeric(),
                          valLabel = character(),
                          length = numeric(),
                          unit = character(),
                          empty = logical())
gads_long_label <- dfSAV
stata_limit <- getProgramLimit("Stata", "valLabels")$value
spss_limit <- getProgramLimit("SPSS", "valLabels")$value
gads_long_label$labels[1, "valLabel"] <- paste0(rep("a", stata_limit + 1),
                                                collapse = "")


test_that("End early if there are no value labels to check", {
  expect_message(out <- checkValLabels(GADSdat = df1, charLimits = "SPSS"))
  expect_equal(out, default_out)
})

test_that("End early if all value labels are within the limits", {
  out1 <- checkValLabels(GADSdat = dfSAV, charLimits = "SPSS")
  expect_equal(out1, default_out)

  out2 <- checkValLabels(GADSdat = gads_long_label, charLimits = "Stata", vars = c("VAR2", "VAR3"))
  expect_equal(out2, default_out)
})

test_that("Correctly identify long value labels", {
  out1 <- checkValLabels(GADSdat = gads_long_label, charLimits = "Stata", printLength = 40)
  out2 <- checkValLabels(GADSdat = gads_long_label, charLimits = "Stata",
                         vars = "VAR1", printLength = 40)
  expected_out12 <- data.frame(varName = "VAR1",
                               value = -99,
                               valLabel = paste0(paste0(rep("a", 40),
                                                        collapse = ""),
                                                 "..."),
                               length = stata_limit + 1,
                               unit = "byte",
                               empty = FALSE)
  expect_equal(out1, expected_out12)
  expect_equal(out2, expected_out12)

  gads_long_byte <- gads_long_label
  gads_long_byte$labels[1, "valLabel"] <- paste0(rep("ä", round(stata_limit / 2) + 1),
                                                 collapse = "")
  out3 <- checkValLabels(GADSdat = gads_long_byte, charLimits = "Stata", printLength = 40)
  expected_out3 <- expected_out12
  expected_out3$length <- (round(stata_limit / 2) + 1) * 2
  expected_out3$valLabel <- paste0(paste0(rep("ä", 40),
                                          collapse = ""),
                                   "...")
  expect_equal(out3, expected_out3)
})

test_that("Use most restrictive limit", {
  gads_less_long_label <- gads_long_label
  gads_less_long_label$labels[1, "valLabel"] <- paste0(rep("a", spss_limit + 1),
                                                       collapse = "")
  out1 <- checkValLabels(GADSdat = gads_less_long_label, charLimits = "Stata", printLength = 40)
  out2 <- checkValLabels(GADSdat = gads_less_long_label, charLimits = "SPSS", printLength = 40)
  out3 <- checkValLabels(GADSdat = gads_less_long_label, charLimits = c("Stata", "SPSS"),
                         printLength = 40)
  expected_out <- data.frame(varName = "VAR1",
                             value = -99,
                             valLabel = paste0(paste0(rep("a", 40),
                                                      collapse = ""),
                                               "..."),
                             length = spss_limit + 1,
                             unit = "byte",
                             empty = FALSE)
  expect_equal(out1, default_out)
  expect_equal(out2, expected_out)
  expect_equal(out3, expected_out)
})

test_that("Return untruncated label on demand", {
  out <- checkValLabels(GADSdat = gads_long_label, charLimits = "Stata", printLength = NULL)
  expected_out <- data.frame(varName = "VAR1",
                             value = -99,
                             valLabel = paste0(rep("a", stata_limit + 1),
                                               collapse = ""),
                             length = stata_limit + 1,
                             unit = "byte",
                             empty = FALSE)
  expect_equal(out, expected_out)
})

test_that("Report on empty long labels", {
  gads_long_empty_label <- gads_long_label
  gads_long_empty_label$dat[gads_long_empty_label$dat$VAR1 == -99, "VAR1"] <- -97
  out <- checkValLabels(GADSdat = gads_long_empty_label, charLimits = "Stata", printLength = 40)
  expected_out <- data.frame(varName = "VAR1",
                             value = -99,
                             valLabel = paste0(paste0(rep("a", 40),
                                                      collapse = ""),
                                               "..."),
                             length = stata_limit + 1,
                             unit = "byte",
                             empty = TRUE)
  expect_equal(out, expected_out)
})
