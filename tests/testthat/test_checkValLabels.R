dfSAV <- import_spss(file = test_path("helper_spss_missings.sav"))
load(file = test_path("helper_data.rda"))
default_out <- data.frame(varName = character(),
                          value = numeric(),
                          valLabel = character(),
                          charLength = numeric(),
                          empty = logical())
gads_long_label <- dfSAV
stata_limit <- getProgramLimit("Stata", "valLabels")$value
gads_long_label$labels$valLabel[1] <- paste0(rep("a", stata_limit + 1),
                                             collapse = "")


test_that("End early if there are no value labels to check", {
  expect_warning(out <- checkValLabels(GADSdat = df1, charLimits = "SPSS"))
  expect_equal(out, default_out)
})

test_that("End early if no value labels are too long", {
  out1 <- checkValLabels(GADSdat = dfSAV, charLimits = "SPSS")
  expect_equal(out1, default_out)

  out2 <- checkValLabels(GADSdat = gads_long_label, charLimits = "Stata", vars = c("VAR2", "VAR3"))
  expect_equal(out2, default_out)
})

test_that("Correctly identify long value labels", {
  out1 <- checkValLabels(GADSdat = gads_long_label, charLimits = "Stata", printLength = 40)
  out2 <- checkValLabels(GADSdat = gads_long_label, charLimits = "Stata",
                         vars = "VAR1", printLength = 40)
  expected_out <- data.frame(varName = "VAR1",
                             value = -99,
                             valLabel = paste0(paste0(rep("a", 40),
                                                      collapse = ""),
                                               "..."),
                             charLength = stata_limit + 1,
                             empty = FALSE)
  expect_equal(out1, expected_out)
  expect_equal(out2, expected_out)
})

test_that("Return untruncated label on demand", {
  out <- checkValLabels(GADSdat = gads_long_label, charLimits = "Stata", printLength = NULL)
  expected_out <- data.frame(varName = "VAR1",
                             value = -99,
                             valLabel = paste0(rep("a", stata_limit + 1),
                                               collapse = ""),
                             charLength = stata_limit + 1,
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
                             charLength = stata_limit + 1,
                             empty = TRUE)
  expect_equal(out, expected_out)
})
