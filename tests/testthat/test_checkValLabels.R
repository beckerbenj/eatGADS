dfSAV <- import_spss(file = test_path("helper_spss_missings.sav"))
load(file = test_path("helper_data.rda"))

test_that("Correctly identify long value labels", {
  stata_limit <- getProgramLimit("Stata", "valLabels")$value
  gads_meta <- dfSAV$labels
  gads_meta$valLabel[1] <- paste0(rep("a", stata_limit + 1),
                                  collapse = "")
  gads <- dfSAV
  gads$labels <- gads_meta
  out <- checkValLabels(GADSdat = gads, charLimits = "Stata", printLength = 40)
  expected_out <- data.frame(varName = "VAR1",
                             value = -99,
                             valLabel = paste0(rep("a", 40),
                                               collapse = ""),
                             charLength = stata_limit + 1,
                             empty = FALSE)
  expect_equal(out, expected_out)
})
