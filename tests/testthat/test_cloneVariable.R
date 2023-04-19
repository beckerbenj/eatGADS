
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

test_that("Errors", {
  expect_error(cloneVariable(dfSAV, varName = "VAR2", new_varName = "VAR3"),
               "'VAR3' is already an existing variable in the 'GADSdat'.")
})

test_that("Clone variable", {
  out <- cloneVariable(dfSAV, varName = "VAR1", new_varName = "VAR1_new")
  expect_equal(namesGADS(out), c("VAR1", "VAR2", "VAR3", "VAR1_new"))
  expect_equal(out$dat$VAR1, out$dat$VAR1_new)

  meta1 <- extractMeta(dfSAV, "VAR1")
  meta2 <- extractMeta(out, "VAR1_new")
  row.names(meta1) <- row.names(meta2) <- NULL
  expect_equal(meta1[, -1], meta2[, -1])

})

test_that("Append varLabel", {
  out <- cloneVariable(dfSAV, varName = "VAR1", new_varName = "VAR1_new", label_suffix = "(recoded)")
  expect_equal(out$labels[8, 2], c("Variable 1 (recoded)"))
})
