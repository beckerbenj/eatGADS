# testM <- import_spss("tests/testthat/helper_spss_missings.sav")
# testM <- import_spss("helper_spss_missings.sav")
#
# test_that("errors", {
#   expect_error(calculateScale(testM, items = "VAR1", scale = "VARnew"),
#                "'items' needs to be a character vector of at least length 2.")
#   expect_error(calculateScale(testM, items = c("VAR1", "VAR2"), scale = c("VARnew", "VARnew2")),
#                "'scale' needs to be a character vector of length 1.")
# })
#
