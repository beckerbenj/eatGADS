
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# dfSAV <- import_spss(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

test_that("compare_meta", {
  m2 <- m1 <- dfSAV$labels
  expect_equal(compare_meta(m2, m1), character())
  m2[3, "value"] <- 2
  out <- suppressMessages(compare_meta(m2, m1))
  expect_equal(out, "VAR1")
  m2 <- m1
  m2[7, "missings"] <- "valid"
  m2[4, "valLabel"] <- "test"
  expect_message(compare_meta(m2, m1), "The following variables have different meta data on value level: VAR2, VAR3")
  out <- suppressMessages(compare_meta(m2, m1))
  expect_equal(out, c("VAR2", "VAR3"))

})

test_that("compare_meta for one with only missing meta data and one no value level meta data", {
  df1_miss <- reuseMeta(df1, "ID1", dfSAV, "VAR3")
  expect_equal(compare_meta(df1_miss, df1), character(0))
})

