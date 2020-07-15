
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")


test_that("compare_variables", {
  out <- compare_and_order(1:3, 3:1)
  expect_equal(out$in_both_ordered, 3:1)
  expect_warning(compare_and_order(1:3, 2:3, name2 = "set2"), "The following variables are not in set2: 1")
  out2 <- suppressWarnings(compare_and_order(1:3, 2:3, name2 = "set2"))
  expect_equal(out2$not_in_set2, 1)
  expect_warning(compare_and_order(1:3, 4:1, name1 = "set1"), "The following variables are not in set1: 4")
  out3 <- suppressWarnings(compare_and_order(1:3, 4:1, name1 = "set1"))
  expect_equal(out3$in_both_ordered, c(3:1))
})


test_that("Variable ordering in GADS", {
  expect_error(orderLike(df1, c()), "newOrder is not a character vector.")
  out <- orderLike(df1, c("V1", "ID1"))
  expect_equal(out$dat, df1$dat[, c(2, 1)])
  expect_equal(out$labels, df1$labels[2:1, ])
  out2 <- orderLike(dfSAV, c("VAR2", "VAR3", "VAR1"))
  expect_equal(out2$dat, dfSAV$dat[, c(2, 3, 1)])
  expect_equal(out2$labels, dfSAV$labels[c(4:5, 6:7, 1:3), ])
  # warnings
  warn1 <- capture_warnings(orderLike(dfSAV, c("VAR2", "VAR4")))
  expect_equal(warn1[[1]], "The following variables are not in GADSdat: VAR4")
  expect_equal(warn1[[2]], "The following variables are not in new Order: VAR1, VAR3")
  expect_warning(capture.output(orderLike(dfSAV, c("VAR2", "VAR3"))), "The following variables are not in new Order: VAR1")
  out3 <- suppressWarnings(orderLike(dfSAV, c("VAR2", "VAR3")))
  expect_equal(out3$dat, dfSAV$dat[, c(2, 3, 1)])
})
