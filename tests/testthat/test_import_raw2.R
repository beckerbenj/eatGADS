
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

test_that("import_raw2 errors",{
  expect_error(import_raw2(dat = 1, labels = df1$labels), "'dat' needs to be a data frame.")
  expect_error(import_raw2(dat = df1$dat, labels = 1), "'labels' needs to be a data frame.")
  dat2 <- df1$dat
  dat2$ID1 <- as.factor(dat2$ID1)
  expect_error(import_raw2(dat = dat2, labels = df1$labels), "At least one of the variables in 'dat' is a factor. All meta information on value level has to be stored in valLabels.")
})

test_that("import_raw2",{
  out <- import_raw2(dat = df1$dat, labels = df1$labels)
  expect_equal(df1, out)
})

test_that("avoiding integers",{
  dat <- df1$dat
  dat$V1 <- as.integer(dat$V1)
  labels <- df1$labels
  labels$value <- as.integer(labels$value)

  out <- import_raw2(dat = dat, labels = labels)
  expect_false(is.integer(out$dat$V1))
  expect_false(is.integer(out$labels$value))
})

