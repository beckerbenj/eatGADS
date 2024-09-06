# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")
# dfSAV <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
dfSAV <- import_spss(file = "helper_spss_missings.sav")

df <- data.frame(id = c(110, 115, 112, 110), var1 = c(1, 1, 3, 1))
g <- import_DF(df)

test_that("auto recode a variable", {
  out <- autoRecode(g, var = "id", var_suffix = "_new", label_suffix = "(recoded)")
  expect_equal(namesGADS(out), c("id", "var1", "id_new"))
  expect_equal(out$dat$id_new, c(1, 3, 2, 1))
  expect_equal(out$labels[3, 2], "(recoded)", fixed = TRUE)
})

test_that("auto recode a variable while overwriting it", {
  out <- autoRecode(g, var = "id", var_suffix = "", label_suffix = "(recoded)")
  expect_equal(namesGADS(out), c("id", "var1"))
  expect_equal(out$dat$id, c(1, 3, 2, 1))
  expect_equal(out$labels[1, 2], "(recoded)", fixed = TRUE)
})


test_that("save lookup", {
  f <- tempfile(fileext = ".csv")
  out <- autoRecode(g, var = "id", var_suffix = "_new", csv_path = f)
  expect_equal(namesGADS(out), c("id", "var1", "id_new"))
  expect_equal(out$dat$id_new, c(1, 3, 2, 1))

  lookup <- read.csv(f)
  expect_equal(names(lookup), c("oldValue", "newValue"))
  expect_equal(lookup$oldValue, c(110, 112, 115))
  expect_equal(lookup$newValue, c(1, 2, 3))
})


test_that("existing lookup", {
  existing_lookup <- data.frame(oldValue = c(110, 111, 112, 113), newValue = 1:4)

  f <- tempfile(fileext = ".csv")
  expect_warning(out <- autoRecode(g, var = "id", var_suffix = "_new", label_suffix = "Test",
                                   template = existing_lookup, csv_path = f),
                 "For variable id_new the following values are in the lookup table but not in the data: 111, 113")

  expect_equal(namesGADS(out), c("id", "var1", "id_new"))
  expect_equal(out$dat$id_new, c(1, 5, 3, 1))
  expect_equal(out$labels[3, 2], "Test", fixed = TRUE)

  lookup <- read.csv(f)
  expect_equal(names(lookup), c("oldValue", "newValue"))
  expect_equal(lookup$oldValue, c(110, 111, 112, 113, 115))
  expect_equal(lookup$newValue, 1:5)
})

test_that("existing lookup, no new cases", {
  existing_lookup <- data.frame(oldValue = c(110:115), newValue = 1:6)

  f <- tempfile(fileext = ".csv")
  expect_warning(out <- autoRecode(g, var = "id", var_suffix = "_new", template = existing_lookup, csv_path = f),
                 "For variable id_new the following values are in the lookup table but not in the data: 111, 113, 114")

  expect_equal(namesGADS(out), c("id", "var1", "id_new"))
  expect_equal(out$dat$id_new, c(1, 6, 3, 1))

  lookup <- read.csv(f)
  expect_equal(names(lookup), c("oldValue", "newValue"))
  expect_equal(lookup$oldValue, 110:115)
  expect_equal(lookup$newValue, 1:6)
})

